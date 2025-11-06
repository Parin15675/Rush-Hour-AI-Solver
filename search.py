from __future__ import annotations

import ast
import re
from pathlib import Path
from typing import Callable, List, Optional, Tuple, Union

try:
    from pyswip import Prolog  # type: ignore[import]
except ImportError:
    Prolog = None  # type: ignore[assignment]

StateTuple = Tuple[Tuple[str, int, int, str, int], ...]
MoveTuple = Tuple[str, str, StateTuple]
MoveProvider = Callable[[StateTuple], List[MoveTuple]]
CostType = Union[int, float]

GRID_SIZE = 6
RED_CAR_ID = "R"

_move_provider: Optional[MoveProvider] = None
_prolog_solver: Optional[Prolog] = None
_LOGIC_PATH = Path(__file__).with_name("logic.pl")


def set_move_provider(provider: MoveProvider) -> None:

    global _move_provider
    _move_provider = provider


def heuristic(state: StateTuple) -> int:

    red_car = _find_red_car(state)
    if not red_car:
        return 0
    _, x, y, orientation, length = red_car
    if orientation != "H":
        return GRID_SIZE * GRID_SIZE
    distance_to_exit = GRID_SIZE - (x + length)
    blocking = _count_blocking_cars(state, y, x + length)
    return distance_to_exit + blocking


def is_goal(state: StateTuple) -> bool:

    red_car = _find_red_car(state)
    if not red_car:
        return False
    _, x, _, orientation, length = red_car
    return orientation == "H" and (x + length) >= GRID_SIZE


def get_valid_moves(state: StateTuple) -> List[MoveTuple]:

    if _move_provider is None:
        raise RuntimeError(
            "No move provider configured. Call set_move_provider before solving."
        )
    return _move_provider(state)


def a_star(
    start_state: StateTuple, move_provider: Optional[MoveProvider] = None
) -> Tuple[List[MoveTuple], CostType, int]:

    solver = _ensure_prolog()
    prolog_state = _state_to_prolog(start_state)
    query = f"prolog_a_star({prolog_state}, Path, Cost, Nodes)"
    solution = next(solver.query(query), None)
    if solution is None:
        return [], float("inf"), 0
    path = _parse_prolog_path(solution["Path"])
    cost = _normalize_cost(solution["Cost"])
    nodes = int(solution["Nodes"])
    return path, cost, nodes


def _ensure_prolog() -> Prolog:

    if Prolog is None:
        raise RuntimeError(
            "pyswip is not installed; Prolog A* solver is unavailable."
        )

    global _prolog_solver
    if _prolog_solver is None:
        solver = Prolog()
        solver.consult(str(_LOGIC_PATH))
        _prolog_solver = solver
    return _prolog_solver


def _state_to_prolog(state: StateTuple) -> str:

    parts = []
    for identifier, x, y, orientation, length in state:
        parts.append(
            f"car({identifier.lower()},{x},{y},{orientation.lower()},{length})"
        )
    return f"[{', '.join(parts)}]"


def _parse_prolog_state(state_term) -> StateTuple:

    if isinstance(state_term, str):
        try:
            entries = ast.literal_eval(state_term)
        except (SyntaxError, ValueError) as exc:
            raise ValueError(f"Unexpected state term: {state_term!r}") from exc
        return tuple(sorted(_parse_car_atom(entry) for entry in entries))

    parsed: List[Tuple[str, int, int, str, int]] = []
    for car in state_term:
        if hasattr(car, "args"):
            identifier = str(car.args[0]).upper()
            x = int(car.args[1])
            y = int(car.args[2])
            orientation = str(car.args[3]).upper()
            length = int(car.args[4])
            parsed.append((identifier, x, y, orientation, length))
        else:
            parsed.append(_parse_car_atom(str(car)))
    parsed.sort()
    return tuple(parsed)


def _parse_prolog_path(path_term) -> List[MoveTuple]:

    moves: List[MoveTuple] = []
    for move in path_term:
        if hasattr(move, "args"):
            car_id = str(move.args[0]).upper()
            direction = str(move.args[1]).lower()
            if len(move.args) == 4:
                next_state_term = move.args[3]
            else:
                next_state_term = move.args[2]
            next_state = _parse_prolog_state(next_state_term)
            moves.append((car_id, direction, next_state))
        else:
            moves.append(_parse_move_atom(str(move)))
    return moves


def _normalize_cost(value) -> CostType:

    text = str(value)
    if text == "inf":
        return float("inf")
    return int(text)


def _find_red_car(state: StateTuple) -> Optional[Tuple[str, int, int, str, int]]:

    for car in state:
        if car[0].upper() == RED_CAR_ID:
            return car
    return None


def _count_blocking_cars(state: StateTuple, row: int, start_column: int) -> int:

    blocking = set()
    for identifier, x, y, orientation, length in state:
        if identifier.upper() == RED_CAR_ID:
            continue
        if orientation == "H":
            occupied = ((x + offset, y) for offset in range(length))
        else:
            occupied = ((x, y + offset) for offset in range(length))
        for cell_x, cell_y in occupied:
            if cell_y == row and cell_x >= start_column:
                blocking.add(identifier)
                break
    return len(blocking)


_MOVE_PATTERN = re.compile(
    r"""move\(\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*(\[[^\]]*\])\s*\)\s*$""",
    re.IGNORECASE,
)
_MOVE_PATTERN_LEGACY = re.compile(
    r"""move\(\s*([^,]+)\s*,\s*([^,]+)\s*,\s*(\[[^\]]*\])\s*\)\s*$""",
    re.IGNORECASE,
)
_CAR_PATTERN = re.compile(
    r"""car\(\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*$""",
    re.IGNORECASE,
)


def _parse_move_atom(text: str) -> MoveTuple:

    match = _MOVE_PATTERN.match(text)
    if match:
        car_id = match.group(1).strip().upper()
        direction = match.group(2).strip().lower()
        state_literal = match.group(4)
    else:
        legacy = _MOVE_PATTERN_LEGACY.match(text)
        if not legacy:
            raise ValueError(f"Unexpected move term: {text!r}")
        car_id = legacy.group(1).strip().upper()
        direction = legacy.group(2).strip().lower()
        state_literal = legacy.group(3)
    try:
        state_entries = ast.literal_eval(state_literal)
    except (SyntaxError, ValueError) as exc:
        raise ValueError(f"Invalid state literal in move term: {text!r}") from exc
    next_state = tuple(sorted(_parse_car_atom(entry) for entry in state_entries))
    return car_id, direction, next_state


def _parse_car_atom(text: str) -> Tuple[str, int, int, str, int]:

    match = _CAR_PATTERN.match(text.strip())
    if not match:
        raise ValueError(f"Unexpected car term: {text!r}")
    identifier = match.group(1).strip().upper()
    x = int(match.group(2))
    y = int(match.group(3))
    orientation = match.group(4).strip().upper()
    length = int(match.group(5))
    return identifier, x, y, orientation, length
