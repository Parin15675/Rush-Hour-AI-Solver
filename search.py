from __future__ import annotations

import ast
import re
from pathlib import Path
from typing import Iterable, List, Optional, Tuple, Union

try:
    from pyswip import Prolog  # type: ignore[import]
except ImportError:
    Prolog = None  # type: ignore[assignment]

StateTuple = Tuple[Tuple[str, int, int, str, int], ...]
MoveTuple = Tuple[str, str, StateTuple]
CostType = Union[int, float]

_GRID_WIDTH = 6
_GRID_HEIGHT = 6
_EXIT_ROW = 2
_EXIT_ROWS: Tuple[int, ...] = (_EXIT_ROW,)
_EXIT_ANY_ROW = False
_EXIT_COL = 6
_POLES: Tuple[Tuple[int, int], ...] = ()

_prolog_solver: Optional[Prolog] = None
_LOGIC_PATH = Path(__file__).with_name("logic.pl")

def configure_board(
    width: int,
    height: int,
    exit_row: int,
    exit_col: int,
    poles: Optional[Iterable[Tuple[int, int]]] = None,
    exit_rows: Optional[Iterable[int]] = None,
    allow_any_row: bool = False,
) -> None:

    global _GRID_WIDTH, _GRID_HEIGHT, _EXIT_ROW, _EXIT_COL, _POLES, _EXIT_ROWS, _EXIT_ANY_ROW
    _GRID_WIDTH = max(1, int(width))
    _GRID_HEIGHT = max(1, int(height))
    _EXIT_ROW = max(0, min(int(exit_row), _GRID_HEIGHT - 1))
    _EXIT_COL = max(1, int(exit_col))
    _EXIT_ANY_ROW = bool(allow_any_row)
    _EXIT_ROWS = _sanitize_exit_rows(exit_rows, _GRID_HEIGHT, _EXIT_ROW)
    _POLES = _normalize_poles(poles)
    _update_solver_board_facts()


def _normalize_heading(value: str) -> str:

    heading = value.upper()
    if heading == "H":
        return "E"
    if heading == "V":
        return "N"
    if heading not in {"N", "E", "S", "W"}:
        return "E"
    return heading


def heuristic(state: StateTuple) -> int:

    solver = _ensure_prolog()
    prolog_state = _state_to_prolog(state)
    query = f"logic:heuristic({prolog_state}, H)"
    result = next(solver.query(query), None)
    if result is None:
        return 0
    return int(result["H"])


def is_goal(state: StateTuple) -> bool:

    solver = _ensure_prolog()
    prolog_state = _state_to_prolog(state)
    query = f"logic:goal_state({prolog_state})"
    return next(solver.query(query), None) is not None


def a_star(start_state: StateTuple) -> Tuple[List[MoveTuple], CostType, int]:

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
        _update_solver_board_facts()
    return _prolog_solver


def _update_solver_board_facts() -> None:

    if _prolog_solver is None:
        return
    try:
        pole_terms = ", ".join(f"pole({x},{y})" for x, y in _POLES)
        exit_spec = _format_exit_spec()
        command = "logic:configure_board({},{},{},{},[{}])".format(
            _GRID_WIDTH, _GRID_HEIGHT, exit_spec, _EXIT_COL, pole_terms
        )
        list(_prolog_solver.query(command))
    except Exception:
        pass


def _sanitize_exit_rows(
    rows: Optional[Iterable[int]], max_height: int, fallback: int
) -> Tuple[int, ...]:

    if not rows:
        return (fallback,)
    sanitized = set()
    for value in rows:
        try:
            row = int(value)
        except (TypeError, ValueError):
            continue
        if max_height <= 0:
            continue
        bounded = max(0, min(row, max_height - 1))
        sanitized.add(bounded)
    if not sanitized:
        return (fallback,)
    return tuple(sorted(sanitized))


def _format_exit_spec() -> str:

    if _EXIT_ANY_ROW:
        return "any"
    rows = _EXIT_ROWS or (_EXIT_ROW,)
    if len(rows) == 1:
        return str(rows[0])
    return "[" + ",".join(str(row) for row in rows) + "]"


def _normalize_poles(
    poles: Optional[Iterable[Tuple[int, int]]]
) -> Tuple[Tuple[int, int], ...]:

    if not poles:
        return ()
    valid = set()
    for entry in poles:
        try:
            px = int(entry[0])
            py = int(entry[1])
        except (TypeError, ValueError, IndexError):
            continue
        if 0 <= px < _GRID_WIDTH and 0 <= py < _GRID_HEIGHT:
            valid.add((px, py))
    return tuple(sorted(valid))


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
    orientation = _normalize_heading(match.group(4).strip())
    length = int(match.group(5))
    return identifier, x, y, orientation, length

