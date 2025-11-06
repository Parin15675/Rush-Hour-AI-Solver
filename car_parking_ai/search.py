from __future__ import annotations

import heapq
from pathlib import Path
from typing import Callable, Dict, List, Optional, Set, Tuple, Union

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

    if Prolog is not None:
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

    provider = move_provider or _move_provider
    if provider is None:
        raise RuntimeError(
            "No move provider set for a_star. Supply move_provider or call set_move_provider."
        )
    return _python_a_star(start_state, provider)


def _python_a_star(
    start_state: StateTuple, provider: MoveProvider
) -> Tuple[List[MoveTuple], CostType, int]:

    frontier: List[Tuple[int, int, StateTuple, List[MoveTuple]]] = []
    heapq.heappush(frontier, (heuristic(start_state), 0, start_state, []))

    cost_so_far: Dict[StateTuple, int] = {start_state: 0}
    explored: Set[StateTuple] = set()
    nodes_expanded = 0

    while frontier:
        _, cost, state, path = heapq.heappop(frontier)
        if state in explored:
            continue
        explored.add(state)

        if is_goal(state):
            return path, cost, nodes_expanded

        nodes_expanded += 1

        for move in provider(state):
            car_id, direction, next_state = move
            new_cost = cost + 1
            if next_state not in cost_so_far or new_cost < cost_so_far[next_state]:
                cost_so_far[next_state] = new_cost
                priority = new_cost + heuristic(next_state)
                heapq.heappush(
                    frontier, (priority, new_cost, next_state, path + [move])
                )

    return [], float("inf"), nodes_expanded


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

    parsed: List[Tuple[str, int, int, str, int]] = []
    for car in state_term:
        identifier = str(car.args[0]).upper()
        x = int(car.args[1])
        y = int(car.args[2])
        orientation = str(car.args[3]).upper()
        length = int(car.args[4])
        parsed.append((identifier, x, y, orientation, length))
    parsed.sort()
    return tuple(parsed)


def _parse_prolog_path(path_term) -> List[MoveTuple]:

    moves: List[MoveTuple] = []
    for move in path_term:
        car_id = str(move.args[0]).upper()
        direction = str(move.args[1]).lower()
        next_state = _parse_prolog_state(move.args[2])
        moves.append((car_id, direction, next_state))
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
