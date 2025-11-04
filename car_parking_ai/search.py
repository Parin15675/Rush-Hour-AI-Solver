"""A* search implementation and supporting heuristics for Rush Hour."""

from __future__ import annotations

import heapq
from typing import Callable, Dict, List, Optional, Set, Tuple

StateTuple = Tuple[Tuple[str, int, int, str, int], ...]
MoveTuple = Tuple[str, str, StateTuple]
MoveProvider = Callable[[StateTuple], List[MoveTuple]]

GRID_SIZE = 6
RED_CAR_ID = "R"

_move_provider: Optional[MoveProvider] = None


def set_move_provider(provider: MoveProvider) -> None:
    """Register a callable that will generate successor states on demand."""
    global _move_provider
    _move_provider = provider


def heuristic(state: StateTuple) -> int:
    """Estimate remaining effort: distance to exit + blocking vehicles."""
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
    """Return True when the red car has fully exited the right edge."""
    red_car = _find_red_car(state)
    if not red_car:
        return False
    _, x, _, orientation, length = red_car
    return orientation == "H" and (x + length) >= GRID_SIZE


def get_valid_moves(state: StateTuple) -> List[MoveTuple]:
    """Delegate move generation to the registered provider."""
    if _move_provider is None:
        raise RuntimeError(
            "No move provider configured. Call set_move_provider before solving."
        )
    return _move_provider(state)


def a_star(
    start_state: StateTuple, move_provider: Optional[MoveProvider] = None
) -> Tuple[List[MoveTuple], int, int]:
    """Search for the optimal solution using the configured move provider."""
    provider = move_provider or _move_provider
    if provider is None:
        raise RuntimeError(
            "No move provider set for a_star. Supply move_provider or call set_move_provider."
        )

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
                heapq.heappush(frontier, (priority, new_cost, next_state, path + [move]))

    return [], float("inf"), nodes_expanded


def _find_red_car(state: StateTuple) -> Optional[Tuple[str, int, int, str, int]]:
    """Locate the red car tuple within the canonical state structure."""
    for car in state:
        if car[0].upper() == RED_CAR_ID:
            return car
    return None


def _count_blocking_cars(state: StateTuple, row: int, start_column: int) -> int:
    """Count how many cars obstruct the red car's path to the exit."""
    blocking: Set[str] = set()
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
