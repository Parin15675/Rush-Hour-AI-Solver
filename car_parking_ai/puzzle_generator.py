from __future__ import annotations

import json
import random
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

from game import Game
from search import a_star, set_move_provider

GRID_SIZE = 6
RED_CAR_ROW = 2


class PuzzleValidator:

    @staticmethod
    def validate_puzzle(puzzle: Dict) -> Tuple[bool, Optional[str]]:

        if "R" not in puzzle:
            return False, "Missing red car 'R'"

        red_car = puzzle["R"]
        if red_car["dir"] != "H":
            return False, "Red car must be horizontal"
        if red_car["len"] != 2:
            return False, "Red car must have length 2"
        if red_car["y"] != RED_CAR_ROW:
            return False, f"Red car should be on row {RED_CAR_ROW}"

        occupied: Dict[Tuple[int, int], str] = {}

        for car_id, attrs in puzzle.items():
            x, y = attrs["x"], attrs["y"]
            direction = attrs["dir"]
            length = attrs["len"]

            if length not in [2, 3]:
                return (
                    False,
                    f"Car {car_id} has invalid length {length} (must be 2 or 3)",
                )

            if direction not in ["H", "V"]:
                return False, f"Car {car_id} has invalid direction {direction}"

            cells = []
            if direction == "H":
                if x + length > GRID_SIZE:
                    return False, f"Car {car_id} extends beyond right edge"
                cells = [(x + i, y) for i in range(length)]
            else:
                if y + length > GRID_SIZE:
                    return False, f"Car {car_id} extends beyond bottom edge"
                cells = [(x, y + i) for i in range(length)]

            for cell in cells:
                if cell in occupied:
                    return (
                        False,
                        f"Cars {car_id} and {occupied[cell]} overlap at {cell}",
                    )
                occupied[cell] = car_id

        return True, None

    @staticmethod
    def solve_and_rate(puzzle: Dict, puzzle_dir: Path) -> Tuple[int, int, float]:

        import time

        temp_file = puzzle_dir / "temp_puzzle.json"
        temp_file.write_text(json.dumps(puzzle, indent=2))

        game = Game(
            asset_dir=puzzle_dir.parent / "assets",
            logic_path=puzzle_dir.parent / "logic.pl",
        )
        game.load_level(temp_file)
        set_move_provider(game.get_valid_moves)

        start = time.perf_counter()
        path, cost, nodes = a_star(game.current_state, game.get_valid_moves)
        elapsed = time.perf_counter() - start

        temp_file.unlink()

        if not path:
            return -1, -1, -1.0

        return cost, nodes, elapsed


class PuzzleGenerator:

    def __init__(self, seed: Optional[int] = None):
        if seed is not None:
            random.seed(seed)

    def generate_beginner(self) -> Dict:

        puzzle = {"R": {"x": 3, "y": RED_CAR_ROW, "dir": "H", "len": 2}}

        num_vehicles = random.randint(3, 5)
        occupied = self._get_occupied(puzzle)

        for i in range(num_vehicles):
            car_id = chr(ord("A") + i)
            vehicle = self._place_random_vehicle(occupied, avoid_blocking_red=True)
            if vehicle:
                puzzle[car_id] = vehicle

        return puzzle

    def generate_intermediate(self) -> Dict:

        puzzle = {"R": {"x": 1, "y": RED_CAR_ROW, "dir": "H", "len": 2}}

        occupied = self._get_occupied(puzzle)

        num_blockers = random.randint(1, 2)
        blocker_columns = random.sample(range(3, 6), num_blockers)

        for idx, col in enumerate(blocker_columns):
            car_id = chr(ord("A") + idx)

            length = random.choice([2, 3])
            max_y = GRID_SIZE - length

            y_options = [y for y in range(max_y + 1) if y <= RED_CAR_ROW < y + length]
            if y_options:
                y = random.choice(y_options)
                vehicle = {"x": col, "y": y, "dir": "V", "len": length}
                if self._can_place(vehicle, occupied):
                    puzzle[car_id] = vehicle
                    occupied.update(self._get_vehicle_cells(vehicle))

        current_idx = len(puzzle) - 1
        for i in range(6):
            car_id = chr(ord("A") + current_idx + i)
            vehicle = self._place_random_vehicle(occupied)
            if vehicle:
                puzzle[car_id] = vehicle

        return puzzle

    def generate_advanced(self) -> Dict:

        puzzle = {"R": {"x": 0, "y": RED_CAR_ROW, "dir": "H", "len": 2}}

        occupied = self._get_occupied(puzzle)

        for idx, col in enumerate([3, 4, 5]):
            car_id = chr(ord("A") + idx)
            length = random.choice([2, 3])
            max_y = GRID_SIZE - length
            y_options = [y for y in range(max_y + 1) if y <= RED_CAR_ROW < y + length]
            if y_options:
                y = random.choice(y_options)
                vehicle = {"x": col, "y": y, "dir": "V", "len": length}
                if self._can_place(vehicle, occupied):
                    puzzle[car_id] = vehicle
                    occupied.update(self._get_vehicle_cells(vehicle))

        current_idx = len(puzzle) - 1
        for i in range(4):
            car_id = chr(ord("A") + current_idx + i)

            row = random.choice([r for r in range(GRID_SIZE) if r != RED_CAR_ROW])
            length = random.choice([2, 3])
            max_x = GRID_SIZE - length
            x = random.randint(0, max_x)
            vehicle = {"x": x, "y": row, "dir": "H", "len": length}
            if self._can_place(vehicle, occupied):
                puzzle[car_id] = vehicle
                occupied.update(self._get_vehicle_cells(vehicle))

        current_idx = len(puzzle) - 1
        for i in range(5):
            car_id = chr(ord("A") + current_idx + i)
            vehicle = self._place_random_vehicle(occupied)
            if vehicle:
                puzzle[car_id] = vehicle

        return puzzle

    def generate_expert(self) -> Dict:

        puzzle = {"R": {"x": 0, "y": RED_CAR_ROW, "dir": "H", "len": 2}}

        occupied = self._get_occupied(puzzle)

        for idx, col in enumerate([2, 3, 4]):
            car_id = chr(ord("A") + idx)
            vehicle = {"x": col, "y": 0, "dir": "V", "len": 3}
            if self._can_place(vehicle, occupied):
                puzzle[car_id] = vehicle
                occupied.update(self._get_vehicle_cells(vehicle))

        for idx, col in enumerate([5]):
            car_id = chr(ord("A") + len(puzzle) - 1 + idx)
            length = 2
            y = random.choice([0, 1, 4])
            vehicle = {"x": col, "y": y, "dir": "V", "len": length}
            if self._can_place(vehicle, occupied):
                puzzle[car_id] = vehicle
                occupied.update(self._get_vehicle_cells(vehicle))

        current_idx = len(puzzle) - 1
        for i in range(8):
            car_id = chr(ord("A") + current_idx + i)
            row = random.choice([r for r in range(GRID_SIZE) if r != RED_CAR_ROW])
            length = random.choice([2, 3])
            max_x = GRID_SIZE - length
            if max_x >= 0:
                x = random.randint(0, max_x)
                vehicle = {"x": x, "y": row, "dir": "H", "len": length}
                if self._can_place(vehicle, occupied):
                    puzzle[car_id] = vehicle
                    occupied.update(self._get_vehicle_cells(vehicle))

        return puzzle

    def _get_occupied(self, puzzle: Dict) -> Set[Tuple[int, int]]:

        occupied = set()
        for vehicle in puzzle.values():
            occupied.update(self._get_vehicle_cells(vehicle))
        return occupied

    def _get_vehicle_cells(self, vehicle: Dict) -> Set[Tuple[int, int]]:

        cells = set()
        x, y = vehicle["x"], vehicle["y"]
        length = vehicle["len"]
        if vehicle["dir"] == "H":
            cells = {(x + i, y) for i in range(length)}
        else:
            cells = {(x, y + i) for i in range(length)}
        return cells

    def _can_place(self, vehicle: Dict, occupied: Set[Tuple[int, int]]) -> bool:

        cells = self._get_vehicle_cells(vehicle)

        for x, y in cells:
            if not (0 <= x < GRID_SIZE and 0 <= y < GRID_SIZE):
                return False

        return not cells.intersection(occupied)

    def _place_random_vehicle(
        self, occupied: Set[Tuple[int, int]], avoid_blocking_red: bool = False
    ) -> Optional[Dict]:

        for _ in range(100):
            direction = random.choice(["H", "V"])
            length = random.choice([2, 3])

            if direction == "H":
                max_x = GRID_SIZE - length
                x = random.randint(0, max_x)
                y = random.randint(0, GRID_SIZE - 1)

                if avoid_blocking_red and y == RED_CAR_ROW:
                    continue
            else:
                x = random.randint(0, GRID_SIZE - 1)
                max_y = GRID_SIZE - length
                y = random.randint(0, max_y)

                if avoid_blocking_red and x > 2 and y <= RED_CAR_ROW < y + length:
                    continue

            vehicle = {"x": x, "y": y, "dir": direction, "len": length}
            if self._can_place(vehicle, occupied):
                occupied.update(self._get_vehicle_cells(vehicle))
                return vehicle

        return None


def main():

    base_dir = Path(__file__).resolve().parent
    puzzle_dir = base_dir / "puzzles"
    puzzle_dir.mkdir(exist_ok=True)

    generator = PuzzleGenerator(seed=42)
    validator = PuzzleValidator()

    difficulty_configs = {
        "beginner": (3, lambda: generator.generate_beginner(), 10),
        "intermediate": (3, lambda: generator.generate_intermediate(), 20),
        "advanced": (3, lambda: generator.generate_advanced(), 35),
        "expert": (3, lambda: generator.generate_expert(), 50),
    }

    print("=" * 60)
    print("Rush Hour Puzzle Generator")
    print("=" * 60)

    for difficulty, (count, gen_func, max_moves) in difficulty_configs.items():
        print(f"\n{difficulty.upper()} puzzles:")
        print("-" * 60)

        generated_count = 0
        attempt = 0

        while generated_count < count and attempt < count * 20:
            attempt += 1
            puzzle = gen_func()

            is_valid, error = validator.validate_puzzle(puzzle)
            if not is_valid:
                print(f"  Attempt {attempt}: Invalid - {error}")
                continue

            moves, nodes, solve_time = validator.solve_and_rate(puzzle, puzzle_dir)

            if moves == -1:
                print(f"  Attempt {attempt}: Unsolvable")
                continue

            if moves > max_moves:
                print(f"  Attempt {attempt}: Too hard ({moves} moves > {max_moves})")
                continue

            if difficulty == "beginner" and moves > 10:
                continue
            elif difficulty == "intermediate" and not (10 < moves <= 20):
                continue
            elif difficulty == "advanced" and not (20 < moves <= 35):
                continue
            elif difficulty == "expert" and moves < 30:
                continue

            generated_count += 1
            filename = f"{difficulty}_{generated_count:02d}.json"
            filepath = puzzle_dir / filename
            filepath.write_text(json.dumps(puzzle, indent=2))

            print(f"  OK {filename}: {moves} moves, {nodes} nodes, {solve_time:.3f}s")

    print("\n" + "=" * 60)
    print("Puzzle generation complete!")
    print("=" * 60)


if __name__ == "__main__":
    main()
