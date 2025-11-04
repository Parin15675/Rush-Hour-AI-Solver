"""Manually create expert-level puzzles based on known hard patterns."""

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent / "car_parking_ai"))

from game import Game
from search import a_star, set_move_provider


def validate_and_solve(puzzle, puzzle_dir):
    """Validate and solve a puzzle."""
    # Save temp file
    temp_file = puzzle_dir / "temp_test.json"
    temp_file.write_text(json.dumps(puzzle, indent=2))

    try:
        game = Game(
            asset_dir=puzzle_dir / "assets",
            logic_path=puzzle_dir / "logic.pl"
        )
        game.load_level(temp_file)
        set_move_provider(game.get_valid_moves)

        path, cost, nodes = a_star(game.current_state, game.get_valid_moves)

        if not path:
            return -1, -1

        return cost, nodes
    finally:
        if temp_file.exists():
            temp_file.unlink()


# Expert puzzles - carefully designed to be solvable but challenging
expert_puzzles = {
    "expert_01.json": {
        # Red must move trucks blocking its path - cascading moves
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "D": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Blocks A from moving up
        "E": {"x": 0, "y": 3, "dir": "H", "len": 2},  # Blocks A from moving down
        "F": {"x": 3, "y": 3, "dir": "H", "len": 2},  # Blocks B/C from moving down
        "G": {"x": 0, "y": 4, "dir": "H", "len": 2},  # Blocks E
        "H": {"x": 3, "y": 4, "dir": "H", "len": 2},  # Blocks F
        "I": {"x": 0, "y": 5, "dir": "H", "len": 2},  # Bottom row
        "J": {"x": 3, "y": 5, "dir": "H", "len": 2}   # Bottom row
    },
    "expert_02.json": {
        # Different configuration with trucks and blocking cars
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 1, "dir": "V", "len": 3},  # Blocks row 2
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "C": {"x": 4, "y": 1, "dir": "V", "len": 3},  # Blocks row 2
        "D": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Top left
        "E": {"x": 0, "y": 1, "dir": "H", "len": 2},  # Blocks A from moving up
        "F": {"x": 0, "y": 4, "dir": "H", "len": 2},  # Bottom left
        "G": {"x": 3, "y": 4, "dir": "H", "len": 2},  # Bottom middle
        "H": {"x": 0, "y": 5, "dir": "H", "len": 2},  # Bottom row
        "I": {"x": 3, "y": 5, "dir": "H", "len": 2},  # Bottom row
        "J": {"x": 5, "y": 0, "dir": "V", "len": 2}   # Right side
    },
    "expert_03.json": {
        # Maximum complexity - four trucks blocking
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},  # Truck 1
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},  # Truck 2
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Truck 3
        "D": {"x": 5, "y": 0, "dir": "V", "len": 3},  # Truck 4
        "E": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Blocks A
        "F": {"x": 0, "y": 3, "dir": "H", "len": 2},  # Blocks A
        "G": {"x": 3, "y": 3, "dir": "H", "len": 2},  # Blocks B/C
        "H": {"x": 0, "y": 4, "dir": "H", "len": 2},  # Complexity
        "I": {"x": 3, "y": 4, "dir": "H", "len": 2},  # Complexity
        "J": {"x": 0, "y": 5, "dir": "H", "len": 2}   # Bottom
    }
}


def main():
    project_dir = Path(__file__).parent
    puzzle_dir = project_dir / "car_parking_ai" / "puzzles"

    print("=" * 70)
    print("Creating Expert Rush Hour Puzzles")
    print("=" * 70)
    print()

    for filename, puzzle in expert_puzzles.items():
        print(f"Testing {filename}...")

        moves, nodes = validate_and_solve(puzzle, project_dir / "car_parking_ai")

        if moves == -1:
            print(f"  ERROR: Unsolvable puzzle")
            continue

        # Save
        filepath = puzzle_dir / filename
        filepath.write_text(json.dumps(puzzle, indent=2))

        print(f"  OK: {moves} moves, {nodes} nodes explored")

    print()
    print("=" * 70)
    print("Expert puzzle creation complete!")
    print("=" * 70)


if __name__ == "__main__":
    main()
