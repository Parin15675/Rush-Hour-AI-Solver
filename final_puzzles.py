"""Create final expert puzzles that are guaranteed solvable."""

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent / "car_parking_ai"))

from game import Game
from search import a_star, set_move_provider


def validate_and_solve(puzzle, puzzle_dir):
    """Validate and solve a puzzle."""
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


# Create expert puzzles by starting simple and verifying solvability
expert_puzzles = {
    "expert_01.json": {
        # Start with 3 trucks, careful blocking
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},
        "D": {"x": 0, "y": 0, "dir": "H", "len": 2},
        "E": {"x": 0, "y": 3, "dir": "H", "len": 2},
        "F": {"x": 3, "y": 3, "dir": "H", "len": 2},
        "G": {"x": 0, "y": 4, "dir": "H", "len": 2},
        "H": {"x": 3, "y": 4, "dir": "H", "len": 2},
        "I": {"x": 0, "y": 5, "dir": "H", "len": 3}
    },
    "expert_02.json": {
        # Different truck pattern
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 1, "dir": "V", "len": 3},
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},
        "C": {"x": 4, "y": 1, "dir": "V", "len": 3},
        "D": {"x": 0, "y": 0, "dir": "H", "len": 2},
        "E": {"x": 0, "y": 1, "dir": "H", "len": 2},
        "F": {"x": 0, "y": 4, "dir": "H", "len": 2},
        "G": {"x": 3, "y": 4, "dir": "H", "len": 2},
        "H": {"x": 0, "y": 5, "dir": "H", "len": 3},
        "I": {"x": 5, "y": 0, "dir": "V", "len": 2}
    },
    "expert_03.json": {
        # Four trucks - maximum blocking
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},
        "D": {"x": 5, "y": 0, "dir": "V", "len": 3},
        "E": {"x": 0, "y": 0, "dir": "H", "len": 2},
        "F": {"x": 0, "y": 3, "dir": "H", "len": 2},
        "G": {"x": 3, "y": 3, "dir": "H", "len": 2},
        "H": {"x": 0, "y": 4, "dir": "H", "len": 2},
        "I": {"x": 3, "y": 4, "dir": "H", "len": 2}
    }
}


def main():
    project_dir = Path(__file__).parent
    puzzle_dir = project_dir / "car_parking_ai" / "puzzles"

    print("=" * 70)
    print("Creating Final Expert Rush Hour Puzzles")
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
    print("Final expert puzzle creation complete!")
    print("=" * 70)


if __name__ == "__main__":
    main()
