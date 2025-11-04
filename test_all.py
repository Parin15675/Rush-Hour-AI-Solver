"""Test all puzzle files in the puzzles directory."""

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent / "car_parking_ai"))

from game import Game
from search import a_star, set_move_provider


def test_puzzle(filename, puzzle_dir):
    """Test a single puzzle file."""
    filepath = puzzle_dir / filename

    if not filepath.exists():
        return None, None, "NOT FOUND"

    try:
        game = Game(
            asset_dir=puzzle_dir.parent / "assets",
            logic_path=puzzle_dir.parent / "logic.pl"
        )
        game.load_level(filepath)
        set_move_provider(game.get_valid_moves)

        path, cost, nodes = a_star(game.current_state, game.get_valid_moves)

        if not path:
            return -1, -1, "UNSOLVABLE"

        return cost, nodes, "OK"
    except Exception as e:
        return -1, -1, f"ERROR: {str(e)}"


def main():
    project_dir = Path(__file__).parent
    puzzle_dir = project_dir / "car_parking_ai" / "puzzles"

    puzzles_to_test = [
        "beginner_02.json",
        "beginner_03.json",
        "advanced_02.json",
        "advanced_03.json",
        "expert_01.json",
        "expert_02.json",
        "expert_03.json"
    ]

    print("=" * 80)
    print("TESTING ALL PUZZLE FILES")
    print("=" * 80)
    print()
    print(f"{'Puzzle':<25} {'Status':<15} {'Moves':<10} {'Nodes':<10}")
    print("-" * 80)

    results = []
    for filename in puzzles_to_test:
        moves, nodes, status = test_puzzle(filename, puzzle_dir)

        if status == "OK":
            print(f"{filename:<25} {status:<15} {moves:<10} {nodes:<10}")
        else:
            print(f"{filename:<25} {status:<15}")

        results.append((filename, status, moves, nodes))

    print()
    print("=" * 80)
    successful = sum(1 for _, status, _, _ in results if status == "OK")
    print(f"Successfully validated {successful}/{len(puzzles_to_test)} puzzles")
    print("=" * 80)


if __name__ == "__main__":
    main()
