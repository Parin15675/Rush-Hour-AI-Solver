from __future__ import annotations

import json
import time
from pathlib import Path
from typing import Dict, List, Tuple

from game import Game
from search import a_star, set_move_provider
from puzzle_generator import PuzzleValidator


def test_all_puzzles():

    base_dir = Path(__file__).resolve().parent
    puzzle_dir = base_dir / "puzzles"
    asset_dir = base_dir / "assets"
    logic_file = base_dir / "logic.pl"

    validator = PuzzleValidator()

    difficulty_groups = {
        "Beginner": [],
        "Intermediate": [],
        "Advanced": [],
        "Expert": [],
    }

    for puzzle_file in sorted(puzzle_dir.glob("*.json")):
        name = puzzle_file.stem
        if name.startswith("beginner"):
            difficulty_groups["Beginner"].append(puzzle_file)
        elif name.startswith("intermediate"):
            difficulty_groups["Intermediate"].append(puzzle_file)
        elif name.startswith("advanced"):
            difficulty_groups["Advanced"].append(puzzle_file)
        elif name.startswith("expert"):
            difficulty_groups["Expert"].append(puzzle_file)

    print("=" * 70)
    print("RUSH HOUR PUZZLE TEST SUITE")
    print("=" * 70)

    all_results: List[Tuple[str, str, bool, int, int, float]] = []

    for difficulty, files in difficulty_groups.items():
        if not files:
            continue

        print(f"\n{difficulty.upper()} LEVEL")
        print("-" * 70)

        for puzzle_file in files:

            puzzle = json.loads(puzzle_file.read_text())
            name = puzzle_file.name

            is_valid, error = validator.validate_puzzle(puzzle)
            if not is_valid:
                print(f"  {name:25s} INVALID: {error}")
                all_results.append((difficulty, name, False, 0, 0, 0.0))
                continue

            game = Game(asset_dir=asset_dir, logic_path=logic_file)
            game.load_level(puzzle_file)
            set_move_provider(game.get_valid_moves)

            start = time.perf_counter()
            path, cost, nodes = a_star(game.current_state, game.get_valid_moves)
            elapsed = time.perf_counter() - start

            if not path:
                print(f"  {name:25s} UNSOLVABLE")
                all_results.append((difficulty, name, False, 0, 0, 0.0))
                continue

            print(
                f"  {name:25s} OK: {cost:3d} moves, {nodes:6d} nodes, {elapsed:6.3f}s"
            )
            all_results.append((difficulty, name, True, cost, nodes, elapsed))

    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)

    for difficulty in ["Beginner", "Intermediate", "Advanced", "Expert"]:
        results = [r for r in all_results if r[0] == difficulty and r[2]]
        if not results:
            continue

        moves = [r[3] for r in results]
        nodes_list = [r[4] for r in results]
        times = [r[5] for r in results]

        print(f"\n{difficulty}:")
        print(f"  Puzzles:       {len(results)}")
        print(
            f"  Moves:         min={min(moves):2d}, max={max(moves):2d}, avg={sum(moves)/len(moves):5.1f}"
        )
        print(
            f"  Nodes:         min={min(nodes_list):6d}, max={max(nodes_list):6d}, avg={sum(nodes_list)/len(nodes_list):8.1f}"
        )
        print(
            f"  Solve time:    min={min(times):.3f}s, max={max(times):.3f}s, avg={sum(times)/len(times):.3f}s"
        )

    print("\n" + "=" * 70)


if __name__ == "__main__":
    test_all_puzzles()
