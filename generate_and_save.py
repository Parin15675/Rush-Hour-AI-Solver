"""Use the existing puzzle generator to create valid puzzles."""

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent / "car_parking_ai"))

from puzzle_generator import PuzzleGenerator, PuzzleValidator

def main():
    base_dir = Path(__file__).parent / "car_parking_ai"
    puzzle_dir = base_dir / "puzzles"

    generator = PuzzleGenerator(seed=None)  # Random seed for variety
    validator = PuzzleValidator()

    puzzles_to_create = {
        "advanced_03.json": ("advanced", 20, 35),
        "expert_01.json": ("expert", 30, 100),
        "expert_02.json": ("expert", 30, 100),
        "expert_03.json": ("expert", 30, 100),
    }

    print("=" * 70)
    print("Generating Remaining Puzzles")
    print("=" * 70)

    for filename, (difficulty, min_moves, max_moves) in puzzles_to_create.items():
        print(f"\nGenerating {filename} ({difficulty}, {min_moves}-{max_moves} moves)...")

        attempts = 0
        max_attempts = 200

        while attempts < max_attempts:
            attempts += 1

            if difficulty == "advanced":
                puzzle = generator.generate_advanced()
            else:
                puzzle = generator.generate_expert()

            # Validate
            is_valid, error = validator.validate_puzzle(puzzle)
            if not is_valid:
                continue

            # Solve
            moves, nodes, elapsed = validator.solve_and_rate(puzzle, puzzle_dir)

            if moves == -1:
                continue

            # Check if it meets criteria
            if difficulty == "advanced" and not (20 <= moves <= 35):
                if attempts % 20 == 0:
                    print(f"  Attempt {attempts}: {moves} moves (need 20-35)")
                continue
            elif difficulty == "expert" and moves < 30:
                if attempts % 20 == 0:
                    print(f"  Attempt {attempts}: {moves} moves (need 30+)")
                continue

            # Save puzzle
            filepath = puzzle_dir / filename
            filepath.write_text(json.dumps(puzzle, indent=2))

            print(f"  SUCCESS: {moves} moves, {nodes} nodes, {elapsed:.3f}s (attempt {attempts})")
            break
        else:
            print(f"  FAILED: Could not generate valid puzzle after {max_attempts} attempts")

    print("\n" + "=" * 70)
    print("Generation complete!")
    print("=" * 70)

if __name__ == "__main__":
    main()
