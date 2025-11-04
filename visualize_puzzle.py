"""Visualize puzzle layout."""

import json
from pathlib import Path

def visualize(puzzle_name):
    puzzle_file = Path(__file__).parent / "car_parking_ai" / "puzzles" / puzzle_name
    puzzle = json.loads(puzzle_file.read_text())

    # Create 6x6 grid
    grid = [['.' for _ in range(6)] for _ in range(6)]

    # Place vehicles
    for car_id, attrs in puzzle.items():
        x, y, direction, length = attrs["x"], attrs["y"], attrs["dir"], attrs["len"]

        if direction == "H":
            for i in range(length):
                if 0 <= x + i < 6 and 0 <= y < 6:
                    grid[y][x + i] = car_id[0]
        else:
            for i in range(length):
                if 0 <= x < 6 and 0 <= y + i < 6:
                    grid[y + i][x] = car_id[0]

    print(f"\n{puzzle_name}:")
    print("  0 1 2 3 4 5")
    for i, row in enumerate(grid):
        print(f"{i} {' '.join(row)}")
    print()

# Visualize working and non-working puzzles
visualize("advanced_03.json")
visualize("expert_01.json")
visualize("expert_02.json")
visualize("expert_03.json")
