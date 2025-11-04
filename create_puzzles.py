"""Create valid Rush Hour puzzle files for all difficulty levels."""

import json
import sys
from pathlib import Path

# Add car_parking_ai to path
sys.path.insert(0, str(Path(__file__).parent / "car_parking_ai"))

from game import Game
from search import a_star, set_move_provider


def validate_puzzle(puzzle):
    """Validate puzzle follows all rules."""
    # Check red car
    if "R" not in puzzle:
        return False, "Missing red car"

    red = puzzle["R"]
    if red["dir"] != "H" or red["len"] != 2 or red["y"] != 2:
        return False, f"Invalid red car: {red}"

    # Build occupancy map
    occupied = {}
    for car_id, attrs in puzzle.items():
        x, y = attrs["x"], attrs["y"]
        length = attrs["len"]
        direction = attrs["dir"]

        # Check valid length
        if length not in [2, 3]:
            return False, f"Car {car_id} has invalid length {length}"

        # Check valid direction
        if direction not in ["H", "V"]:
            return False, f"Car {car_id} has invalid direction {direction}"

        # Get cells
        cells = []
        if direction == "H":
            if x + length > 6:
                return False, f"Car {car_id} extends beyond board"
            cells = [(x + i, y) for i in range(length)]
        else:
            if y + length > 6:
                return False, f"Car {car_id} extends beyond board"
            cells = [(x, y + i) for i in range(length)]

        # Check overlaps
        for cell in cells:
            if cell in occupied:
                return False, f"Cars {car_id} and {occupied[cell]} overlap at {cell}"
            occupied[cell] = car_id

    return True, None


def solve_puzzle(puzzle, puzzle_dir):
    """Solve puzzle and return move count."""
    # Save temp file
    temp_file = puzzle_dir / "temp_test.json"
    temp_file.write_text(json.dumps(puzzle, indent=2))

    try:
        # Load and solve
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


# Define all puzzles
puzzles = {
    "beginner_02.json": {
        # Red car at x=3, needs to move 1 right
        # One blocker at exit that needs to move
        "R": {"x": 3, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 5, "y": 0, "dir": "V", "len": 3},  # Blocks exit
        "B": {"x": 1, "y": 0, "dir": "H", "len": 2},
        "C": {"x": 0, "y": 4, "dir": "H", "len": 3},
        "D": {"x": 4, "y": 4, "dir": "V", "len": 2}
    },
    "beginner_03.json": {
        # Red car at x=2, two blockers need to move
        "R": {"x": 2, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Blocks exit
        "B": {"x": 5, "y": 3, "dir": "V", "len": 3},  # Blocks A from moving down
        "C": {"x": 0, "y": 0, "dir": "H", "len": 2},
        "D": {"x": 1, "y": 4, "dir": "H", "len": 2}
    },
    "advanced_02.json": {
        # Red car at x=0, multiple layers of blocking
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},  # First blocker on row 2
        "B": {"x": 3, "y": 1, "dir": "V", "len": 3},  # Second blocker on row 2
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Third blocker on row 2
        "D": {"x": 5, "y": 1, "dir": "V", "len": 3},  # Fourth blocker on row 2
        "E": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Blocks A from moving up
        "F": {"x": 0, "y": 3, "dir": "H", "len": 2},  # Blocks A/B from moving down
        "G": {"x": 3, "y": 4, "dir": "H", "len": 2},  # Blocks B from moving down
        "H": {"x": 0, "y": 5, "dir": "H", "len": 2}
    },
    "advanced_03.json": {
        # Red car at x=1, simpler but still challenging
        "R": {"x": 1, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 3, "y": 1, "dir": "V", "len": 3},  # Blocks row 2
        "B": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "C": {"x": 5, "y": 1, "dir": "V", "len": 2},  # Blocks B from moving right
        "D": {"x": 0, "y": 0, "dir": "V", "len": 2},  # Left side
        "E": {"x": 1, "y": 0, "dir": "H", "len": 2},  # Top
        "F": {"x": 0, "y": 3, "dir": "H", "len": 3},  # Blocks A from moving down
        "G": {"x": 0, "y": 5, "dir": "H", "len": 3},
        "H": {"x": 4, "y": 4, "dir": "V", "len": 2}
    },
    "expert_01.json": {
        # Red car at x=0, heavily blocked - 3 trucks pattern
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},  # Truck blocking row 2
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},  # Truck blocking row 2
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Truck blocking row 2
        "D": {"x": 5, "y": 3, "dir": "V", "len": 3},  # Right side
        "E": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Blocks A top
        "F": {"x": 0, "y": 3, "dir": "H", "len": 2},  # Blocks A bottom
        "G": {"x": 3, "y": 3, "dir": "H", "len": 2},  # Blocks B/C bottom
        "H": {"x": 0, "y": 4, "dir": "H", "len": 2},  # Creates complexity
        "I": {"x": 3, "y": 4, "dir": "H", "len": 2},
        "J": {"x": 0, "y": 5, "dir": "H", "len": 2}
    },
    "expert_02.json": {
        # Red car at x=0, different dense pattern
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "B": {"x": 3, "y": 1, "dir": "V", "len": 3},  # Blocks row 2
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Blocks row 2
        "D": {"x": 5, "y": 1, "dir": "V", "len": 2},  # Right edge
        "E": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Blocks A top
        "F": {"x": 0, "y": 3, "dir": "H", "len": 2},  # Blocks A/B bottom
        "G": {"x": 3, "y": 4, "dir": "H", "len": 2},  # Bottom area
        "H": {"x": 0, "y": 4, "dir": "H", "len": 2},  # Creates complexity
        "I": {"x": 0, "y": 5, "dir": "H", "len": 3},
        "J": {"x": 0, "y": 1, "dir": "H", "len": 2}  # Blocks A from moving up
    },
    "expert_03.json": {
        # Red car at x=0, maximum difficulty - four trucks
        "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
        "A": {"x": 2, "y": 0, "dir": "V", "len": 3},  # Main blocker
        "B": {"x": 3, "y": 0, "dir": "V", "len": 3},  # Secondary blocker
        "C": {"x": 4, "y": 0, "dir": "V", "len": 3},  # Tertiary blocker
        "D": {"x": 5, "y": 0, "dir": "V", "len": 3},  # Fourth blocker
        "E": {"x": 0, "y": 0, "dir": "H", "len": 2},  # Blocks A top
        "F": {"x": 0, "y": 3, "dir": "H", "len": 2},  # Blocks A bottom
        "G": {"x": 3, "y": 3, "dir": "H", "len": 2},  # Blocks B/C bottom
        "H": {"x": 0, "y": 4, "dir": "H", "len": 2},  # Creates complexity
        "I": {"x": 3, "y": 4, "dir": "H", "len": 2},
        "J": {"x": 0, "y": 5, "dir": "H", "len": 2},
        "K": {"x": 3, "y": 5, "dir": "H", "len": 2}
    }
}


def main():
    """Create and validate all puzzles."""
    project_dir = Path(__file__).parent
    puzzle_dir = project_dir / "car_parking_ai" / "puzzles"

    print("=" * 70)
    print("Creating Rush Hour Puzzle Files")
    print("=" * 70)
    print()

    results = []

    for filename, puzzle in puzzles.items():
        print(f"Processing {filename}...")

        # Validate
        is_valid, error = validate_puzzle(puzzle)
        if not is_valid:
            print(f"  ERROR: {error}")
            results.append((filename, "INVALID", error, -1, -1))
            continue

        # Solve
        try:
            moves, nodes = solve_puzzle(puzzle, project_dir / "car_parking_ai")
            if moves == -1:
                print(f"  ERROR: Unsolvable puzzle")
                results.append((filename, "UNSOLVABLE", "", -1, -1))
                continue

            # Save
            filepath = puzzle_dir / filename
            filepath.write_text(json.dumps(puzzle, indent=2))

            print(f"  OK: {moves} moves, {nodes} nodes explored")
            results.append((filename, "OK", "", moves, nodes))

        except Exception as e:
            print(f"  ERROR: {e}")
            results.append((filename, "ERROR", str(e), -1, -1))

    # Summary
    print()
    print("=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print()
    print(f"{'Puzzle':<20} {'Status':<12} {'Moves':<8} {'Nodes':<10}")
    print("-" * 70)

    for filename, status, error, moves, nodes in results:
        if status == "OK":
            print(f"{filename:<20} {status:<12} {moves:<8} {nodes:<10}")
        else:
            print(f"{filename:<20} {status:<12} {error}")

    print()
    print("=" * 70)
    successful = sum(1 for _, status, _, _, _ in results if status == "OK")
    print(f"Successfully created {successful}/{len(puzzles)} puzzles")
    print("=" * 70)


if __name__ == "__main__":
    main()
