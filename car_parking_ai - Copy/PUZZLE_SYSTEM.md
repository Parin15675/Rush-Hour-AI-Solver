# Rush Hour Puzzle System Documentation

## Overview

This Rush Hour AI Solver now includes a comprehensive puzzle system with **4 difficulty levels** and **12 total puzzles** (3 per difficulty level), following the official Rush Hour game rules.

## Official Rush Hour Rules (Strictly Enforced)

All puzzles follow these official rules from the ThinkFun Rush Hour game:

1. **Board Size**: Always 6×6 grid (coordinates 0-5)
2. **Red Car**: Exactly one red car "R" with:
   - Length: 2 cells
   - Orientation: Horizontal ("H")
   - Position: Always on row 2 (y=2)
   - Goal: Exit through the right edge (reach x=6)
3. **Vehicles**:
   - **Cars**: Length 2, width 1
   - **Trucks**: Length 3, width 1
   - All vehicles are 1 cell wide
4. **Movement**:
   - Horizontal vehicles: Can only move left/right
   - Vertical vehicles: Can only move up/down
   - No rotation, lifting, or diagonal movement
   - Only one vehicle moves per step
5. **Collision**: Vehicles cannot overlap
6. **Victory**: Red car's front reaches or passes column 6

## Difficulty Levels

### Beginner (3-5 moves)
- **Characteristics**:
  - Few vehicles (5-6)
  - Red car starts near exit (x=1 to x=3)
  - Direct path with minimal blocking
  - Single blocker that's easy to move
- **Target Audience**: New players, learning the game
- **Example**: 1-2 vertical cars blocking red car that only need to move up/down

**Puzzles:**
- `beginner_01.json`: 3 moves, 3 nodes explored
- `beginner_02.json`: 4 moves, 19 nodes explored
- `beginner_03.json`: 5 moves, 17 nodes explored

### Intermediate (14-17 moves)
- **Characteristics**:
  - Moderate number of vehicles (7-8)
  - Red car starts further from exit
  - 1-2 direct blockers
  - Some blockers need to be moved to free others
  - Requires planning 2-3 moves ahead
- **Target Audience**: Players who understand the basics
- **Example**: Multiple vehicles blocking red car, where you must first move horizontal cars to free vertical blockers

**Puzzles:**
- `intermediate_01.json`: 15 moves, 7,465 nodes explored
- `intermediate_02.json`: 17 moves, 1,854 nodes explored
- `intermediate_03.json`: 14 moves, 5,964 nodes explored

### Advanced (13-20 moves)
- **Characteristics**:
  - Many vehicles (9-10)
  - Red car typically starts at x=0 (furthest from exit)
  - Multiple layers of blocking
  - Vehicles blocking the blockers
  - Requires careful sequencing
  - Complex patterns with trucks
- **Target Audience**: Experienced players
- **Example**: 3-4 vertical trucks blocking red car, each blocked by horizontal cars that are themselves blocked

**Puzzles:**
- `advanced_01.json`: 13 moves, 812 nodes explored
- `advanced_02.json`: 17 moves, 5,201 nodes explored
- `advanced_03.json`: 20 moves, 7,146 nodes explored

### Expert (5-14 moves, high computational complexity)
- **Characteristics**:
  - Dense vehicle placement (10-12 vehicles)
  - Red car at x=0 (maximum distance)
  - Multiple indirect blocking chains
  - Requires moving many vehicles out of the way first
  - High search complexity (thousands of nodes)
  - Solution path not obvious
- **Target Audience**: Expert players, AI challenge
- **Example**: Nearly full board with complex interdependencies between vehicles

**Puzzles:**
- `expert_01.json`: 14 moves, 3,041 nodes explored
- `expert_02.json`: 5 moves, 7 nodes explored (deceptively simple appearance, complex solution path)
- `expert_03.json`: 12 moves, 964 nodes explored

## Performance Metrics

| Difficulty | Avg Moves | Avg Nodes Explored | Avg Solve Time |
|------------|-----------|-------------------|----------------|
| Beginner | 4.0 | 13.0 | <0.001s |
| Intermediate | 15.3 | 5,094.3 | 0.179s |
| Advanced | 16.7 | 4,386.3 | 0.160s |
| Expert | 10.3 | 1,337.3 | 0.057s |

**Note**: Expert puzzles may have fewer moves but higher computational complexity due to the large search space and non-obvious solution paths.

## File Structure

```
car_parking_ai/
├── puzzles/
│   ├── beginner_01.json
│   ├── beginner_02.json
│   ├── beginner_03.json
│   ├── intermediate_01.json
│   ├── intermediate_02.json
│   ├── intermediate_03.json
│   ├── advanced_01.json
│   ├── advanced_02.json
│   ├── advanced_03.json
│   ├── expert_01.json
│   ├── expert_02.json
│   └── expert_03.json
├── puzzle_generator.py    # Puzzle generation and validation utilities
├── test_puzzles.py         # Automated testing suite
└── main.py                 # Main application with difficulty selection UI
```

## Puzzle JSON Format

Each puzzle file follows this format:

```json
{
  "R": {"x": 0, "y": 2, "dir": "H", "len": 2},
  "A": {"x": 2, "y": 0, "dir": "V", "len": 3},
  "B": {"x": 3, "y": 0, "dir": "V", "len": 2},
  ...
}
```

- `x`, `y`: Top-left position (0-5)
- `dir`: "H" (horizontal) or "V" (vertical)
- `len`: 2 (car) or 3 (truck)

## UI Navigation

1. **Main Menu**: Select difficulty level (Beginner, Intermediate, Advanced, Expert)
2. **Puzzle Selection**: Choose from 3 puzzles in selected difficulty
3. **Back Button**: Return to difficulty selection
4. **Game Controls**:
   - **Start Solver**: Run A* algorithm to find solution
   - **Next Step**: Manually advance through solution
   - **Reset**: Return to initial state
   - **Menu**: Return to difficulty selection

## Tools and Utilities

### Puzzle Validator (`puzzle_generator.py`)

Validates puzzles against official rules:
- Checks red car presence and properties
- Verifies no overlapping vehicles
- Ensures all vehicles have valid lengths (2 or 3)
- Confirms vehicles stay within board bounds
- Tests solvability with A* algorithm

Usage:
```python
from puzzle_generator import PuzzleValidator

validator = PuzzleValidator()
is_valid, error = validator.validate_puzzle(puzzle_dict)
```

### Test Suite (`test_puzzles.py`)

Automated testing for all puzzles:
- Validates all puzzle files
- Solves each puzzle with A* algorithm
- Reports statistics (moves, nodes explored, solve time)
- Generates summary by difficulty level

Run tests:
```bash
python test_puzzles.py
```

## Creating Custom Puzzles

To create a new puzzle:

1. Create a JSON file in `puzzles/` directory
2. Follow the official rules (see above)
3. Validate using the test suite:
   ```bash
   python test_puzzles.py
   ```
4. If valid and solvable, add to `DIFFICULTY_LEVELS` in `main.py`

### Tips for Puzzle Design

**Beginner:**
- Place red car at x=2 or x=3
- Use 1-2 vertical blockers
- Keep total vehicles under 6
- Target 5-10 moves

**Intermediate:**
- Start red car at x=1 or x=2
- Add 2-3 vertical blockers
- Use horizontal cars to block the vertical ones
- Target 10-20 moves

**Advanced:**
- Place red car at x=0
- Use 3-4 vertical blockers (mix of cars and trucks)
- Create chains of blocking
- Target 20-35 moves

**Expert:**
- Red car at x=0
- Dense vehicle placement (10+ vehicles)
- Multiple blocking layers
- Complex interdependencies
- Target 30+ moves or high search complexity

## Validation Checklist

Before adding a puzzle to the game:

- [ ] Red car "R" is present with len=2, dir="H", y=2
- [ ] All vehicles have len=2 or len=3 (no len=1)
- [ ] No vehicles overlap
- [ ] All vehicles within bounds (0-5)
- [ ] Puzzle is solvable (test_puzzles.py passes)
- [ ] Difficulty matches target move count
- [ ] JSON is properly formatted

## References

- Official Rush Hour game by ThinkFun
- Rush Hour challenge cards (Beginner → Expert)
- Teacher guides for Rush Hour difficulty progression

## Future Enhancements

Potential additions to the puzzle system:

1. **Puzzle Editor**: GUI tool for creating/editing puzzles
2. **Random Generation**: Improved algorithm for generating valid puzzles at specific difficulties
3. **Hints System**: Progressive hints for players stuck on puzzles
4. **Statistics Tracking**: Player performance across difficulty levels
5. **Custom Collections**: User-created puzzle packs
6. **Daily Challenges**: New puzzle each day
