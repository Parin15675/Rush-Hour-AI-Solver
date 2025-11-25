# Rush Hour AI Solver – Pixel Edition

An interactive Rush Hour–style puzzle game with an AI solver, built with Python, Pygame, and Prolog.  
The project was created as part of an AI course to explore informed search (A*) on a combinatorial puzzle.

---

## Features

- **AI Solver (A\*)**
  - State–space A* search implemented in **Prolog** (`logic.pl`).
  - Domain‑specific heuristic (distance to exit, row alignment, blocking cars) for faster solving.
  - Supports both manual play and automatic solving of each puzzle.

- **Pixel‑Art Game UI**
  - Smooth animations for car movement and rotation.
  - Custom pixel‑art cars, trucks, and obstacles.
  - Single fixed exit per level; red car must drive “out” with its front.

- **Flexible Board & Levels**
  - Supports 6×6 and 9×9 boards.
  - Levels defined in JSON under `puzzles/` (e.g. `expert_13.json`–`expert_20.json`).
  - Each level can specify:
    - Board size and exit position.
    - Cars (id, position, heading, length).
    - Poles (obstacles) that block paths.

- **Movement Rules**
  - All cars slide along their current axis.
  - **Only the red car** is allowed to rotate.
  - Each single‑cell movement counts as 1 move (no multi‑cell jumps).

---

## Tech Stack

- **Language:** Python 3.x, Prolog (SWI‑Prolog)
- **Libraries:**
  - `pygame` – rendering, input, audio, animation
  - `pyswip` – bridge between Python and SWI‑Prolog
- **Core Files:**
  - `main.py` – game loop, UI, menus, and event handling
  - `game.py` – board representation, animation, and move querying
  - `search.py` – Python ↔ Prolog glue (calls `prolog_a_star/4`, `heuristic/2`, `goal_state/1`)
  - `logic.pl` – Prolog move rules, heuristic, and A* implementation
  - `puzzles/*.json` – level definitions

---

## Installation

1. **Clone the repository**

   ```bash
   git clone https://github.com/<your-username>/<your-repo>.git
   cd <your-repo>
