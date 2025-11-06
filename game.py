from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

try:
    from pyswip import Prolog
except ImportError:
    Prolog = None


StateTuple = Tuple[Tuple[str, int, int, str, int], ...]
MoveTuple = Tuple[str, str, StateTuple]


@dataclass(frozen=True)
class Car:

    identifier: str
    x: int
    y: int
    orientation: str
    length: int

    def occupy_cells(self) -> Iterable[Tuple[int, int]]:

        if self.orientation.upper() == "H":
            for offset in range(self.length):
                yield self.x + offset, self.y
        else:
            for offset in range(self.length):
                yield self.x, self.y + offset


class AnimatedCar:

    def __init__(self, car: Car) -> None:
        self.car = car
        self.display_x = float(car.x)
        self.display_y = float(car.y)
        self.target_x = float(car.x)
        self.target_y = float(car.y)

    def update_target(self, car: Car) -> None:

        self.car = car
        self.target_x = float(car.x)
        self.target_y = float(car.y)

    def animate(self, speed: float = 0.25) -> bool:

        dx = self.target_x - self.display_x
        dy = self.target_y - self.display_y
        distance = (dx * dx + dy * dy) ** 0.5

        if distance < 0.01:
            self.display_x = self.target_x
            self.display_y = self.target_y
            return False

        self.display_x += dx * speed
        self.display_y += dy * speed
        return True


class Game:

    grid_size: int = 6

    def __init__(self, asset_dir: Path, logic_path: Optional[Path] = None) -> None:

        self.asset_dir = Path(asset_dir)
        self.logic_path = (
            Path(logic_path) if logic_path else Path(__file__).with_name("logic.pl")
        )
        self.cars: Dict[str, Car] = {}
        self.animated_cars: Dict[str, AnimatedCar] = {}
        self.initial_state: Optional[StateTuple] = None
        self.current_state: Optional[StateTuple] = None
        self.solution_path: List[MoveTuple] = []
        self.solution_index: int = 0
        self.nodes_expanded: int = 0
        self.last_heuristic: float = 0.0
        self._prolog: Optional[Prolog] = None
        self._prolog_available: bool = False
        self._texture_cache: Dict[str, Optional["pygame.Surface"]] = {}
        self._initialise_prolog()

    def load_level(self, file_path: Path) -> None:

        data = json.loads(Path(file_path).read_text(encoding="utf-8"))
        self.cars = {}
        self.animated_cars = {}
        for car_id, attrs in data.items():
            car = Car(
                identifier=car_id,
                x=int(attrs["x"]),
                y=int(attrs["y"]),
                orientation=str(attrs["dir"]).upper(),
                length=int(attrs["len"]),
            )
            self.cars[car_id] = car
            self.animated_cars[car_id] = AnimatedCar(car)
        self.initial_state = self._cars_to_state()
        self.current_state = self.initial_state
        self.solution_path = []
        self.solution_index = 0
        self.nodes_expanded = 0
        self.last_heuristic = 0.0

    def update_animations(self) -> None:

        for animated_car in self.animated_cars.values():
            animated_car.animate()

    def move_car(self, car_id: str, direction: str) -> bool:

        if not self.current_state:
            return False
        for candidate_id, candidate_dir, next_state in self.get_valid_moves(
            self.current_state
        ):
            if candidate_id == car_id and candidate_dir == direction:
                self.current_state = next_state
                self._update_cars_from_state(next_state)
                return True
        return False

    def draw(self, screen) -> None:

        import pygame
        import math

        cell_size = screen.get_height() // self.grid_size

        screen.fill((32, 34, 40))

        background_path = self.asset_dir / "background.png"
        if background_path.exists():
            try:
                tile = pygame.image.load(str(background_path)).convert_alpha()
                tile = pygame.transform.scale(tile, (cell_size, cell_size))
                for gy in range(self.grid_size):
                    for gx in range(self.grid_size):
                        screen.blit(tile, (gx * cell_size, gy * cell_size))
            except pygame.error:
                pass

        exit_y = 2
        exit_rect = pygame.Rect(
            (self.grid_size - 1) * cell_size, exit_y * cell_size, cell_size, cell_size
        )
        exit_glow = pygame.Surface((cell_size, cell_size), pygame.SRCALPHA)
        pulse = int(100 + 50 * math.sin(pygame.time.get_ticks() / 500))
        pygame.draw.rect(exit_glow, (0, 255, 100, pulse), exit_glow.get_rect())
        screen.blit(exit_glow, exit_rect.topleft)

        for car_id, animated_car in self.animated_cars.items():
            car = animated_car.car

            rect = pygame.Rect(
                animated_car.display_x * cell_size,
                animated_car.display_y * cell_size,
                (cell_size * car.length) if car.orientation == "H" else cell_size,
                (cell_size * car.length) if car.orientation == "V" else cell_size,
            )

            if car.identifier.upper() == "R":
                colour = (255, 60, 60)
                accent_color = (255, 120, 120)
                texture_name = "car_red.png"
            else:
                palette = {
                    0: ((100, 200, 255), (150, 220, 255)),
                    1: ((255, 200, 100), (255, 220, 150)),
                    2: ((150, 255, 150), (200, 255, 200)),
                }
                colours = palette[hash(car.identifier) % len(palette)]
                colour = colours[0]
                accent_color = colours[1]
                texture_name = {
                    0: "car_blue.png",
                    1: "car_yellow.png",
                }.get(hash(car.identifier) % 2, "car_blue.png")

            if car.length == 3 and self._draw_truck(
                screen, animated_car, cell_size, car.orientation
            ):
                continue

            flip_red = False
            rendered_texture = self._blit_texture(
                screen,
                texture_name,
                rect,
                car.orientation,
                flip_horizontal=flip_red,
            )

            if not rendered_texture:

                shadow_rect = rect.copy()
                shadow_rect.x += 3
                shadow_rect.y += 3
                shadow_surf = pygame.Surface(
                    (int(shadow_rect.width), int(shadow_rect.height)), pygame.SRCALPHA
                )
                pygame.draw.rect(
                    shadow_surf, (0, 0, 0, 80), shadow_surf.get_rect(), border_radius=12
                )
                screen.blit(shadow_surf, shadow_rect.topleft)

                car_surf = pygame.Surface(
                    (int(rect.width), int(rect.height)), pygame.SRCALPHA
                )
                for i in range(int(rect.height)):
                    factor = i / rect.height
                    blend_color = tuple(
                        int(
                            colour[j] * (1 - factor * 0.4)
                            + accent_color[j] * factor * 0.4
                        )
                        for j in range(3)
                    )
                    pygame.draw.line(
                        car_surf, blend_color, (0, i), (int(rect.width), i)
                    )

                mask = pygame.Surface(
                    (int(rect.width), int(rect.height)), pygame.SRCALPHA
                )
                pygame.draw.rect(
                    mask, (255, 255, 255), mask.get_rect(), border_radius=12
                )
                car_surf.blit(mask, (0, 0), special_flags=pygame.BLEND_RGBA_MIN)
                screen.blit(car_surf, rect.topleft)

                highlight = pygame.Surface(
                    (int(rect.width), int(rect.height // 3)), pygame.SRCALPHA
                )
                pygame.draw.rect(
                    highlight,
                    (*accent_color, 100),
                    highlight.get_rect(),
                    border_radius=10,
                )
                screen.blit(highlight, (rect.x, rect.y))

    def apply_solution(self, path: List[MoveTuple]) -> None:

        self.solution_path = path
        self.solution_index = 0

    def step_solution(self) -> bool:

        if not self.solution_path or self.solution_index >= len(self.solution_path):
            return False
        _, _, next_state = self.solution_path[self.solution_index]
        self.current_state = next_state
        self._update_cars_from_state(next_state)
        self.solution_index += 1
        return True

    def reset(self) -> None:

        if not self.initial_state:
            return
        self.current_state = self.initial_state
        self._update_cars_from_state(self.initial_state)
        self.solution_path = []
        self.solution_index = 0
        self.nodes_expanded = 0
        self.last_heuristic = 0.0

    def get_valid_moves(self, state: Optional[StateTuple] = None) -> List[MoveTuple]:

        active_state = state or self.current_state
        if not active_state:
            return []
        if self._prolog_available:
            return self._query_prolog_moves(active_state)
        return self._python_valid_moves(active_state)

    def _initialise_prolog(self) -> None:

        if Prolog is None:
            self._prolog_available = False
            return
        self._prolog = Prolog()
        self._prolog.consult(str(self.logic_path))
        self._prolog_available = True

    def _cars_to_state(self) -> StateTuple:

        return tuple(
            sorted(
                (car.identifier, car.x, car.y, car.orientation, car.length)
                for car in self.cars.values()
            )
        )

    def _update_cars_from_state(self, state: StateTuple) -> None:

        self.cars = {
            identifier: Car(identifier, x, y, orientation, length)
            for identifier, x, y, orientation, length in state
        }

        for identifier, car in self.cars.items():
            if identifier in self.animated_cars:
                self.animated_cars[identifier].update_target(car)
            else:
                self.animated_cars[identifier] = AnimatedCar(car)

    def _load_texture(self, filename: str) -> Optional["pygame.Surface"]:

        import pygame

        if filename not in self._texture_cache:
            path = self.asset_dir / filename
            if not path.exists():
                self._texture_cache[filename] = None
            else:
                try:
                    self._texture_cache[filename] = pygame.image.load(
                        str(path)
                    ).convert_alpha()
                except pygame.error:
                    self._texture_cache[filename] = None
        return self._texture_cache[filename]

    def _blit_texture(
        self,
        screen,
        filename: str,
        rect,
        orientation: str,
        flip_horizontal: bool = False,
        flip_vertical: bool = False,
    ) -> bool:

        import pygame

        base_texture = self._load_texture(filename)
        if base_texture is None:
            return False
        texture = base_texture
        if orientation == "H":
            texture = pygame.transform.rotate(texture, -90)
        if flip_horizontal or flip_vertical:
            texture = pygame.transform.flip(texture, flip_horizontal, flip_vertical)
        texture = pygame.transform.scale(texture, (int(rect.width), int(rect.height)))
        screen.blit(texture, rect.topleft)
        return True

    def _draw_truck(
        self, screen, animated_car: AnimatedCar, cell_size: int, orientation: str
    ) -> bool:

        if (
            self._load_texture("truck_red.png") is None
            or self._load_texture("trailer.png") is None
        ):
            return False

        import pygame

        offsets = (
            [(0, 0), (1, 0), (2, 0)] if orientation == "H" else [(0, 0), (0, 1), (0, 2)]
        )
        base_x = animated_car.display_x * cell_size
        base_y = animated_car.display_y * cell_size
        for index, (ox, oy) in enumerate(offsets):
            dest_rect = pygame.Rect(
                base_x + ox * cell_size,
                base_y + oy * cell_size,
                cell_size,
                cell_size,
            )
            is_head = index == 0
            texture_name = "truck_red.png" if is_head else "trailer.png"
            if not self._blit_texture(
                screen,
                texture_name,
                dest_rect,
                orientation,
                flip_vertical=is_head and orientation == "V",
            ):
                return False
        return True

    def _state_to_prolog(self, state: StateTuple) -> str:

        parts = []
        for identifier, x, y, orientation, length in state:
            parts.append(
                f"car({identifier.lower()},{x},{y},{orientation.lower()},{length})"
            )
        return f"[{', '.join(parts)}]"

    def _query_prolog_moves(self, state: StateTuple) -> List[MoveTuple]:

        if not self._prolog:
            return []
        prolog_state = self._state_to_prolog(state)
        query = f"valid_move({prolog_state}, Car, Dir, NewState)"
        moves: List[MoveTuple] = []
        for solution in self._prolog.query(query):
            car_id = str(solution["Car"]).upper()
            direction = str(solution["Dir"]).lower()
            new_state = self._parse_prolog_state(solution["NewState"])
            moves.append((car_id, direction, new_state))
        return moves

    def _parse_prolog_state(self, state_term) -> StateTuple:

        parsed: List[Tuple[str, int, int, str, int]] = []
        for car in state_term:
            identifier = str(car.args[0]).upper()
            x = int(car.args[1])
            y = int(car.args[2])
            orientation = str(car.args[3]).upper()
            length = int(car.args[4])
            parsed.append((identifier, x, y, orientation, length))
        parsed.sort()
        return tuple(parsed)

    def _python_valid_moves(self, state: StateTuple) -> List[MoveTuple]:

        occupied = self._build_occupancy_map(state)
        moves: List[MoveTuple] = []

        for identifier, x, y, orientation, length in state:

            current_car_cells = set()
            if orientation == "H":
                current_car_cells = {(x + i, y) for i in range(length)}
            else:
                current_car_cells = {(x, y + i) for i in range(length)}

            if orientation == "H":

                if x - 1 >= 0:
                    new_cell = (x - 1, y)
                    if new_cell not in occupied or new_cell in current_car_cells:
                        next_state = self._apply_move(state, identifier, -1, 0)
                        moves.append((identifier, "left", next_state))

                if x + length < self.grid_size:
                    new_cell = (x + length, y)
                    if new_cell not in occupied or new_cell in current_car_cells:
                        next_state = self._apply_move(state, identifier, 1, 0)
                        moves.append((identifier, "right", next_state))
            else:

                if y - 1 >= 0:
                    new_cell = (x, y - 1)
                    if new_cell not in occupied or new_cell in current_car_cells:
                        next_state = self._apply_move(state, identifier, 0, -1)
                        moves.append((identifier, "up", next_state))

                if y + length < self.grid_size:
                    new_cell = (x, y + length)
                    if new_cell not in occupied or new_cell in current_car_cells:
                        next_state = self._apply_move(state, identifier, 0, 1)
                        moves.append((identifier, "down", next_state))

        return moves

    def _build_occupancy_map(self, state: StateTuple) -> Dict[Tuple[int, int], str]:

        occupancy: Dict[Tuple[int, int], str] = {}
        for identifier, x, y, orientation, length in state:
            if orientation == "H":
                for offset in range(length):
                    occupancy[(x + offset, y)] = identifier
            else:
                for offset in range(length):
                    occupancy[(x, y + offset)] = identifier
        return occupancy

    def _apply_move(
        self, state: StateTuple, car_id: str, dx: int, dy: int
    ) -> StateTuple:

        new_state = []
        for identifier, x, y, orientation, length in state:
            if identifier == car_id:
                new_state.append((identifier, x + dx, y + dy, orientation, length))
            else:
                new_state.append((identifier, x, y, orientation, length))
        new_state.sort()
        return tuple(new_state)
