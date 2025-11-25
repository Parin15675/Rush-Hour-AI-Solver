from __future__ import annotations

import json
import ast
from dataclasses import dataclass
import re
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple
import math

try:
    from pyswip import Prolog
except ImportError:
    Prolog = None


StateTuple = Tuple[Tuple[str, int, int, str, int], ...]
MoveTuple = Tuple[str, str, StateTuple]

DEFAULT_GRID_WIDTH = 6
DEFAULT_GRID_HEIGHT = 6
DEFAULT_EXIT_ROW = 2
DEFAULT_EXIT_COL = 6

_HEADING_SEQUENCE = ("N", "E", "S", "W")
_HORIZONTAL_HEADINGS = {"E", "W"}
_VERTICAL_HEADINGS = {"N", "S"}
_HEADING_TO_ANGLE = {"N": 0.0, "E": -90.0, "S": 180.0, "W": 90.0}


def _normalize_heading(value: str) -> str:

    heading = value.upper()
    if heading in ("H", "E"):
        return "E"
    if heading in ("V", "N"):
        return "N"
    if heading not in _HEADING_SEQUENCE:
        raise ValueError(f"Unsupported heading value: {value!r}")
    return heading


def _heading_axis(heading: str) -> str:

    return "H" if heading in _HORIZONTAL_HEADINGS else "V"


def _heading_to_angle(heading: str) -> float:

    return _HEADING_TO_ANGLE.get(heading.upper(), 0.0)


def _rotate_heading(heading: str, direction: str) -> str:

    index = _HEADING_SEQUENCE.index(heading)
    if direction == "rotate_cw":
        return _HEADING_SEQUENCE[(index + 1) % len(_HEADING_SEQUENCE)]
    if direction == "rotate_ccw":
        return _HEADING_SEQUENCE[(index - 1) % len(_HEADING_SEQUENCE)]
    raise ValueError(f"Unsupported rotation direction: {direction!r}")


def _center_times_two(x: int, y: int, heading: str, length: int) -> Tuple[int, int]:

    axis = _heading_axis(heading)
    if axis == "H":
        return 2 * x + (length - 1), 2 * y
    return 2 * x, 2 * y + (length - 1)


def _half_round_up(value: int) -> int:

    return (value + 1) // 2


def _anchor_from_center(center_x2: int, center_y2: int, axis: str, length: int) -> Tuple[int, int]:

    if axis == "H":
        new_x = _half_round_up(center_x2 - (length - 1))
        new_y = _half_round_up(center_y2)
    else:
        new_x = _half_round_up(center_x2)
        new_y = _half_round_up(center_y2 - (length - 1))
    return new_x, new_y


def _front_position(x: int, y: int, heading: str, length: int) -> Tuple[int, int]:

    heading = heading.upper()
    if heading == "E":
        return x + length - 1, y
    if heading == "W":
        return x, y
    if heading == "N":
        return x, y
    if heading == "S":
        return x, y + length - 1
    raise ValueError(f"Unsupported heading: {heading}")


def _anchor_from_front(
    front_x: int, front_y: int, heading: str, length: int
) -> Tuple[int, int]:

    heading = heading.upper()
    if heading == "E":
        return front_x - (length - 1), front_y
    if heading == "W":
        return front_x, front_y
    if heading == "N":
        return front_x, front_y
    if heading == "S":
        return front_x, front_y - (length - 1)
    raise ValueError(f"Unsupported heading: {heading}")


def _heading_forward_step(heading: str) -> Tuple[int, int]:

    heading = heading.upper()
    if heading == "E":
        return 1, 0
    if heading == "W":
        return -1, 0
    if heading == "N":
        return 0, -1
    if heading == "S":
        return 0, 1
    raise ValueError(f"Unsupported heading: {heading}")


_CAR_PATTERN = re.compile(
    r"""car\(\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*$""",
    re.IGNORECASE,
)


@dataclass(frozen=True)
class Car:

    identifier: str
    x: int
    y: int
    heading: str
    length: int

    @property
    def orientation(self) -> str:

        return _heading_axis(self.heading)

    def occupy_cells(self) -> Iterable[Tuple[int, int]]:

        if self.orientation == "H":
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
        self.display_heading = car.heading
        self.target_heading = car.heading
        self.display_angle = _heading_to_angle(car.heading)
        self.target_angle = self.display_angle

    def update_target(self, car: Car) -> None:

        self.car = car
        self.target_x = float(car.x)
        self.target_y = float(car.y)
        self.target_heading = car.heading
        self.target_angle = _heading_to_angle(car.heading)

    def animate(self, speed: float = 0.25, rotation_speed: float = 2.0) -> bool:

        dx = self.target_x - self.display_x
        dy = self.target_y - self.display_y
        distance = (dx * dx + dy * dy) ** 0.5
        moving = False

        if distance < 0.01:
            self.display_x = self.target_x
            self.display_y = self.target_y
        else:
            self.display_x += dx * speed
            self.display_y += dy * speed
            moving = True

        diff = (self.target_angle - self.display_angle + 180.0) % 360.0 - 180.0
        rotating = False
        if abs(diff) > 0.5:
            step = max(-rotation_speed, min(rotation_speed, diff))
            self.display_angle += step
            rotating = True
        else:
            self.display_angle = self.target_angle
            self.display_heading = self.target_heading

        return moving or rotating


class Game:

    def __init__(self, asset_dir: Path, logic_path: Optional[Path] = None) -> None:

        self.asset_dir = Path(asset_dir)
        self.logic_path = (
            Path(logic_path) if logic_path else Path(__file__).with_name("logic.pl")
        )
        self.grid_width = DEFAULT_GRID_WIDTH
        self.grid_height = DEFAULT_GRID_HEIGHT
        self.exit_row = DEFAULT_EXIT_ROW
        self.exit_col = DEFAULT_EXIT_COL
        self.exit_rows: Tuple[int, ...] = (DEFAULT_EXIT_ROW,)
        self.exit_any_row: bool = False
        self.poles: List[Tuple[int, int]] = []
        self.grid_labels: List[List[str]] = []
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
        board_data = data.get("board", {})
        cars_payload = data.get("cars")
        if cars_payload is None:
            cars_payload = {
                key: value
                for key, value in data.items()
                if isinstance(value, dict) and {"x", "y", "len"}.issubset(value.keys())
            }

        width = int(board_data.get("width", board_data.get("size", DEFAULT_GRID_WIDTH)))
        height = int(board_data.get("height", board_data.get("size", DEFAULT_GRID_HEIGHT)))
        exit_info = board_data.get("exit", {})
        exit_row = int(exit_info.get("row", DEFAULT_EXIT_ROW))
        exit_col = int(exit_info.get("col", width))

        self.grid_width = max(1, width)
        self.grid_height = max(1, height)
        self.exit_row = max(0, min(exit_row, self.grid_height - 1))
        self.exit_col = max(1, exit_col)
        exit_rows_raw = exit_info.get("rows")
        self.exit_rows = self._parse_exit_rows(exit_rows_raw, self.exit_row)
        self.exit_any_row = bool(exit_info.get("any_row", False))
        raw_poles = data.get("poles", [])
        self.poles = self._parse_poles(raw_poles, self.grid_width, self.grid_height)
        self.grid_labels = data.get("grid_labels") or []
        self._configure_prolog_board()

        self.cars = {}
        self.animated_cars = {}
        for car_id, attrs in cars_payload.items():
            car = Car(
                identifier=car_id,
                x=int(attrs["x"]),
                y=int(attrs["y"]),
                heading=_normalize_heading(str(attrs.get("heading", attrs.get("dir", "H")))),
                length=int(attrs["len"]),
            )
            self.cars[car_id] = car
            self.animated_cars[car_id] = AnimatedCar(car)
        self._validate_poles_against_cars()
        self.initial_state = self._cars_to_state()
        self.current_state = self.initial_state
        self.solution_path = []
        self.solution_index = 0
        self.nodes_expanded = 0
        self.last_heuristic = 0.0

    def update_animations(self) -> bool:

        animating = False
        for animated_car in self.animated_cars.values():
            animating = animated_car.animate() or animating
        return animating

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

        grid_width = max(1, self.grid_width)
        grid_height = max(1, self.grid_height)
        cell_size = max(
            1,
            min(
                screen.get_width() // grid_width,
                screen.get_height() // grid_height,
            ),
        )
        board_width_px = cell_size * grid_width
        board_height_px = cell_size * grid_height
        offset_x = (screen.get_width() - board_width_px) // 2
        offset_y = (screen.get_height() - board_height_px) // 2

        screen.fill((32, 34, 40))

        background_path = self.asset_dir / "background.png"
        if background_path.exists():
            try:
                tile = pygame.image.load(str(background_path)).convert_alpha()
                tile = pygame.transform.scale(tile, (cell_size, cell_size))
                for gy in range(grid_height):
                    for gx in range(grid_width):
                        screen.blit(
                            tile, (offset_x + gx * cell_size, offset_y + gy * cell_size)
                        )
            except pygame.error:
                pass

        if self.exit_any_row:
            rows_to_draw = range(grid_height)
        elif self.exit_rows:
            rows_to_draw = self.exit_rows
        else:
            rows_to_draw = (self.exit_row,)
        pulse = int(100 + 50 * math.sin(pygame.time.get_ticks() / 500))
        for exit_row in rows_to_draw:
            bounded_row = max(0, min(exit_row, grid_height - 1))
            if self.exit_col >= grid_width:
                exit_rect = pygame.Rect(
                    offset_x + board_width_px - cell_size // 3,
                    offset_y + bounded_row * cell_size,
                    max(4, cell_size // 2),
                    cell_size,
                )
            else:
                exit_rect = pygame.Rect(
                    offset_x + self.exit_col * cell_size,
                    offset_y + bounded_row * cell_size,
                    cell_size,
                    cell_size,
                )
            exit_glow = pygame.Surface(
                (exit_rect.width, exit_rect.height), pygame.SRCALPHA
            )
            pygame.draw.rect(
                exit_glow, (0, 255, 100, pulse), exit_glow.get_rect(), border_radius=6
            )
            screen.blit(exit_glow, exit_rect.topleft)

        if self.poles:
            pole_size = max(6, cell_size // 2)
            highlight_margin = max(1, pole_size // 6)
            pole_colour = (190, 30, 45)
            highlight_colour = (255, 130, 140)
            shadow_colour = (90, 0, 0)
            for px, py in self.poles:
                base_rect = pygame.Rect(
                    offset_x + px * cell_size + (cell_size - pole_size) // 2,
                    offset_y + py * cell_size + (cell_size - pole_size) // 2,
                    pole_size,
                    pole_size,
                )
                pygame.draw.rect(screen, shadow_colour, base_rect.inflate(2, 2))
                pygame.draw.rect(screen, pole_colour, base_rect)
                inner = base_rect.inflate(-2 * highlight_margin, -2 * highlight_margin)
                if inner.width > 0 and inner.height > 0:
                    pygame.draw.rect(screen, highlight_colour, inner)

        if self.grid_labels:
            font_size = max(10, cell_size // 3)
            try:
                label_font = pygame.font.SysFont("Consolas", font_size)
            except pygame.error:
                label_font = pygame.font.Font(None, font_size)
            max_rows = min(grid_height, len(self.grid_labels))
            for row_index in range(max_rows):
                row_labels = self.grid_labels[row_index]
                max_cols = min(grid_width, len(row_labels))
                for col_index in range(max_cols):
                    label = row_labels[col_index]
                    if not label:
                        continue
                    label_surface = label_font.render(str(label), True, (210, 210, 210))
                    label_rect = label_surface.get_rect(
                        center=(
                            offset_x + col_index * cell_size + cell_size // 2,
                            offset_y + row_index * cell_size + cell_size // 2,
                        )
                    )
                    screen.blit(label_surface, label_rect)

        animated_items = list(self.animated_cars.items())

        def _is_rotating(animated: AnimatedCar) -> bool:
            return abs(animated.display_angle - animated.target_angle) > 0.1

        static_cars = [
            (cid, animated) for cid, animated in animated_items if not _is_rotating(animated)
        ]
        rotating_cars = [
            (cid, animated) for cid, animated in animated_items if _is_rotating(animated)
        ]

        for car_id, animated_car in static_cars + rotating_cars:
            car = animated_car.car

            rect = pygame.Rect(
                offset_x + animated_car.display_x * cell_size,
                offset_y + animated_car.display_y * cell_size,
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
                screen, animated_car, cell_size, offset_x, offset_y
            ):
                continue

            flip_red = False
            rendered_texture = self._blit_texture(
                screen,
                texture_name,
                rect,
                animated_car.display_angle,
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
        if not self._prolog_available:
            raise RuntimeError(
                "Prolog engine is unavailable; cannot compute valid moves."
            )
        return self._query_prolog_moves(active_state)

    def _initialise_prolog(self) -> None:

        if Prolog is None:
            self._prolog_available = False
            return
        self._prolog = Prolog()
        self._prolog.consult(str(self.logic_path))
        self._prolog_available = True
        self._configure_prolog_board()

    def _configure_prolog_board(self) -> None:

        if not self._prolog_available or self._prolog is None:
            return
        try:
            pole_terms = ", ".join(f"pole({x},{y})" for x, y in self.poles)
            exit_spec = self._format_exit_spec()
            command = "logic:configure_board({},{},{},{},[{}])".format(
                self.grid_width,
                self.grid_height,
                exit_spec,
                self.exit_col,
                pole_terms,
            )
            list(self._prolog.query(command))
        except Exception:
            self._prolog_available = False

    def _parse_poles(
        self, entries, width: int, height: int
    ) -> List[Tuple[int, int]]:

        poles = set()
        if not entries:
            return []
        for entry in entries:
            try:
                if isinstance(entry, dict):
                    px = int(entry["x"])
                    py = int(entry["y"])
                elif isinstance(entry, (list, tuple)) and len(entry) >= 2:
                    px = int(entry[0])
                    py = int(entry[1])
                else:
                    continue
            except (KeyError, TypeError, ValueError):
                continue
            if 0 <= px < width and 0 <= py < height:
                poles.add((px, py))
        return sorted(poles)

    def _parse_exit_rows(
        self, rows_entry, fallback_row: int
    ) -> Tuple[int, ...]:

        if not rows_entry:
            return (fallback_row,)
        parsed = set()
        for value in rows_entry:
            try:
                row = int(value)
            except (TypeError, ValueError):
                continue
            if self.grid_height <= 0:
                continue
            bounded = max(0, min(row, self.grid_height - 1))
            parsed.add(bounded)
        if not parsed:
            return (fallback_row,)
        return tuple(sorted(parsed))

    def _format_exit_spec(self) -> str:

        if self.exit_any_row:
            return "any"
        rows = self.exit_rows or (self.exit_row,)
        if len(rows) == 1:
            return str(rows[0])
        return "[" + ",".join(str(row) for row in rows) + "]"

    def _validate_poles_against_cars(self) -> None:

        if not self.poles:
            return
        occupied = set()
        for car in self.cars.values():
            occupied.update(car.occupy_cells())
        for pole in self.poles:
            if pole in occupied:
                raise ValueError(
                    f"Pole at {pole} overlaps a car. Adjust the level definition."
                )

    def _cars_to_state(self) -> StateTuple:

        return tuple(
            sorted(
                (car.identifier, car.x, car.y, car.heading, car.length)
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
        angle_or_heading,
        flip_horizontal: bool = False,
        flip_vertical: bool = False,
    ) -> bool:

        import pygame

        base_texture = self._load_texture(filename)
        if base_texture is None:
            return False
        texture = base_texture
        if flip_horizontal or flip_vertical:
            texture = pygame.transform.flip(texture, flip_horizontal, flip_vertical)

        rect_width = int(rect.width)
        rect_height = int(rect.height)
        if rect_width <= 0 or rect_height <= 0:
            return False
        cell_size = max(1, min(rect_width, rect_height))
        length_estimate = max(rect_width, rect_height) / cell_size
        length = max(1, int(round(length_estimate)))
        base_surface = pygame.transform.scale(
            texture, (cell_size, cell_size * length)
        )
        angle = angle_or_heading
        if isinstance(angle_or_heading, str):
            angle = _heading_to_angle(angle_or_heading)
        if angle:
            rotated = pygame.transform.rotate(base_surface, angle)
            dest_rect = rotated.get_rect(center=rect.center)
            screen.blit(rotated, dest_rect.topleft)
        else:
            screen.blit(base_surface, rect.topleft)
        return True

    def _draw_truck(
        self,
        screen,
        animated_car: AnimatedCar,
        cell_size: int,
        offset_x: int,
        offset_y: int,
    ) -> bool:

        import pygame

        head_texture = self._load_texture("truck_red.png")
        trailer_texture = self._load_texture("trailer.png")
        if head_texture is None or trailer_texture is None:
            return False

        length = animated_car.car.length
        base_surface = pygame.Surface(
            (cell_size, cell_size * length), pygame.SRCALPHA
        )
        for index in range(length):
            texture = head_texture if index == 0 else trailer_texture
            segment = pygame.transform.scale(texture, (cell_size, cell_size))
            base_surface.blit(segment, (0, index * cell_size))

        rotated = pygame.transform.rotate(base_surface, animated_car.display_angle)
        center_x, center_y = self._car_center_pixels(
            animated_car, cell_size, offset_x, offset_y
        )
        dest_rect = rotated.get_rect(center=(center_x, center_y))
        screen.blit(rotated, dest_rect)
        return True

    def _car_center_pixels(
        self,
        animated_car: AnimatedCar,
        cell_size: int,
        offset_x: int,
        offset_y: int,
    ) -> Tuple[float, float]:

        car = animated_car.car
        center_x2, center_y2 = _center_times_two(
            car.x, car.y, car.heading, car.length
        )
        center_x = offset_x + ((center_x2 + 1) / 2.0) * cell_size
        center_y = offset_y + ((center_y2 + 1) / 2.0) * cell_size
        return center_x, center_y

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
        query = f"valid_move({prolog_state}, Car, Dir, Step, NewState)"
        moves: List[MoveTuple] = []
        for solution in self._prolog.query(query):
            step = int(solution.get("Step", 1))
            if step != 1:
                continue
            car_id = str(solution["Car"]).upper()
            direction = str(solution["Dir"]).lower()
            new_state = self._parse_prolog_state(solution["NewState"])
            moves.append((car_id, direction, new_state))
        return moves

    def _parse_prolog_state(self, state_term) -> StateTuple:

        parsed: List[Tuple[str, int, int, str, int]] = []

        def _parse_entry(entry) -> Tuple[str, int, int, str, int]:
            if hasattr(entry, "args"):
                identifier = str(entry.args[0]).upper()
                x = int(entry.args[1])
                y = int(entry.args[2])
                orientation = _normalize_heading(str(entry.args[3]))
                length = int(entry.args[4])
                return identifier, x, y, orientation, length
            if isinstance(entry, str):
                return self._parse_car_string(entry)
            return self._parse_car_string(str(entry))

        entries: Iterable = []
        if isinstance(state_term, str):
            text = state_term.strip()
            try:
                literal = ast.literal_eval(text)
            except (SyntaxError, ValueError):
                literal = [text]
            if isinstance(literal, (list, tuple)):
                entries = literal
            else:
                entries = [literal]
        elif isinstance(state_term, (list, tuple)):
            entries = state_term
        else:
            entries = [state_term]

        for car in entries:
            parsed.append(_parse_entry(car))
        parsed.sort()
        return tuple(parsed)

    def _parse_car_string(self, text: str) -> Tuple[str, int, int, str, int]:

        match = _CAR_PATTERN.match(text.strip())
        if not match:
            raise ValueError(f"Unexpected car term: {text!r}")
        identifier = match.group(1).strip().upper()
        x = int(match.group(2))
        y = int(match.group(3))
        orientation = match.group(4).strip().upper()
        length = int(match.group(5))
        return identifier, x, y, _normalize_heading(orientation), length

    def _build_occupancy_map(self, state: StateTuple) -> Dict[Tuple[int, int], str]:

        occupancy: Dict[Tuple[int, int], str] = {}
        for index, (px, py) in enumerate(self.poles):
            occupancy[(px, py)] = f"#pole{index}"
        for identifier, x, y, heading, length in state:
            for cell in self._cells_for_car(x, y, heading, length):
                occupancy[cell] = identifier
        return occupancy

    def _cells_for_car(
        self, x: int, y: int, heading: str, length: int
    ) -> Iterable[Tuple[int, int]]:

        axis = _heading_axis(heading)
        if axis == "H":
            for offset in range(length):
                yield (x + offset, y)
        else:
            for offset in range(length):
                yield (x, y + offset)

    def _within_board(self, x: int, y: int, heading: str, length: int) -> bool:

        axis = _heading_axis(heading)
        if axis == "H":
            return (
                0 <= x
                and x + length <= self.grid_width
                and 0 <= y < self.grid_height
            )
        return (
            0 <= y
            and y + length <= self.grid_height
            and 0 <= x < self.grid_width
        )

    def _apply_move(
        self,
        state: StateTuple,
        car_id: str,
        dx: int,
        dy: int,
        new_heading: Optional[str] = None,
    ) -> StateTuple:

        new_state = []
        for identifier, x, y, orientation, length in state:
            if identifier == car_id:
                heading = _normalize_heading(new_heading or orientation)
                new_state.append((identifier, x + dx, y + dy, heading, length))
            else:
                new_state.append((identifier, x, y, orientation, length))
        new_state.sort()
        return tuple(new_state)
