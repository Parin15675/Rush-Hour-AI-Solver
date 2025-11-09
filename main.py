from __future__ import annotations

import math
import random
import sys
import time
import threading
from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

import pygame

from game import Game, MoveTuple, StateTuple
from search import a_star, configure_board, heuristic, is_goal

WINDOW_SIZE = 720
FPS = 60
BUTTON_WIDTH = 180
BUTTON_HEIGHT = 45
BUTTON_MARGIN = 12
BOTTOM_PANEL_HEIGHT = 140
TOP_PANEL_HEIGHT = 55
UNDO_REDO_BUTTON_SIZE = 45
DIFFICULTY_LEVELS: Dict[str, List[str]] = {
    "Expert": [f"expert_{index:02d}.json" for index in range(13, 21)],
}


def compute_board_geometry(game: Game) -> Tuple[int, int, int, int, int]:

    max_dimension = max(1, max(game.grid_width, game.grid_height))
    cell_size = max(1, WINDOW_SIZE // max_dimension)
    board_width = cell_size * game.grid_width
    board_height = cell_size * game.grid_height
    offset_x = (WINDOW_SIZE - board_width) // 2
    offset_y = (WINDOW_SIZE - board_height) // 2
    return cell_size, offset_x, offset_y, board_width, board_height


class Particle:

    def __init__(self, x: float, y: float, color: Tuple[int, int, int]) -> None:
        self.x = x
        self.y = y
        self.vx = random.uniform(-2, 2)
        self.vy = random.uniform(-3, -1)
        self.color = color
        self.lifetime = random.uniform(0.5, 1.5)
        self.age = 0.0
        self.size = random.uniform(2, 5)

    def update(self, dt: float) -> bool:

        self.x += self.vx
        self.y += self.vy
        self.vy += 0.2
        self.age += dt
        return self.age < self.lifetime

    def draw(self, screen: pygame.Surface) -> None:

        alpha = int(255 * (1 - self.age / self.lifetime))
        size = int(self.size * (1 - self.age / self.lifetime))
        if size > 0:
            surf = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            pygame.draw.circle(surf, (*self.color, alpha), (size, size), size)
            screen.blit(surf, (int(self.x - size), int(self.y - size)))


class Button:

    click_sound_callback: Optional[Callable[[str, float], None]] = None

    def __init__(
        self,
        label: str,
        position: Tuple[int, int],
        callback: Callable[[], None],
        width: int = BUTTON_WIDTH,
        height: int = BUTTON_HEIGHT,
    ) -> None:
        self.label = label
        self.rect = pygame.Rect(position[0], position[1], width, height)
        self.callback = callback
        self.hover = False
        self.click_animation = 0.0
        self.hover_scale = 0.0
        self.width = width
        self.height = height

    def draw(self, screen: pygame.Surface, font: pygame.font.Font) -> None:

        target_hover = 1.0 if self.hover else 0.0
        self.hover_scale += (target_hover - self.hover_scale) * 0.15

        if self.click_animation > 0:
            self.click_animation -= 0.05

        base_color = 70
        hover_boost = int(30 * self.hover_scale)
        click_offset = int(20 * self.click_animation)

        bg_color = (
            base_color + hover_boost - click_offset,
            base_color + hover_boost - click_offset,
            base_color + hover_boost - click_offset,
        )

        self._draw_gradient_rect(screen, self.rect, bg_color, 12)

        if self.hover_scale > 0.1:
            glow_surf = pygame.Surface(
                (self.rect.width + 20, self.rect.height + 20), pygame.SRCALPHA
            )
            glow_alpha = int(40 * self.hover_scale)
            pygame.draw.rect(
                glow_surf,
                (100, 150, 255, glow_alpha),
                glow_surf.get_rect(),
                border_radius=15,
            )
            screen.blit(glow_surf, (self.rect.x - 10, self.rect.y - 10))

        text_color = (
            220 + int(35 * self.hover_scale),
            220 + int(35 * self.hover_scale),
            220 + int(35 * self.hover_scale),
        )

        shadow_surface = font.render(self.label, True, (0, 0, 0))
        shadow_rect = shadow_surface.get_rect(
            center=(self.rect.centerx + 2, self.rect.centery + 2)
        )
        screen.blit(shadow_surface, shadow_rect)

        text_surface = font.render(self.label, True, text_color)
        text_rect = text_surface.get_rect(center=self.rect.center)
        screen.blit(text_surface, text_rect)

    def _draw_gradient_rect(
        self,
        screen: pygame.Surface,
        rect: pygame.Rect,
        base_color: Tuple[int, int, int],
        radius: int,
    ) -> None:

        temp_surf = pygame.Surface((rect.width, rect.height), pygame.SRCALPHA)
        for i in range(rect.height):
            factor = i / rect.height
            color = tuple(int(c * (1 - factor * 0.3)) for c in base_color)
            pygame.draw.line(temp_surf, color, (0, i), (rect.width, i))

        mask_surf = pygame.Surface((rect.width, rect.height), pygame.SRCALPHA)
        pygame.draw.rect(
            mask_surf, (255, 255, 255, 255), mask_surf.get_rect(), border_radius=radius
        )

        temp_surf.blit(mask_surf, (0, 0), special_flags=pygame.BLEND_RGBA_MIN)
        screen.blit(temp_surf, rect.topleft)

    def handle_event(self, event: pygame.event.Event) -> None:

        if event.type == pygame.MOUSEMOTION:
            self.hover = self.rect.collidepoint(event.pos)
        elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
            if self.rect.collidepoint(event.pos):
                self.click_animation = 1.0
                if Button.click_sound_callback:
                    Button.click_sound_callback("click", 0.4)
                self.callback()


def main() -> None:

    pygame.init()
    screen = pygame.display.set_mode(
        (WINDOW_SIZE, TOP_PANEL_HEIGHT + WINDOW_SIZE + BOTTOM_PANEL_HEIGHT)
    )
    pygame.display.set_caption("Rush Hour AI Solver - Pixel Edition")
    clock = pygame.time.Clock()
    board_surface = pygame.Surface((WINDOW_SIZE, WINDOW_SIZE))

    pixel_fonts = ["Courier New", "Consolas", "monospace"]
    font = pygame.font.SysFont(pixel_fonts, 16)
    info_font = pygame.font.SysFont(pixel_fonts, 15)
    button_font = pygame.font.SysFont(pixel_fonts, 20, bold=True)
    arrow_font = pygame.font.SysFont(pixel_fonts, 32, bold=True)
    title_font = pygame.font.SysFont(pixel_fonts, 56, bold=True)
    subtitle_font = pygame.font.SysFont(pixel_fonts, 24, bold=True)
    puzzle_number_font = pygame.font.SysFont(pixel_fonts, 36, bold=True)

    base_dir = Path(__file__).resolve().parent
    asset_dir = base_dir / "assets"
    puzzle_dir = base_dir / "puzzles"
    logic_file = base_dir / "logic.pl"
    sounds_dir = base_dir / "sounds"

    game = Game(asset_dir=asset_dir, logic_path=logic_file)

    pygame.mixer.init()
    sound_enabled = True
    sounds: Dict[str, Optional[pygame.mixer.Sound]] = {
        "click": None,
        "move": None,
        "undo": None,
        "win": None,
    }

    if sounds_dir.exists():
        sound_files = {
            "click": "click.wav",
            "move": "move.wav",
            "undo": "undo.wav",
            "win": "win.wav",
        }
        for sound_name, filename in sound_files.items():
            sound_path = sounds_dir / filename
            if sound_path.exists():
                try:
                    sounds[sound_name] = pygame.mixer.Sound(str(sound_path))
                except pygame.error:
                    pass

    bg_music_path = sounds_dir / "bgm.wav" if sounds_dir.exists() else None
    if bg_music_path and bg_music_path.exists():
        try:
            pygame.mixer.music.load(str(bg_music_path))
            pygame.mixer.music.set_volume(0.3)
            pygame.mixer.music.play(-1)
        except pygame.error:
            pass

    def play_sound(sound_name: str, volume: float = 0.25) -> None:

        if sound_enabled and sounds.get(sound_name):
            try:
                sounds[sound_name].set_volume(volume)
                sounds[sound_name].play()
            except:
                pass

    Button.click_sound_callback = play_sound

    particles: List[Particle] = []
    background_pulse = 0.0
    success_animation = 0.0
    status_fade = 0.0

    def load_level(level_name: str) -> None:
        nonlocal selected_car_id, manual_move_count, success_hold_timer, undo_stack, redo_stack
        level_path = puzzle_dir / level_name
        game.load_level(level_path)
        configure_board(
            game.grid_width,
            game.grid_height,
            game.exit_row,
            game.exit_col,
            game.poles,
            game.exit_rows,
            game.exit_any_row,
        )
        game.last_heuristic = 0.0
        selected_car_id = None
        manual_move_count = 0
        success_hold_timer = 0.0
        undo_stack.clear()
        redo_stack.clear()

    status_message = "Ready"
    auto_play = False
    auto_play_timer = 0.0
    auto_play_delay = 0.25
    game_state = "menu"
    selected_level = "Easy"
    selected_car_id: Optional[str] = None
    manual_move_count = 0
    undo_stack: List[Tuple[StateTuple, int]] = []
    redo_stack: List[Tuple[StateTuple, int]] = []
    SUCCESS_HOLD_DURATION = 4.0
    success_hold_timer = 0.0
    solver_thread: Optional[threading.Thread] = None
    solver_running = False
    solver_result: Optional[
        Tuple[List[MoveTuple], float, int, float, StateTuple]
    ] = None
    solver_error: Optional[str] = None
    solver_spin_angle = 0.0
    solver_request_level: Optional[str] = None

    def start_solver() -> None:
        nonlocal status_message, auto_play, success_animation, status_fade, particles, success_hold_timer
        nonlocal solver_thread, solver_running, solver_result, solver_error, solver_spin_angle, solver_request_level
        if solver_running:
            status_message = "Solver already running..."
            status_fade = 1.0
            return
        if game_state != "playing":
            status_message = "Select a level to start."
            status_fade = 1.0
            return
        if success_hold_timer > 0:
            status_message = "Puzzle already complete."
            status_fade = 1.0
            return
        state = game.current_state
        if not state:
            status_message = "No level loaded."
            status_fade = 1.0
            return
        if not any(car[0].upper() == "R" for car in state):
            status_message = "Add the red car (id 'R') before running the solver."
            status_fade = 1.0
            return

        solver_error = None
        solver_result = None
        solver_spin_angle = 0.0
        auto_play = False
        status_message = "Solving puzzle..."
        status_fade = 1.0
        solver_request_level = selected_level

        state_snapshot = state

        def run_solver(state_snapshot: StateTuple) -> None:
            nonlocal solver_result, solver_running, solver_error
            try:
                start_time = time.perf_counter()
                path, cost, expanded = a_star(state_snapshot)
                elapsed_time = time.perf_counter() - start_time
                solver_result = (
                    path,
                    float(cost),
                    expanded,
                    elapsed_time,
                    state_snapshot,
                )
            except Exception as exc:
                solver_error = str(exc)
            finally:
                solver_running = False

        solver_running = True
        solver_thread = threading.Thread(
            target=run_solver, args=(state_snapshot,), daemon=True
        )
        solver_thread.start()

    def next_step() -> None:
        nonlocal status_message, auto_play, success_animation, status_fade, particles, success_hold_timer
        if game_state != "playing":
            return
        if success_hold_timer > 0:
            return
        prev_state = game.current_state
        prev_manual = manual_move_count
        moved = game.step_solution()
        if moved:
            _push_undo_snapshot(prev_state, prev_manual)
            play_sound("move", 0.4)
            return
        trigger_success("SUCCESS! Puzzle complete.")
        play_sound("win", 0.7)

        for _ in range(50):
            x = random.uniform(WINDOW_SIZE * 0.3, WINDOW_SIZE * 0.7)
            y = random.uniform(WINDOW_SIZE * 0.3, WINDOW_SIZE * 0.7)
            particles.append(Particle(x, y, (255, 215, 0)))

    def reset_level() -> None:
        nonlocal status_message, auto_play, game_state, success_animation, status_fade, selected_car_id, manual_move_count, success_hold_timer, undo_stack, redo_stack
        if game_state != "playing":
            return
        game.reset()
        status_message = "↻ Reset to initial state."
        auto_play = False
        success_animation = 0.0
        status_fade = 1.0
        selected_car_id = None
        manual_move_count = 0
        success_hold_timer = 0.0
        undo_stack.clear()
        redo_stack.clear()

    def return_to_menu() -> None:
        nonlocal status_message, auto_play, game_state, success_animation, status_fade, menu_state, selected_car_id, manual_move_count, success_hold_timer, undo_stack, redo_stack
        auto_play = False
        status_message = "Ready"
        game_state = "menu"
        menu_state = "difficulty_select"
        success_animation = 0.0
        status_fade = 0.0
        selected_car_id = None
        manual_move_count = 0
        success_hold_timer = 0.0
        undo_stack.clear()
        redo_stack.clear()
        create_difficulty_menu()

    def trigger_success(message: str = "SUCCESS! Puzzle complete.") -> None:
        nonlocal status_message, success_animation, status_fade, success_hold_timer, auto_play, selected_car_id, auto_play_timer
        status_message = message
        status_fade = 1.0
        success_animation = 1.0
        success_hold_timer = SUCCESS_HOLD_DURATION
        auto_play = False
        auto_play_timer = 0.0
        selected_car_id = None

    def _apply_state(state: StateTuple, manual_value: int) -> None:
        nonlocal manual_move_count, selected_car_id, auto_play, auto_play_timer, success_hold_timer, success_animation
        if not state:
            return
        game.current_state = state
        game._update_cars_from_state(state)
        for animated in game.animated_cars.values():
            animated.display_x = animated.target_x
            animated.display_y = animated.target_y
        game.solution_path = []
        game.solution_index = 0
        game.nodes_expanded = 0
        game.last_heuristic = heuristic(state)
        manual_move_count = manual_value
        selected_car_id = None
        auto_play = False
        auto_play_timer = 0.0
        success_hold_timer = 0.0
        success_animation = 0.0

    def _push_undo_snapshot(prev_state: Optional[StateTuple], prev_manual: int) -> None:
        if prev_state is None:
            return
        undo_stack.append((prev_state, prev_manual))
        redo_stack.clear()

    def undo_move() -> None:
        nonlocal status_message, status_fade
        if not undo_stack:
            status_message = "Nothing to undo."
            status_fade = 1.0
            return
        if game.current_state:
            redo_stack.append((game.current_state, manual_move_count))
        state, manual_value = undo_stack.pop()
        _apply_state(state, manual_value)
        status_message = "Undid last move."
        status_fade = 1.0
        play_sound("undo", 0.3)

    def redo_move() -> None:
        nonlocal status_message, status_fade
        if not redo_stack:
            status_message = "Nothing to redo."
            status_fade = 1.0
            return
        if game.current_state:
            undo_stack.append((game.current_state, manual_move_count))
        state, manual_value = redo_stack.pop()
        _apply_state(state, manual_value)
        play_sound("undo", 0.3)
        status_message = "Redid move."
        status_fade = 1.0

    buttons: List[Button] = []

    undo_redo_buttons: List[Button] = []
    undo_button = Button(
        "←",
        (BUTTON_MARGIN, 5),
        undo_move,
        width=UNDO_REDO_BUTTON_SIZE,
        height=UNDO_REDO_BUTTON_SIZE,
    )
    redo_button = Button(
        "→",
        (BUTTON_MARGIN * 2 + UNDO_REDO_BUTTON_SIZE, 5),
        redo_move,
        width=UNDO_REDO_BUTTON_SIZE,
        height=UNDO_REDO_BUTTON_SIZE,
    )
    undo_redo_buttons.append(undo_button)
    undo_redo_buttons.append(redo_button)

    row_width = 2 * BUTTON_WIDTH + BUTTON_MARGIN
    row_start_x = (WINDOW_SIZE - row_width) // 2
    y_row1 = TOP_PANEL_HEIGHT + WINDOW_SIZE + BUTTON_MARGIN
    y_row2 = y_row1 + BUTTON_HEIGHT + BUTTON_MARGIN

    buttons.append(Button("Start Solver", (row_start_x, y_row1), start_solver))
    buttons.append(
        Button(
            "Reset", (row_start_x + BUTTON_WIDTH + BUTTON_MARGIN, y_row1), reset_level
        )
    )

    menu_button_width = row_width
    buttons.append(
        Button("Menu", (row_start_x, y_row2), return_to_menu, width=menu_button_width)
    )

    menu_buttons = []
    menu_state = "difficulty_select"
    selected_difficulty = None

    def create_difficulty_menu():

        nonlocal menu_buttons, menu_state, selected_difficulty
        menu_buttons = []

        if len(DIFFICULTY_LEVELS) == 1:
            selected_difficulty = next(iter(DIFFICULTY_LEVELS))
            menu_state = "puzzle_select"
            create_puzzle_menu(selected_difficulty)
            return

        for index, difficulty in enumerate(DIFFICULTY_LEVELS.keys()):
            x_offset = (WINDOW_SIZE - BUTTON_WIDTH) // 2
            y_offset = 260 + index * (BUTTON_HEIGHT + 20)

            def make_callback(diff: str) -> Callable[[], None]:
                def _callback() -> None:
                    nonlocal menu_state, selected_difficulty
                    selected_difficulty = diff
                    menu_state = "puzzle_select"
                    create_puzzle_menu(diff)

                return _callback

            menu_buttons.append(
                Button(difficulty, (x_offset, y_offset), make_callback(difficulty))
            )

    def create_puzzle_menu(difficulty: str):

        nonlocal menu_buttons
        menu_buttons = []
        puzzles = DIFFICULTY_LEVELS[difficulty]

        max_cols = 4
        button_width = 85
        button_height = 85
        button_spacing = 20
        start_y = 280

        for index, puzzle_file in enumerate(puzzles):
            puzzle_num = index + 1
            label = f"{puzzle_num}"

            row = index // max_cols
            col = index % max_cols

            grid_width = max_cols * button_width + (max_cols - 1) * button_spacing
            row_start_x = (WINDOW_SIZE - grid_width) // 2

            x_offset = int(row_start_x + col * (button_width + button_spacing))
            y_offset = int(start_y + row * (button_height + button_spacing))

            def make_callback(
                level_file: str, diff: str, num: int
            ) -> Callable[[], None]:
                def _callback() -> None:
                    nonlocal game_state, status_message, selected_level, auto_play
                    load_level(level_file)
                    game_state = "playing"
                    selected_level = f"{diff} #{num}"
                    status_message = f"{diff} Puzzle {num} loaded."
                    auto_play = False

                return _callback

            menu_buttons.append(
                Button(
                    label,
                    (x_offset, y_offset),
                    make_callback(puzzle_file, difficulty, puzzle_num),
                    width=button_width,
                    height=button_height,
                )
            )

        num_rows = (len(puzzles) + max_cols - 1) // max_cols
        back_btn_y = int(start_y + num_rows * (button_height + button_spacing) + 30)
        back_btn_width = 140
        back_btn_x = (WINDOW_SIZE - back_btn_width) // 2

        def go_back() -> None:
            nonlocal menu_state
            menu_state = "difficulty_select"
            create_difficulty_menu()

        # menu_buttons.append(
        #     Button(
        #         "< Back",
        #         (back_btn_x, back_btn_y),
        #         go_back,
        #         width=back_btn_width,
        #         height=BUTTON_HEIGHT,
        #     )
        # )

    create_difficulty_menu()

    while True:
        dt = clock.tick(FPS) / 1000.0

        background_pulse += dt
        if success_hold_timer > 0:
            success_animation = min(1.0, max(success_animation, 0.9))
        elif success_animation > 0:
            success_animation = max(0, success_animation - dt * 0.5)
        if status_fade > 0:
            status_fade = max(0, status_fade - dt * 0.5)
        if solver_running:
            solver_spin_angle = (solver_spin_angle + dt * 6.0) % (math.tau if hasattr(math, "tau") else 2 * math.pi)
            status_fade = 1.0
        else:
            solver_spin_angle = 0.0

        particles = [p for p in particles if p.update(dt)]
        (
            cell_size,
            board_offset_x,
            board_offset_y,
            board_width_px,
            board_height_px,
        ) = compute_board_geometry(game)

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit(0)
            if game_state == "menu":
                for button in menu_buttons:
                    button.handle_event(event)
            else:
                if solver_running:
                    if event.type in (pygame.MOUSEBUTTONDOWN, pygame.KEYDOWN):
                        status_message = "Solver running... please wait."
                        status_fade = 1.0
                    continue
                if success_hold_timer <= 0:
                    if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                        mouse_x, mouse_y = event.pos
                        if TOP_PANEL_HEIGHT < mouse_y < TOP_PANEL_HEIGHT + WINDOW_SIZE:
                            local_x = mouse_x
                            local_y = mouse_y - TOP_PANEL_HEIGHT
                            within_board = (
                                board_offset_x
                                <= local_x
                                < board_offset_x + board_width_px
                            ) and (
                                board_offset_y
                                <= local_y
                                < board_offset_y + board_height_px
                            )
                            if not within_board:
                                selected_car_id = None
                                continue
                            grid_x = int((local_x - board_offset_x) // cell_size)
                            grid_y = int((local_y - board_offset_y) // cell_size)
                            newly_selected: Optional[str] = None
                            for car_id, car in game.cars.items():
                                if any(
                                    cx == grid_x and cy == grid_y
                                    for cx, cy in car.occupy_cells()
                                ):
                                    newly_selected = car_id
                                    break
                            if newly_selected:
                                selected_car_id = newly_selected
                                status_message = (
                                    f"Car {newly_selected.upper()} selected."
                                )
                                status_fade = 1.0
                                auto_play = False
                            else:
                                selected_car_id = None
                    elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 3:
                        selected_car_id = None
                    elif event.type == pygame.KEYDOWN:
                        if event.key == pygame.K_z and (event.mod & pygame.KMOD_CTRL):
                            undo_move()
                        elif event.key == pygame.K_y and (event.mod & pygame.KMOD_CTRL):
                            redo_move()
                        elif event.key == pygame.K_ESCAPE:
                            selected_car_id = None
                        elif selected_car_id:
                            car = game.cars.get(selected_car_id)
                            if car:
                                direction: Optional[str] = None
                                if event.key == pygame.K_q:
                                    direction = "rotate_ccw"
                                elif event.key == pygame.K_e:
                                    direction = "rotate_cw"
                                elif car.orientation == "H":
                                    if event.key == pygame.K_LEFT:
                                        direction = "left"
                                    elif event.key == pygame.K_RIGHT:
                                        direction = "right"
                                else:
                                    if event.key == pygame.K_UP:
                                        direction = "up"
                                    elif event.key == pygame.K_DOWN:
                                        direction = "down"
                                if direction:
                                    auto_play = False
                                    prev_state = game.current_state
                                    prev_manual = manual_move_count
                                    moved = game.move_car(selected_car_id, direction)
                                    if moved:
                                        _push_undo_snapshot(prev_state, prev_manual)
                                        manual_move_count = prev_manual + 1
                                        game.solution_path = []
                                        game.solution_index = 0
                                        game.nodes_expanded = 0
                                        solved = False
                                        if game.current_state:
                                            game.last_heuristic = heuristic(
                                                game.current_state
                                            )
                                            solved = is_goal(game.current_state)
                                        if solved:
                                            trigger_success(
                                                f"SUCCESS! Cleared in {manual_move_count} moves."
                                            )
                                            play_sound("win", 0.7)
                                            for _ in range(60):
                                                x = random.uniform(
                                                    WINDOW_SIZE * 0.25,
                                                    WINDOW_SIZE * 0.75,
                                                )
                                                y = random.uniform(
                                                    WINDOW_SIZE * 0.25,
                                                    WINDOW_SIZE * 0.75,
                                                )
                                                particles.append(
                                                    Particle(x, y, (255, 215, 0))
                                                )
                                        else:
                                            if direction and direction.startswith("rotate"):
                                                pretty = "CW" if direction.endswith("cw") else "CCW"
                                                status_message = (
                                                    f"Rotated {selected_car_id.upper()} {pretty}."
                                                )
                                            else:
                                                status_message = (
                                                    f"Moved {selected_car_id.upper()} {direction}."
                                                )
                                            status_fade = 1.0
                                            play_sound("move", 0.4)
                                    else:
                                        status_message = "Move blocked."
                                        status_fade = 1.0
                for button in buttons:
                    button.handle_event(event)
                for button in undo_redo_buttons:
                    button.handle_event(event)

        if not solver_running:
            if solver_error:
                status_message = f"Solver error: {solver_error}"
                status_fade = 1.0
                solver_error = None
                solver_request_level = None
            elif solver_result is not None:
                path, cost, expanded, elapsed_time, initial_state = solver_result
                solver_result = None
                previous_level = solver_request_level
                solver_request_level = None
                if game_state != "playing" or previous_level != selected_level:
                    status_message = "Solver finished, but puzzle changed."
                    status_fade = 1.0
                elif path:
                    if game.current_state != initial_state:
                        status_message = "Solver finished, but puzzle changed."
                        status_fade = 1.0
                    else:
                        if math.isfinite(cost):
                            cost_text = (
                                str(int(cost))
                                if float(cost).is_integer()
                                else f"{cost:.2f}"
                            )
                        else:
                            cost_text = "∞"
                        status_message = (
                            f"✓ Solution found: {cost_text} moves in {elapsed_time:.2f}s"
                        )
                        print("success")
                        game.apply_solution(path)
                        game.nodes_expanded = expanded
                        game.last_heuristic = heuristic(initial_state)
                        auto_play = True
                        auto_play_timer = 0.0
                        status_fade = 1.0

                        for _ in range(30):
                            particles.append(
                                Particle(
                                    WINDOW_SIZE // 2,
                                    WINDOW_SIZE // 2,
                                    (100, 200, 255),
                                )
                            )
                else:
                    status_message = "✗ No solution found."
                    status_fade = 1.0

        animations_active = False
        if game_state == "playing":
            animations_active = game.update_animations()

        if auto_play and game_state == "playing":
            if animations_active:
                auto_play_timer = 0.0
            else:
                auto_play_timer += dt
                if auto_play_timer >= auto_play_delay:
                    auto_play_timer = 0.0
                    prev_state = game.current_state
                    prev_manual = manual_move_count
                    moved = game.step_solution()
                    if moved:
                        _push_undo_snapshot(prev_state, prev_manual)
                        play_sound("move", 0.3)
                    else:
                        trigger_success("SUCCESS! Puzzle complete.")
                        play_sound("win", 0.7)

                        for _ in range(40):
                            x = random.uniform(WINDOW_SIZE * 0.2, WINDOW_SIZE * 0.8)
                            y = random.uniform(WINDOW_SIZE * 0.2, WINDOW_SIZE * 0.8)
                            particles.append(Particle(x, y, (255, 215, 0)))

        if success_hold_timer > 0:
            success_hold_timer = max(0.0, success_hold_timer - dt)
            if success_hold_timer <= 0 and game_state == "playing":
                return_to_menu()
                continue

        if game_state == "menu":

            for y in range(screen.get_height()):
                factor = y / screen.get_height()
                pulse = math.sin(background_pulse * 2) * 0.1
                r = int(20 + 15 * factor + pulse * 10)
                g = int(20 + 25 * factor + pulse * 15)
                b = int(40 + 30 * factor + pulse * 20)
                pygame.draw.line(screen, (r, g, b), (0, y), (screen.get_width(), y))

            title_text = title_font.render("Rush Hour AI", True, (255, 255, 255))
            title_rect = title_text.get_rect(center=(WINDOW_SIZE // 2, 120))

            glow_size = int(10 + 5 * math.sin(background_pulse * 3))
            for offset in range(glow_size, 0, -2):
                alpha = int(30 * (1 - offset / glow_size))
                glow_surf = pygame.Surface(
                    (title_rect.width + offset * 2, title_rect.height + offset * 2),
                    pygame.SRCALPHA,
                )
                glow_text = title_font.render(
                    "Rush Hour AI", True, (100, 150, 255, alpha)
                )
                glow_rect = glow_text.get_rect(
                    center=(glow_surf.get_width() // 2, glow_surf.get_height() // 2)
                )
                glow_surf.blit(glow_text, glow_rect)
                screen.blit(glow_surf, (title_rect.x - offset, title_rect.y - offset))

            screen.blit(title_text, title_rect)

            if menu_state == "difficulty_select":
                subtitle_str = "Select Difficulty Level"
            else:
                subtitle_str = f"{selected_difficulty} - Select Puzzle"
            subtitle_text = subtitle_font.render(subtitle_str, True, (200, 220, 240))
            subtitle_rect = subtitle_text.get_rect(center=(WINDOW_SIZE // 2, 190))
            screen.blit(subtitle_text, subtitle_rect)

            line_y = 230
            pygame.draw.line(
                screen,
                (80, 100, 140),
                (WINDOW_SIZE // 2 - 100, line_y),
                (WINDOW_SIZE // 2 + 100, line_y),
                2,
            )

            for button in menu_buttons:

                if button.width == button.height and button.width == 85:
                    button.draw(screen, puzzle_number_font)
                else:
                    button.draw(screen, button_font)
            pygame.display.flip()
            continue
        else:

            top_panel_surf = pygame.Surface(
                (WINDOW_SIZE, TOP_PANEL_HEIGHT), pygame.SRCALPHA
            )
            for y in range(TOP_PANEL_HEIGHT):
                factor = y / TOP_PANEL_HEIGHT
                color = (
                    int(25 * (1 - factor * 0.2)),
                    int(25 * (1 - factor * 0.2)),
                    int(30 * (1 - factor * 0.2)),
                    255,
                )
                pygame.draw.line(top_panel_surf, color, (0, y), (WINDOW_SIZE, y))
            screen.blit(top_panel_surf, (0, 0))

            for button in undo_redo_buttons:
                button.draw(screen, arrow_font)

            level_text = font.render(f"Level: {selected_level}", True, (220, 220, 220))
            level_rect = level_text.get_rect()
            level_rect.right = WINDOW_SIZE - 15
            level_rect.centery = TOP_PANEL_HEIGHT // 2
            screen.blit(level_text, level_rect)

            board_surface.fill((0, 0, 0))
            game.draw(board_surface)

            if selected_car_id and selected_car_id in game.animated_cars:
                animated = game.animated_cars[selected_car_id]
                car = animated.car

                selected_id_upper = selected_car_id.upper()
                if success_hold_timer <= 0 and not auto_play and game.current_state:
                    seen_positions = set()
                    current_x = car.x
                    current_y = car.y
                    try:
                        moves = game.get_valid_moves(game.current_state)
                    except Exception:
                        moves = []
                    for move_car_id, _direction, next_state in moves:
                        if move_car_id != selected_car_id:
                            continue
                        for state_car in next_state:
                            if state_car[0].upper() == selected_id_upper:
                                new_x, new_y = state_car[1], state_car[2]
                                heading = state_car[3].upper()
                                axis = "H" if heading in ("H", "E", "W") else "V"
                                length = state_car[4]
                                dx = new_x - current_x
                                dy = new_y - current_y
                                if dx == 0 and dy == 0:
                                    break
                                if axis == "H":
                                    dest_x_cell = (
                                        new_x if dx < 0 else new_x + length - 1
                                    )
                                    dest_y_cell = new_y
                                else:
                                    dest_x_cell = new_x
                                    dest_y_cell = (
                                        new_y if dy < 0 else new_y + length - 1
                                    )
                                key = (dest_x_cell, dest_y_cell)
                                if key in seen_positions:
                                    break
                                seen_positions.add(key)
                                dest_rect = pygame.Rect(
                                    board_offset_x + dest_x_cell * cell_size,
                                    board_offset_y + dest_y_cell * cell_size,
                                    cell_size,
                                    cell_size,
                                )
                                pad = max(4, cell_size // 4)
                                dest_rect = dest_rect.inflate(-pad, -pad)
                                highlight_move = pygame.Surface(
                                    (dest_rect.width, dest_rect.height), pygame.SRCALPHA
                                )
                                pygame.draw.rect(
                                    highlight_move,
                                    (90, 255, 190, 120),
                                    highlight_move.get_rect(),
                                    border_radius=8,
                                )
                                pygame.draw.rect(
                                    highlight_move,
                                    (50, 200, 150, 220),
                                    highlight_move.get_rect(),
                                    width=2,
                                    border_radius=8,
                                )
                                board_surface.blit(highlight_move, dest_rect.topleft)
                                break

                width = (
                    int(cell_size * car.length) if car.orientation == "H" else cell_size
                )
                height = (
                    cell_size if car.orientation == "H" else int(cell_size * car.length)
                )
                highlight_rect = pygame.Rect(
                    board_offset_x + int(animated.display_x * cell_size),
                    board_offset_y + int(animated.display_y * cell_size),
                    width,
                    height,
                )
                highlight = pygame.Surface(
                    (highlight_rect.width, highlight_rect.height), pygame.SRCALPHA
                )
                pygame.draw.rect(
                    highlight,
                    (120, 200, 255, 70),
                    highlight.get_rect(),
                    border_radius=12,
                )
                pygame.draw.rect(
                    highlight,
                    (120, 200, 255, 200),
                    highlight.get_rect(),
                    width=3,
                    border_radius=12,
                )
                board_surface.blit(highlight, highlight_rect.topleft)

            if success_animation > 0:
                overlay = pygame.Surface((WINDOW_SIZE, WINDOW_SIZE), pygame.SRCALPHA)
                alpha = int(
                    100 * success_animation * math.sin(success_animation * math.pi)
                )
                pygame.draw.rect(overlay, (255, 215, 0, alpha), overlay.get_rect())
                board_surface.blit(overlay, (0, 0))
                if success_hold_timer > 0:
                    success_text = title_font.render("SUCCESS", True, (255, 255, 255))
                    success_rect = success_text.get_rect(
                        center=(WINDOW_SIZE // 2, WINDOW_SIZE // 2 - 20)
                    )
                    board_surface.blit(success_text, success_rect)
                    detail_text = subtitle_font.render(
                        status_message, True, (255, 255, 255)
                    )
                    detail_rect = detail_text.get_rect(
                        center=(WINDOW_SIZE // 2, WINDOW_SIZE // 2 + 20)
                    )
                    board_surface.blit(detail_text, detail_rect)

            screen.blit(board_surface, (0, TOP_PANEL_HEIGHT))

        for particle in particles:
            particle.draw(screen)

        panel_height = BOTTOM_PANEL_HEIGHT
        panel_surf = pygame.Surface((WINDOW_SIZE, panel_height), pygame.SRCALPHA)
        for y in range(panel_height):
            factor = y / panel_height
            color = (
                int(20 * (1 - factor * 0.3)),
                int(20 * (1 - factor * 0.3)),
                int(20 * (1 - factor * 0.3)),
                255,
            )
            pygame.draw.line(panel_surf, color, (0, y), (WINDOW_SIZE, y))
        screen.blit(panel_surf, (0, TOP_PANEL_HEIGHT + WINDOW_SIZE))

        moves_y = (
            TOP_PANEL_HEIGHT + WINDOW_SIZE + BUTTON_MARGIN + (BUTTON_HEIGHT // 2) - 8
        )

        ai_moves_text = info_font.render(
            f"🎯 AI Moves: {game.solution_index}/{len(game.solution_path)}",
            True,
            (150, 200, 255),
        )
        screen.blit(ai_moves_text, (20, moves_y))

        manual_moves_text = info_font.render(
            f"🕹️ Manual: {manual_move_count}", True, (255, 210, 120)
        )
        screen.blit(manual_moves_text, (20, moves_y + BUTTON_HEIGHT + BUTTON_MARGIN))

        status_y = (
            TOP_PANEL_HEIGHT
            + WINDOW_SIZE
            + BUTTON_MARGIN
            + 2 * (BUTTON_HEIGHT + BUTTON_MARGIN)
            + 8
        )
        status_x = 20

        if status_fade > 0:
            status_width = min(680, WINDOW_SIZE - 40)
            status_bg = pygame.Surface((status_width, 28), pygame.SRCALPHA)
            bg_alpha = int(120 * status_fade)
            pygame.draw.rect(
                status_bg, (40, 40, 40, bg_alpha), status_bg.get_rect(), border_radius=8
            )
            screen.blit(status_bg, (status_x - 5, status_y))

            status_color = (
                200 + int(55 * status_fade),
                200 + int(55 * status_fade),
                200 + int(55 * status_fade),
            )
            spinner_offset = 0
            if solver_running:
                spinner_size = 22
                spinner_surf = pygame.Surface(
                    (spinner_size, spinner_size), pygame.SRCALPHA
                )
                spinner_center = spinner_size / 2
                segments = 12
                for index in range(segments):
                    angle = solver_spin_angle + (index / segments) * 2 * math.pi
                    fade = (index + 1) / segments
                    color = (100, int(160 + 60 * fade), 255, int(200 * fade))
                    end_x = spinner_center + math.cos(angle) * (spinner_size / 2 - 2)
                    end_y = spinner_center + math.sin(angle) * (spinner_size / 2 - 2)
                    pygame.draw.line(
                        spinner_surf,
                        color,
                        (
                            int(round(spinner_center)),
                            int(round(spinner_center)),
                        ),
                        (
                            int(round(end_x)),
                            int(round(end_y)),
                        ),
                        2,
                    )
                screen.blit(spinner_surf, (status_x, status_y + 4))
                spinner_offset = spinner_size + 8

            status_surf = info_font.render(status_message, True, status_color)
            screen.blit(status_surf, (status_x + spinner_offset, status_y + 6))

        for button in buttons:
            button.draw(screen, button_font)

        pygame.display.flip()


if __name__ == "__main__":
    main()
