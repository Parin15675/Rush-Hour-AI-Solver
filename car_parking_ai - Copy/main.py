"""Pygame front-end for the AI Car Parking Puzzle Solver."""

from __future__ import annotations

import math
import random
import sys
import time
from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

import pygame

from game import Game
from search import a_star, heuristic, set_move_provider

WINDOW_SIZE = 720
FPS = 60
BUTTON_WIDTH = 180
BUTTON_HEIGHT = 48
BUTTON_MARGIN = 16
DIFFICULTY_LEVELS: Dict[str, List[str]] = {
    "Beginner": ["beginner_01.json", "beginner_02.json", "beginner_03.json"],
    "Intermediate": ["intermediate_01.json", "intermediate_02.json", "intermediate_03.json"],
    "Advanced": ["advanced_01.json", "advanced_02.json", "advanced_03.json"],
    "Expert": ["expert_01.json", "expert_02.json", "expert_03.json"],
}


class Particle:
    """Particle effect for visual polish."""

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
        """Update particle position and age. Returns False when expired."""
        self.x += self.vx
        self.y += self.vy
        self.vy += 0.2  # gravity
        self.age += dt
        return self.age < self.lifetime

    def draw(self, screen: pygame.Surface) -> None:
        """Render the particle with alpha fade."""
        alpha = int(255 * (1 - self.age / self.lifetime))
        size = int(self.size * (1 - self.age / self.lifetime))
        if size > 0:
            surf = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            pygame.draw.circle(surf, (*self.color, alpha), (size, size), size)
            screen.blit(surf, (int(self.x - size), int(self.y - size)))


class Button:
    """Modern interactive button widget with animations."""

    def __init__(self, label: str, position: Tuple[int, int], callback: Callable[[], None]) -> None:
        self.label = label
        self.rect = pygame.Rect(position[0], position[1], BUTTON_WIDTH, BUTTON_HEIGHT)
        self.callback = callback
        self.hover = False
        self.click_animation = 0.0
        self.hover_scale = 0.0

    def draw(self, screen: pygame.Surface, font: pygame.font.Font) -> None:
        """Render the button with smooth hover and click animations."""
        # Smooth hover transition
        target_hover = 1.0 if self.hover else 0.0
        self.hover_scale += (target_hover - self.hover_scale) * 0.15

        # Click animation decay
        if self.click_animation > 0:
            self.click_animation -= 0.05

        # Calculate animated colors
        base_color = 70
        hover_boost = int(30 * self.hover_scale)
        click_offset = int(20 * self.click_animation)

        bg_color = (base_color + hover_boost - click_offset,
                    base_color + hover_boost - click_offset,
                    base_color + hover_boost - click_offset)

        # Gradient background
        self._draw_gradient_rect(screen, self.rect, bg_color, 12)

        # Glow effect on hover
        if self.hover_scale > 0.1:
            glow_surf = pygame.Surface((self.rect.width + 20, self.rect.height + 20), pygame.SRCALPHA)
            glow_alpha = int(40 * self.hover_scale)
            pygame.draw.rect(glow_surf, (100, 150, 255, glow_alpha),
                           glow_surf.get_rect(), border_radius=15)
            screen.blit(glow_surf, (self.rect.x - 10, self.rect.y - 10))

        # Text with subtle shadow
        text_color = (220 + int(35 * self.hover_scale),
                     220 + int(35 * self.hover_scale),
                     220 + int(35 * self.hover_scale))

        # Shadow
        shadow_surface = font.render(self.label, True, (0, 0, 0))
        shadow_rect = shadow_surface.get_rect(center=(self.rect.centerx + 2, self.rect.centery + 2))
        screen.blit(shadow_surface, shadow_rect)

        # Main text
        text_surface = font.render(self.label, True, text_color)
        text_rect = text_surface.get_rect(center=self.rect.center)
        screen.blit(text_surface, text_rect)

    def _draw_gradient_rect(self, screen: pygame.Surface, rect: pygame.Rect,
                           base_color: Tuple[int, int, int], radius: int) -> None:
        """Draw a rectangle with vertical gradient."""
        temp_surf = pygame.Surface((rect.width, rect.height), pygame.SRCALPHA)
        for i in range(rect.height):
            factor = i / rect.height
            color = tuple(int(c * (1 - factor * 0.3)) for c in base_color)
            pygame.draw.line(temp_surf, color, (0, i), (rect.width, i))

        # Create rounded mask
        mask_surf = pygame.Surface((rect.width, rect.height), pygame.SRCALPHA)
        pygame.draw.rect(mask_surf, (255, 255, 255, 255), mask_surf.get_rect(), border_radius=radius)

        temp_surf.blit(mask_surf, (0, 0), special_flags=pygame.BLEND_RGBA_MIN)
        screen.blit(temp_surf, rect.topleft)

    def handle_event(self, event: pygame.event.Event) -> None:
        """Update hover state and trigger callbacks on click."""
        if event.type == pygame.MOUSEMOTION:
            self.hover = self.rect.collidepoint(event.pos)
        elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
            if self.rect.collidepoint(event.pos):
                self.click_animation = 1.0
                self.callback()


def main() -> None:
    """Entrypoint that prepares pygame, loads a level, and runs the loop."""
    pygame.init()
    screen = pygame.display.set_mode((WINDOW_SIZE, WINDOW_SIZE + BUTTON_HEIGHT + BUTTON_MARGIN * 2))
    pygame.display.set_caption("Rush Hour AI Solver - Modern Edition")
    clock = pygame.time.Clock()
    font = pygame.font.SysFont("segoeui", 18)
    button_font = pygame.font.SysFont("segoeui", 20, bold=True)
    title_font = pygame.font.SysFont("segoeui", 56, bold=True)
    subtitle_font = pygame.font.SysFont("segoeui", 24)

    base_dir = Path(__file__).resolve().parent
    asset_dir = base_dir / "assets"
    puzzle_dir = base_dir / "puzzles"
    logic_file = base_dir / "logic.pl"

    game = Game(asset_dir=asset_dir, logic_path=logic_file)

    # Visual effects
    particles: List[Particle] = []
    background_pulse = 0.0
    success_animation = 0.0
    status_fade = 0.0

    def load_level(level_name: str) -> None:
        nonlocal selected_car_id, manual_move_count
        level_path = puzzle_dir / level_name
        game.load_level(level_path)
        set_move_provider(game.get_valid_moves)
        game.last_heuristic = 0.0
        selected_car_id = None
        manual_move_count = 0

    status_message = "Ready"
    auto_play = False
    auto_play_timer = 0.0
    auto_play_delay = 0.4  # seconds between animated steps (faster for modern feel)
    game_state = "menu"  # menu or playing
    selected_level = "Easy"
    selected_car_id: Optional[str] = None
    manual_move_count = 0

    def start_solver() -> None:
        nonlocal status_message, auto_play, success_animation, status_fade, particles
        if game_state != "playing":
            status_message = "Select a level to start."
            status_fade = 1.0
            return
        state = game.current_state
        if not state:
            status_message = "No level loaded."
            status_fade = 1.0
            return
        start = time.perf_counter()
        path, cost, expanded = a_star(state, game.get_valid_moves)
        elapsed = time.perf_counter() - start
        if path:
            status_message = f"✓ Solution found: {cost} moves in {elapsed:.2f}s"
            print("success")
            game.apply_solution(path)
            game.nodes_expanded = expanded
            game.last_heuristic = heuristic(state)
            auto_play = True
            status_fade = 1.0
            # Spawn celebration particles
            for _ in range(30):
                particles.append(Particle(WINDOW_SIZE // 2, WINDOW_SIZE // 2, (100, 200, 255)))
        else:
            status_message = "✗ No solution found."
            status_fade = 1.0

    def next_step() -> None:
        nonlocal status_message, auto_play, success_animation, status_fade, particles
        if game_state != "playing":
            return
        if not game.step_solution():
            status_message = "✓ Solution complete!"
            auto_play = False
            success_animation = 1.0
            status_fade = 1.0
            # Victory particles
            for _ in range(50):
                x = random.uniform(WINDOW_SIZE * 0.3, WINDOW_SIZE * 0.7)
                y = random.uniform(WINDOW_SIZE * 0.3, WINDOW_SIZE * 0.7)
                particles.append(Particle(x, y, (255, 215, 0)))

    def reset_level() -> None:
        nonlocal status_message, auto_play, game_state, success_animation, status_fade, selected_car_id, manual_move_count
        if game_state != "playing":
            return
        game.reset()
        status_message = "↻ Reset to initial state."
        auto_play = False
        success_animation = 0.0
        status_fade = 1.0
        selected_car_id = None
        manual_move_count = 0

    def return_to_menu() -> None:
        nonlocal status_message, auto_play, game_state, success_animation, status_fade, menu_state, selected_car_id, manual_move_count
        auto_play = False
        status_message = "Ready"
        game_state = "menu"
        menu_state = "difficulty_select"
        success_animation = 0.0
        status_fade = 0.0
        selected_car_id = None
        manual_move_count = 0
        create_difficulty_menu()

    buttons = [
        Button("Start Solver", (BUTTON_MARGIN, WINDOW_SIZE + BUTTON_MARGIN), start_solver),
        Button("Next Step", (BUTTON_MARGIN * 2 + BUTTON_WIDTH, WINDOW_SIZE + BUTTON_MARGIN), next_step),
        Button("Reset", (BUTTON_MARGIN * 3 + BUTTON_WIDTH * 2, WINDOW_SIZE + BUTTON_MARGIN), reset_level),
        Button("Menu", (BUTTON_MARGIN * 4 + BUTTON_WIDTH * 3, WINDOW_SIZE + BUTTON_MARGIN), return_to_menu),
    ]

    # Create menu buttons for each difficulty level
    menu_buttons = []
    menu_state = "difficulty_select"  # difficulty_select or puzzle_select
    selected_difficulty = None

    def create_difficulty_menu():
        """Create buttons for difficulty selection."""
        nonlocal menu_buttons
        menu_buttons = []
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

            menu_buttons.append(Button(difficulty, (x_offset, y_offset), make_callback(difficulty)))

    def create_puzzle_menu(difficulty: str):
        """Create buttons for puzzle selection within a difficulty."""
        nonlocal menu_buttons
        menu_buttons = []
        puzzles = DIFFICULTY_LEVELS[difficulty]

        for index, puzzle_file in enumerate(puzzles):
            puzzle_num = index + 1
            label = f"Puzzle {puzzle_num}"
            x_offset = (WINDOW_SIZE - BUTTON_WIDTH) // 2
            y_offset = 260 + index * (BUTTON_HEIGHT + 20)

            def make_callback(level_file: str, diff: str, num: int) -> Callable[[], None]:
                def _callback() -> None:
                    nonlocal game_state, status_message, selected_level, auto_play
                    load_level(level_file)
                    game_state = "playing"
                    selected_level = f"{diff} #{num}"
                    status_message = f"{diff} Puzzle {num} loaded."
                    auto_play = False
                return _callback

            menu_buttons.append(Button(label, (x_offset, y_offset), make_callback(puzzle_file, difficulty, puzzle_num)))

        # Add back button
        back_btn_y = 260 + len(puzzles) * (BUTTON_HEIGHT + 20) + 20
        def go_back():
            nonlocal menu_state
            menu_state = "difficulty_select"
            create_difficulty_menu()
        menu_buttons.append(Button("< Back", ((WINDOW_SIZE - BUTTON_WIDTH) // 2, back_btn_y), go_back))

    create_difficulty_menu()

    while True:
        dt = clock.tick(FPS) / 1000.0

        # Update animations
        background_pulse += dt
        if success_animation > 0:
            success_animation = max(0, success_animation - dt * 0.5)
        if status_fade > 0:
            status_fade = max(0, status_fade - dt * 0.5)

        # Update particles
        particles = [p for p in particles if p.update(dt)]

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit(0)
            if game_state == "menu":
                for button in menu_buttons:
                    button.handle_event(event)
            else:
                if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                    mouse_x, mouse_y = event.pos
                    if mouse_y < WINDOW_SIZE:
                        cell_size = WINDOW_SIZE // game.grid_size
                        grid_x = mouse_x // cell_size
                        grid_y = mouse_y // cell_size
                        newly_selected: Optional[str] = None
                        for car_id, car in game.cars.items():
                            if any(cx == grid_x and cy == grid_y for cx, cy in car.occupy_cells()):
                                newly_selected = car_id
                                break
                        if newly_selected:
                            selected_car_id = newly_selected
                            status_message = f"Car {newly_selected.upper()} selected."
                            status_fade = 1.0
                            auto_play = False
                        else:
                            selected_car_id = None
                elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 3:
                    selected_car_id = None
                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        selected_car_id = None
                    elif selected_car_id:
                        car = game.cars.get(selected_car_id)
                        if car:
                            direction: Optional[str] = None
                            if car.orientation == "H":
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
                                moved = game.move_car(selected_car_id, direction)
                                if moved:
                                    manual_move_count += 1
                                    game.solution_path = []
                                    game.solution_index = 0
                                    game.nodes_expanded = 0
                                    if game.current_state:
                                        game.last_heuristic = heuristic(game.current_state)
                                    status_message = f"Moved {selected_car_id.upper()} {direction}."
                                    status_fade = 1.0
                                else:
                                    status_message = "Move blocked."
                                    status_fade = 1.0
                for button in buttons:
                    button.handle_event(event)

        if auto_play and game_state == "playing":
            auto_play_timer += dt
            if auto_play_timer >= auto_play_delay:
                auto_play_timer = 0.0
                moved = game.step_solution()
                if not moved:
                    auto_play = False
                    success_animation = 1.0
                    # Final victory particles
                    for _ in range(40):
                        x = random.uniform(WINDOW_SIZE * 0.2, WINDOW_SIZE * 0.8)
                        y = random.uniform(WINDOW_SIZE * 0.2, WINDOW_SIZE * 0.8)
                        particles.append(Particle(x, y, (255, 215, 0)))

        if game_state == "menu":
            # Animated gradient background
            for y in range(screen.get_height()):
                factor = y / screen.get_height()
                pulse = math.sin(background_pulse * 2) * 0.1
                r = int(20 + 15 * factor + pulse * 10)
                g = int(20 + 25 * factor + pulse * 15)
                b = int(40 + 30 * factor + pulse * 20)
                pygame.draw.line(screen, (r, g, b), (0, y), (screen.get_width(), y))

            # Animated title with glow
            title_text = title_font.render("Rush Hour AI", True, (255, 255, 255))
            title_rect = title_text.get_rect(center=(WINDOW_SIZE // 2, 120))

            # Pulsing glow effect
            glow_size = int(10 + 5 * math.sin(background_pulse * 3))
            for offset in range(glow_size, 0, -2):
                alpha = int(30 * (1 - offset / glow_size))
                glow_surf = pygame.Surface((title_rect.width + offset * 2, title_rect.height + offset * 2), pygame.SRCALPHA)
                glow_text = title_font.render("Rush Hour AI", True, (100, 150, 255, alpha))
                glow_rect = glow_text.get_rect(center=(glow_surf.get_width() // 2, glow_surf.get_height() // 2))
                glow_surf.blit(glow_text, glow_rect)
                screen.blit(glow_surf, (title_rect.x - offset, title_rect.y - offset))

            screen.blit(title_text, title_rect)

            # Dynamic subtitle based on menu state
            if menu_state == "difficulty_select":
                subtitle_str = "Select Difficulty Level"
            else:
                subtitle_str = f"{selected_difficulty} - Select Puzzle"
            subtitle_text = subtitle_font.render(subtitle_str, True, (200, 220, 240))
            subtitle_rect = subtitle_text.get_rect(center=(WINDOW_SIZE // 2, 190))
            screen.blit(subtitle_text, subtitle_rect)

            # Decorative line
            line_y = 230
            pygame.draw.line(screen, (80, 100, 140), (WINDOW_SIZE // 2 - 100, line_y),
                           (WINDOW_SIZE // 2 + 100, line_y), 2)

            for button in menu_buttons:
                button.draw(screen, button_font)
            pygame.display.flip()
            continue
        else:
            # Update car animations
            game.update_animations()

            board_surface = pygame.Surface((WINDOW_SIZE, WINDOW_SIZE))
            game.draw(board_surface)

            if selected_car_id and selected_car_id in game.animated_cars:
                cell_size = WINDOW_SIZE // game.grid_size
                animated = game.animated_cars[selected_car_id]
                car = animated.car
                width = int(cell_size * car.length) if car.orientation == "H" else cell_size
                height = cell_size if car.orientation == "H" else int(cell_size * car.length)
                highlight_rect = pygame.Rect(
                    int(animated.display_x * cell_size),
                    int(animated.display_y * cell_size),
                    width,
                    height,
                )
                highlight = pygame.Surface((highlight_rect.width, highlight_rect.height), pygame.SRCALPHA)
                pygame.draw.rect(highlight, (120, 200, 255, 70), highlight.get_rect(), border_radius=12)
                pygame.draw.rect(highlight, (120, 200, 255, 200), highlight.get_rect(), width=3, border_radius=12)
                board_surface.blit(highlight, highlight_rect.topleft)

            # Success overlay
            if success_animation > 0:
                overlay = pygame.Surface((WINDOW_SIZE, WINDOW_SIZE), pygame.SRCALPHA)
                alpha = int(100 * success_animation * math.sin(success_animation * math.pi))
                pygame.draw.rect(overlay, (255, 215, 0, alpha), overlay.get_rect())
                board_surface.blit(overlay, (0, 0))

            screen.blit(board_surface, (0, 0))

        # Draw particles
        for particle in particles:
            particle.draw(screen)

        # Modern info panel with gradient
        panel_height = BUTTON_HEIGHT + BUTTON_MARGIN * 2
        panel_surf = pygame.Surface((WINDOW_SIZE, panel_height), pygame.SRCALPHA)
        for y in range(panel_height):
            factor = y / panel_height
            color = (int(20 * (1 - factor * 0.3)), int(20 * (1 - factor * 0.3)), int(20 * (1 - factor * 0.3)), 255)
            pygame.draw.line(panel_surf, color, (0, y), (WINDOW_SIZE, y))
        screen.blit(panel_surf, (0, WINDOW_SIZE))

        # Info display with icons and better formatting
        info_data = [
            ("🎯", f"Moves: {game.solution_index}/{len(game.solution_path)}", (150, 200, 255)),
            ("🕹️", f"Manual Moves: {manual_move_count}", (255, 210, 120)),
            ("🧠", f"Heuristic: {game.last_heuristic:.1f}", (255, 180, 100)),
            ("📊", f"Expanded: {game.nodes_expanded}", (100, 255, 150)),
            ("⭐", f"Level: {selected_level}", (255, 220, 100)),
        ]

        start_x = WINDOW_SIZE - 360
        start_y = WINDOW_SIZE + BUTTON_MARGIN
        row_height = 22

        for idx, (icon, text, color) in enumerate(info_data):
            row_y = start_y + idx * row_height
            icon_surf = font.render(icon, True, color)
            screen.blit(icon_surf, (start_x, row_y))
            text_surf = font.render(text, True, (220, 220, 220))
            screen.blit(text_surf, (start_x + 25, row_y))

        status_y = start_y + len(info_data) * row_height + 10

        # Status message with fade and background
        if status_fade > 0:
            status_bg = pygame.Surface((340, 30), pygame.SRCALPHA)
            bg_alpha = int(120 * status_fade)
            pygame.draw.rect(status_bg, (40, 40, 40, bg_alpha), status_bg.get_rect(), border_radius=8)
            screen.blit(status_bg, (start_x - 5, status_y))

            status_color = (200 + int(55 * status_fade), 200 + int(55 * status_fade), 200 + int(55 * status_fade))
            status_surf = font.render(status_message, True, status_color)
            screen.blit(status_surf, (start_x, status_y + 4))

        for button in buttons:
            button.draw(screen, button_font)

        pygame.display.flip()


if __name__ == "__main__":
    main()
