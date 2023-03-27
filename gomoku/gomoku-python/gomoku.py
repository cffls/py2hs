import sys
import termios
import tty
from typing import List, Optional
from enum import Enum, auto
from contextlib import contextmanager

class Stone(Enum):
    STONE_O = "○"
    STONE_X = "●"
    BLANK = " "
    HIGHLIGHT = "."  # Current selection

    def __str__(self):
        return self.value

class Board:
    def __init__(self, grid: List[Stone], width: int):
        self.grid = grid
        self.width = width

    def __str__(self):
        grid_length = len(self.grid)
        horizontal_edge = (self.width * 2 + 1) * "="
        if grid_length == self.width ** 2:
            return horizontal_edge + "\n" + "║" + str(self.grid[0]) + "|" + str(Board(self.grid[1:], self.width))
        elif grid_length % self.width == 0:
            return "║" + str(self.grid[0]) + "|" + str(Board(self.grid[1:], self.width))
        elif grid_length == 1:
            return str(self.grid[0]) + "║\n" + horizontal_edge
        elif grid_length % self.width == 1:
            return str(self.grid[0]) + "║\n" + str(Board(self.grid[1:], self.width))
        else:
            return str(self.grid[0]) + "|" + str(Board(self.grid[1:], self.width))

def create_board(width: int) -> Board:
    return Board([Stone.BLANK] * (width ** 2), width)

def update_board(stone: Stone, pos: int, board: Board) -> Optional[Board]:
    if board.grid[pos] in [Stone.STONE_O, Stone.STONE_X]:
        return None
    return Board(board.grid[:pos] + [stone] + board.grid[pos + 1:], board.width)

class Player(Enum):
    PLAYER_A = auto()
    PLAYER_B = auto()

    def __str__(self) -> str:
        if self == Player.PLAYER_A:
            return "Player A"
        elif self == Player.PLAYER_B:
            return "Player B"

def player_to_stone(player: Player) -> Stone:
    return Stone.STONE_X if player == Player.PLAYER_A else Stone.STONE_O

class Direction(Enum):
    UP = auto()
    RIGHT = auto()
    DOWN = auto()
    LEFT = auto()

def next_selection(direction: Direction, width: int, selection: int) -> int:
    total = width ** 2
    new_selection = {
        Direction.UP: selection - width,
        Direction.DOWN: selection + width,
        Direction.RIGHT: selection + 1,
        Direction.LEFT: selection - 1,
    }[direction]
    return (new_selection + total) % total

class Game:
    def __init__(self, player: Player, board: Board, selection: int, stone_count: int):
        self.player = player
        self.board = board
        self.selection = selection
        self.stone_count = stone_count

    def __str__(self):
        return f"\n{self.board}\n\nTurn: {self.player}\n"

def create_game(width: int) -> Game:
    selection = width ** 2 // 2
    fresh_board = create_board(width)
    initialized_board = update_board(Stone.HIGHLIGHT, selection, fresh_board)
    return Game(Player.PLAYER_A, initialized_board, selection, 0)

def go_to_next_avail_pos(direction: Direction, game: Game) -> Game:
    def go_to_next_avail_pos_helper(direction: Direction, init_pos: int, game: Game) -> Game:
        cleared_board = update_board(Stone.BLANK, game.selection, game.board)
        if cleared_board is None:
            cleared_board = game.board
        new_selection = next_selection(direction, game.board.width, game.selection)
        updated_board = update_board(Stone.HIGHLIGHT, new_selection, cleared_board)

        if new_selection == init_pos:
            return game
        elif updated_board is None:
            return go_to_next_avail_pos_helper(direction, init_pos, Game(game.player, cleared_board, new_selection, game.stone_count))
        else:
            return Game(game.player, updated_board, new_selection, game.stone_count)

    return go_to_next_avail_pos_helper(direction, game.selection, game)

def place_stone(game: Game) -> Game:
    updated_board = update_board(player_to_stone(game.player), game.selection, game.board)
    next_player = Player.PLAYER_B if game.player == Player.PLAYER_A else Player.PLAYER_A

    if updated_board is None:
        return game
    else:
        return Game(next_player, updated_board, game.selection, game.stone_count + 1)

def update_game(input_str: str, game: Game) -> Game:
    if input_str == "\x1b[A":
        return go_to_next_avail_pos(Direction.UP, game)
    elif input_str == "\x1b[B":
        return go_to_next_avail_pos(Direction.DOWN, game)
    elif input_str == "\x1b[C":
        return go_to_next_avail_pos(Direction.RIGHT, game)
    elif input_str == "\x1b[D":
        return go_to_next_avail_pos(Direction.LEFT, game)
    elif input_str in ("\n", "\r"):
        return place_stone(game)
    else:
        return game
    

def check_win(game: Game) -> bool:
    def extension(x: int, y: int, dx: int, dy: int) -> int:
        return extension_in_dir(x, y, dx, dy) + extension_in_dir(x, y, -dx, -dy) - 1

    def extension_in_dir(x: int, y: int, dx: int, dy: int) -> int:
        if valid_pos(x, y) and game.board.grid[x * game.board.width + y] == cur_stone:
            return 1 + extension_in_dir(x + dx, y + dy, dx, dy)
        else:
            return 0

    def valid_pos(x: int, y: int) -> bool:
        return 0 <= x < game.board.width and 0 <= y < game.board.width

    dirs = [(0, 1), (1, 0), (1, 1), (1, -1)]  # Vertical, horizontal, and two diagonals to be checked
    width = game.board.width
    selection = game.selection
    x, y = selection // width, selection % width
    cur_stone = game.board.grid[selection]
    return any(extension(x, y, dx, dy) >= 5 for dx, dy in dirs)

def check_draw(game: Game) -> bool:
    return game.stone_count == game.board.width ** 2

@contextmanager
def terminal_settings(file_descriptor: int):
    old_settings = termios.tcgetattr(file_descriptor)
    try:
        tty.setraw(file_descriptor)
        yield
    finally:
        termios.tcsetattr(file_descriptor, termios.TCSADRAIN, old_settings)

def get_key() -> str:
    with terminal_settings(sys.stdin.fileno()):
        ch = sys.stdin.read(1)
        if ch == "\x1b":
            ch += sys.stdin.read(2)
        return ch

def clear_screen() -> None:
    print("\033[H\033[J", end="")

def play_game(game: Game) -> None:
    clear_screen()
    play_game_helper(game)

def play_game_helper(game: Game) -> None:
    print(game, end="")
    input_str = get_key()
    new_game = update_game(input_str, game)

    clear_screen()

    if check_win(new_game):
        print(new_game)
        print(f"{game.player} won!")
    elif check_draw(new_game):
        print(new_game)
        print("It is a draw!")
    else:
        play_game_helper(new_game)

def main() -> None:
    game = create_game(15)
    play_game(game)


if __name__ == "__main__":
    main()