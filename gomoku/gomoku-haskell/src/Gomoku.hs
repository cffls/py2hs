{-# LANGUAGE NamedFieldPuns #-}

module Gomoku
  ( playGame,
    createGame,
  )
where

import Data.Maybe
import System.Console.ANSI
import System.IO (BufferMode (NoBuffering), hReady, hSetBuffering, hSetEcho, stdin)

data Stone = StoneO | StoneX | Blank | Highlight deriving (Eq)

instance Show Stone where
  show StoneO = "○"
  show StoneX = "●"
  show Blank = " "
  show Highlight = "." -- Current selection

data Board = Board
  { grid :: [Stone], -- A list to track the placement of stones
    width :: Int -- An integer to store the dimension of the grid (width of the side)
  }

instance Show Board where
  show Board {grid, width}
    | gridLength == width ^ 2 = horizontalEdge ++ "\n" ++ "║" ++ showFirst ++ "|" ++ showRest
    | mod gridLength width == 0 = "║" ++ showFirst ++ "|" ++ showRest
    | gridLength == 1 = showFirst ++ "║\n" ++ horizontalEdge
    | mod gridLength width == 1 = showFirst ++ "║\n" ++ showRest
    | otherwise = showFirst ++ "|" ++ showRest
    where
      showFirst = show $ head grid
      showRest = show $ Board {grid = tail grid, width = width}
      gridLength = length grid
      horizontalEdge = replicate (width * 2 + 1) '='

createBoard :: Int -> Board
createBoard width = Board {grid = replicate (width ^ 2) Blank, width = width}

updateBoard :: Stone -> Int -> Board -> Maybe Board
updateBoard stone pos board@Board {grid, width}
  | stoneAtPos == StoneO || stoneAtPos == StoneX = Nothing
  | otherwise = Just $ Board {grid = replacedGrid, width = width}
  where
    stoneAtPos = grid !! pos
    replacedGrid = replaceNth pos stone grid
    replaceNth :: Int -> a -> [a] -> [a]
    replaceNth n newVal xs = x ++ (newVal : ys)
      where
        (x, _ : ys) = splitAt n xs

data Player = PlayerA | PlayerB deriving (Eq, Show)

playerToStone :: Player -> Stone
playerToStone PlayerA = StoneX
playerToStone PlayerB = StoneO

data Game = Game
  { player :: Player, -- Current player
    board :: Board, -- Board state
    selection :: Int, -- Current selection
    stoneCount :: Int -- Number of stones being placed on the board
  }

data Direction = Up | Right | Down | Left deriving (Eq)

nextSelection :: Direction -> Int -> Int -> Int
nextSelection direction width selection = case direction of
  Gomoku.Up -> finalizeSelection $ selection - width
  Gomoku.Down -> finalizeSelection $ selection + width
  Gomoku.Right -> finalizeSelection $ selection + 1
  Gomoku.Left -> finalizeSelection $ selection - 1
  where
    total = width ^ 2
    finalizeSelection x = mod (x + total) total -- Mod the selection index with the size of board to make sure
    -- the index does not overflow

createGame :: Int -> Game
createGame w = Game {player = PlayerA, board = initializedBoard, selection = selection, stoneCount = 0}
  where
    selection = w ^ 2 `div` 2 -- Try to select the center of the board
    freshBoard = createBoard w
    initializedBoard = fromMaybe freshBoard $ updateBoard Highlight selection freshBoard

instance Show Game where
  show game = concat ["\n", show $ board game, "\n\n", "Turn: ", show $ player game, "\n"]

goToNextAvailPos :: Direction -> Game -> Game
goToNextAvailPos direction game@Game {player, board, selection, stoneCount} = goToNextAvailPos' direction selection game
  where
    goToNextAvailPos' :: Direction -> Int -> Game -> Game
    goToNextAvailPos' direction initPos game@Game {player, board, selection, stoneCount}
      | nSelection == initPos = game
      | otherwise = case updatedBoard of
        Nothing -> goToNextAvailPos' direction initPos game {board = clearedBoard, selection = nSelection}
        Just b -> game {board = b, selection = nSelection}
      where
        clearedBoard = fromMaybe board $ updateBoard Blank selection board
        nSelection = nextSelection direction (width board) selection
        updatedBoard = updateBoard Highlight nSelection clearedBoard

placeStone :: Game -> Game
placeStone game@Game {player, board, selection, stoneCount} =
  case updatedBoard of
    Just b -> game {board = b, stoneCount = stoneCount + 1, player = nextPlayer player}
    Nothing -> game
  where
    updatedBoard = updateBoard (playerToStone player) selection board
    nextPlayer PlayerA = PlayerB
    nextPlayer PlayerB = PlayerA

updateGame :: String -> Game -> Game
updateGame "\ESC[A" game = goToNextAvailPos Gomoku.Up game
updateGame "\ESC[B" game = goToNextAvailPos Gomoku.Down game
updateGame "\ESC[C" game = goToNextAvailPos Gomoku.Right game
updateGame "\ESC[D" game = goToNextAvailPos Gomoku.Left game
updateGame "\n" game = placeStone game
updateGame _ game = game

checkWin :: Game -> Bool
checkWin game@Game {player, board, selection, stoneCount} = any (\d -> extension (x, y) d >= 5) dirs
  where
    dirs = [(0, 1), (1, 0), (1, 1), (1, -1)] -- Four directions (vertical, horizontal, and two diagonals) need to be checked
    w = width board
    x = selection `div` w
    y = selection `mod` w
    curGrid = grid board
    curStone = curGrid !! selection
    validPos x y = x >= 0 && y >= 0 && x < w && y < w
    extensionInDir (x, y) (dX, dY)
      | validPos x y && curGrid !! (x * w + y) == curStone =
        1 + extensionInDir (x + dX, y + dY) (dX, dY)
      | otherwise = 0
    extension (x, y) (dX, dY) =
      extensionInDir (x, y) (dX, dY) + extensionInDir (x, y) (- dX, - dY) - 1

checkDraw :: Game -> Bool
checkDraw game@Game {player, board, selection, stoneCount} = stoneCount == width board ^ 2

getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char : chars)

playGame :: Game -> IO ()
playGame game = do
  clearScreen
  playGame' game
  where
    playGame' game = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      printGame game
      input <- getKey
      let newGame = updateGame input game
      if input == "\n" && checkWin newGame
        then do
          printGame newGame
          putStrLn $ show (player game) ++ " won!"
        else
          if checkDraw newGame
            then do
              printGame newGame
              putStrLn "It is a draw!"
            else playGame' newGame
      where
        printGame game = do
          cursorUpLine numLines
          putStr gameStr
          where
            gameStr = show game
            numLines = length . filter ('\n' ==) $ gameStr
