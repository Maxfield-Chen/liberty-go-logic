module Board where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe


data Pair a = Pair a a deriving (Show, Eq, Ord)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

type Position = Pair Integer

data Outcome = Valid | Capture | IllegalKo | Suicide | OutOfBounds deriving (Show, Eq)
data Space = White | Black | Empty deriving (Show, Eq)

type Board = M.Map Position Space

-- TODO: Refactor record to include captures at each board state
data Game = Game { boardSize :: Integer
                 , record :: [Board]
                 , groups :: [Group]
                 , whiteCaptures :: Integer
                 , blackCaptures :: Integer} deriving (Show)

data Group = Group { liberties :: Integer
                   , members :: S.Set Position
                   , color :: Space} deriving (Show, Eq)

emptyBoard = M.empty
newGame = Game 19 [emptyBoard] [] 0 0

currentBoard :: Game -> Board
currentBoard g = if null (record g) then emptyBoard else head (record g)

getPosition :: Position -> State Game Space
getPosition pos =
  state (\game -> (M.findWithDefault Empty pos (currentBoard game), game))

-- TODO Add move validation
setPosition :: Position -> Space -> State Game Outcome
setPosition pos sp = state
  (\game ->
    let newBoard = M.insert pos sp (currentBoard game)
    in  (Valid, game { record = newBoard : record game })
  )

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> [Position]
getNeighbors pos = (\p -> (+) <$> pos <*> p) <$> neighborDeltas

updateGroup :: Position -> State Game ()
updateGroup pos = state
  (\game -> (listToMaybe $ filter (S.member pos . members) (groups game), game))

-- Place a stone, updating the game record and groups if the move is valid.
-- Will also update the number of captures if relevant
-- Use mapM to check all groups at once
placeStone :: Position -> Space -> State Game Outcome
placeStone pos sp =
  let neighbors = getNeighbors pos
  in  do
        curSpace <- getPosition pos
        setPosition pos sp
        mapM updateGroup neighbors


