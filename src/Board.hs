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

data Outcome = Alive | Dead | IllegalKo | Suicide | OutOfBounds deriving (Show, Eq)
-- TODO: Refactor Space to deal with empty case. Maybe instead?
data Space = Black | White | Empty deriving (Show, Eq, Ord)

type Board = M.Map Position Space

-- TODO: Refactor record to include captures at each board state
data Game = Game { boardSize :: Integer
                 , record :: [Board]
                 , whiteCaptures :: Integer
                 , blackCaptures :: Integer
                 , activePlayer :: Space} deriving (Show)

data Group = Group { liberties :: S.Set Position
                   , members :: S.Set Position
                   , color :: Space} deriving (Show, Eq)

emptyBoard = M.empty
newGame = Game 19 [emptyBoard] 0 0 Black

currentBoard :: Game -> Board
currentBoard g = if null (record g) then emptyBoard else head (record g)

getPosition :: Position -> State Game Space
getPosition pos =
  state (\game -> (M.findWithDefault Empty pos (currentBoard game), game))

-- TODO Add move validation
setPosition :: Position -> State Game ()
setPosition pos = state
  (\game ->
    let newBoard = M.insert pos (activePlayer game) (currentBoard game)
    in  ((), game { record = newBoard : record game })
  )

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> [Position]
getNeighbors pos = (\p -> (+) <$> pos <*> p) <$> neighborDeltas

adjMatchingPos :: Space -> Position -> State Game (S.Set Position)
adjMatchingPos sp pos = do
  let neighbors = getNeighbors pos
  spaces <- mapM getPosition neighbors
  let liberties = foldr
        (\(p, curSpace) ret -> if sp == curSpace then S.insert p ret else ret)
        S.empty
        (zip neighbors spaces)
  pure liberties

-- Enum a group by adding all neighbors of members + their liberties
-- Base Case: return if new group == old group
-- Find all neighbors, add same color unseen to members
enumGroup :: Group -> State Game Group
enumGroup group = do
  newMembers <- foldM
    (\ret m -> adjMatchingPos (color group) m >>= (pure . S.union ret))
    (members group)
    (members group)
  newLiberties <- foldM
    (\ret lib -> adjMatchingPos Empty lib >>= (pure . S.union ret))
    (liberties group)
    newMembers
  let newGroup = group { members = newMembers, liberties = newLiberties }
  if newGroup == group then pure newGroup else enumGroup newGroup


-- Given a position, build the group associated with that position
posToGroup :: Position -> State Game Group
posToGroup pos = do
  color     <- getPosition pos
  liberties <- adjMatchingPos color pos
  let newGroup = Group liberties (S.singleton pos) color
  enumGroup newGroup

-- Place a stone, updating the game record and groups if the move is valid.
-- Use mapM to check all groups at once
placeStone :: Position -> State Game ()
placeStone pos = do
  curSpace <- getPosition pos
  let neighbors = getNeighbors pos
  setPosition pos
