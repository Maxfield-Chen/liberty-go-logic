{-# LANGUAGE TemplateHaskell #-}



module Board where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import           Control.Lens            hiding ( Empty )


data Pair a = Pair a a deriving (Show, Eq, Ord)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

type Position = Pair Int

-- TODO split outcome into error and outcome to facilitate either
data Outcome = Nop | Kill | IllegalKo | Suicide | OutOfBounds deriving (Show, Eq)
-- TODO: Refactor Space to deal with empty case. Maybe instead?
data Space = Black | White | Empty deriving (Show, Eq, Ord)

type Board = M.Map Position Space

-- TODO: Refactor record to include captures / active player at each board state
data Game = Game { _boardSize :: Int
                 , _record :: [GameState]} deriving (Show)

data GameState = GameState { _board :: Board
                           , _toPlay :: Space
                           , _captures :: M.Map Space Int} deriving (Show)

data Group = Group { _liberties :: S.Set Position
                   , _members :: S.Set Position
                   , _color :: Space} deriving (Show, Eq)

makeLenses ''Game
makeLenses ''GameState
makeLenses ''Group

newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game 19 [newGameState]

-- TODO: Refactor to use lens.
-- Was struggling to deal with functional dependency of cons when using _head
currentState :: State Game GameState
currentState = gets (head . view record)

currentBoard :: State Game Board
currentBoard = view board <$> currentState


activePlayer :: State Game Space
activePlayer = view toPlay <$> currentState

getPosition :: Position -> State Game Space
getPosition pos = M.findWithDefault Empty pos <$> currentBoard

-- TODO: Check if occupied and check if within bounds
-- TODO: Refactor to return a maybe or either for better error handling?
setPosition :: Position -> State Game ()
setPosition pos = do
  newBoard <- M.insert pos <$> activePlayer <*> currentBoard
  oldState <- use record
  record %= (:) (head oldState & board .~ newBoard)

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> [Position]
getNeighbors pos = (\p -> (+) <$> pos <*> p) <$> neighborDeltas

isOccupied :: Position -> State Game Bool
isOccupied pos =
  getPosition pos >>= (\sp -> if sp == Empty then pure False else pure True)


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
  let findSpaceOf sp =
        foldM (\ret s -> adjMatchingPos sp s >>= (pure . S.union ret))
  newMembers <- findSpaceOf (group ^. color)
                            (group ^. members)
                            (group ^. members)
  newLiberties <- findSpaceOf Empty (group ^. liberties) newMembers
  let newGroup = group { _members = newMembers, _liberties = newLiberties }
  if newGroup == group then pure newGroup else enumGroup newGroup


-- Given a position, build the group associated with that position
posToGroup :: Position -> State Game Group
posToGroup pos = do
  color     <- getPosition pos
  liberties <- adjMatchingPos color pos
  let newGroup = Group liberties (S.singleton pos) color
  enumGroup newGroup

-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move was valid, and if a group was captured
-- TODO learn monad transformers so that I can use an either within this state monad to better track the flow of potential failures
placeStone :: Position -> State Game Outcome
placeStone pos = do
  setPosition pos
  neighbors <- filterM isOccupied (getNeighbors pos)
  adjGroups <- mapM posToGroup neighbors
  color     <- activePlayer
  outcome   <- resolvePlacement color adjGroups
  resolveIllegalKo outcome

-- Given surrounding groups, resolve stone placement + captures
-- Does not handle IllegalKO
-- TODO break out suicide logic into a separate function
-- TODO rename to resolveCapture
resolvePlacement :: Space -> [Group] -> State Game Outcome
resolvePlacement activePlayer groups
  | null zeroLibGroups
  = pure Nop
  | activePlayer == Black && not (null blackZLGroups) && null whiteZLGroups
  = revokeRecord >> pure Suicide
  | activePlayer == White && not (null whiteZLGroups) && null blackZLGroups
  = revokeRecord >> pure Suicide
  | activePlayer == Black
  = mapM_ captureGroup whiteZLGroups >> pure Kill
  | activePlayer == White
  = mapM_ captureGroup blackZLGroups >> pure Kill
  | otherwise
  = error "Somehow the active player is not white or black."
 where
  zeroLibGroups = filter ((==) 0 . S.size . _liberties) groups
  blackZLGroups = filter ((==) Black . _color) zeroLibGroups
  whiteZLGroups = filter ((==) White . _color) zeroLibGroups

revokeRecord :: State Game ()
revokeRecord = do
  restRecord <- use (record . _tail)
  record .= restRecord


resolveIllegalKo :: Outcome -> State Game Outcome
resolveIllegalKo o = do
  illegalKo <- isIllegalKo
  if illegalKo then revokeRecord >> pure IllegalKo else pure o


isIllegalKo :: State Game Bool
isIllegalKo =
  use record
    >>= (\r -> case r of
          k : k' : _ -> pure (k ^. board == k' ^. board)
          _          -> pure False
        )

-- Given a group, remove all members of that group and credit the player with captures
captureGroup :: Group -> State Game ()
captureGroup deadGroup = do
  record . _head . board %= M.filterWithKey
    (\pos _ -> S.member pos (deadGroup ^. members))
  record . _head . captures . ix Black += S.size (deadGroup ^. members)

