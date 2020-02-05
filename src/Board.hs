module Board where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import           Data.Functor


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
data Game = Game { boardSize :: Int
                 , record :: [GameState]} deriving (Show)

data GameState = GameState { board :: Board
                           , toPlay :: Space
                           , whiteCaptures :: Int
                           , blackCaptures :: Int} deriving (Show)

data Group = Group { liberties :: S.Set Position
                   , members :: S.Set Position
                   , color :: Space} deriving (Show, Eq)

newGameState = GameState M.empty Black 0 0
newGame = Game 19 [newGameState]

currentState :: State Game GameState
currentState = gets (head . record)

currentBoard :: State Game Board
currentBoard = board <$> currentState

activePlayer :: State Game Space
activePlayer = toPlay <$> currentState

getPosition :: Position -> State Game Space
getPosition pos = M.findWithDefault Empty pos <$> currentBoard

-- TODO: Check if occupied and check if within bounds
-- TODO: Refactor to return a maybe or either for better error handling?
setPosition :: Position -> State Game ()
setPosition pos = do
  r        <- gets record
  newBoard <- M.insert pos <$> activePlayer <*> currentBoard
  cs       <- currentState
  g        <- get
  let _ : rs   = r
      newState = cs { board = newBoard }
  put (g { record = newState : rs })

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
  = resolveSuicide
  | activePlayer == White && not (null whiteZLGroups) && null blackZLGroups
  = resolveSuicide
  | activePlayer == Black
  = mapM_ captureGroup whiteZLGroups >> pure Kill
  | activePlayer == White
  = mapM_ captureGroup blackZLGroups >> pure Kill
  | otherwise
  = error "Somehow the active player is not white or black."
 where
  zeroLibGroups = filter ((==) 0 . S.size . liberties) groups
  blackZLGroups = filter ((==) Black . color) zeroLibGroups
  whiteZLGroups = filter ((==) White . color) zeroLibGroups

resolveIllegalKo :: Outcome -> State Game Outcome
resolveIllegalKo o = do
  illegalKo <- isIllegalKo
  if illegalKo
    then
      state
        (\game -> let b : bs = record game in (IllegalKo, game { record = bs })
        )
    else pure o


isIllegalKo :: State Game Bool
isIllegalKo =
  gets record
    >>= (\r -> case r of
          k : k' : _ -> pure (board k == board k')
          _          -> pure False
        )

resolveSuicide :: State Game Outcome
resolveSuicide =
  state (\game -> let b : bs = record game in (Suicide, game { record = bs }))

-- Given a group, remove all members of that group and credit the player with captures
captureGroup :: Group -> State Game ()
captureGroup deadGroup = do
  cb <- currentBoard
  cs <- currentState
  ap <- activePlayer
  let newBoard = M.filterWithKey (isMember deadGroup) cb
      isMember tk pos _ = S.member pos (members tk)
      newState = if ap == Black
        then cs
          { board         = newBoard
          , blackCaptures = blackCaptures cs + (S.size . members) deadGroup
          }
        else cs
          { board         = newBoard
          , whiteCaptures = whiteCaptures cs + (S.size . members) deadGroup
          }
  newGame <-
    get >>= (\g -> let s : ss = record g in pure g { record = newState : ss })
  put newGame
