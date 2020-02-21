{-# LANGUAGE TemplateHaskell #-}

module Board where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import           Control.Lens            hiding ( Empty )
import           Control.Monad.Trans.Except

data Pair a = Pair a a deriving (Show, Eq, Ord)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

type Position = Pair Int

data Outcome = NoKill | Kill deriving (Eq, Show)

data MoveError = IllegalPlayer |  NoBoard |  IllegalKo | Suicide | OutOfBounds | Occupied deriving (Show, Eq)

data Space = Black | White | Empty deriving (Show, Eq, Ord)

type Board = M.Map Position Space

data Game = Game { _boardSize :: Int
                 , _record :: [GameState]} deriving (Show, Eq)

data GameState = GameState { _board :: Board
                           , _toPlay :: Space
                           , _captures :: M.Map Space Int} deriving (Show, Eq)

data Group = Group { _liberties :: S.Set Position
                   , _members :: S.Set Position
                   , _player :: Space} deriving (Show, Eq)

makeLenses ''Game
makeLenses ''GameState
makeLenses ''Group

type ExceptGame a = ExceptT MoveError (State Game) a

standardBoardSize = 19
newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game standardBoardSize [newGameState]

csl :: Traversal' Game GameState
csl = record . _head

currentGState :: ExceptGame GameState
currentGState = do
  mgs <- preuse csl
  case mgs of
    Just gs -> pure gs
    Nothing -> throwE NoBoard

currentBoard :: ExceptGame Board
currentBoard = do
  mb <- preuse (csl . board)
  case mb of
    Just b  -> pure b
    Nothing -> throwE NoBoard

nextToPlay :: ExceptGame Space
nextToPlay = do
  maybeActiveSpace <- preuse (csl . toPlay)
  case maybeActiveSpace of
    Just ap -> pure ap
    Nothing -> throwE NoBoard

swapPlayer :: Space -> Space
swapPlayer Black = White
swapPlayer White = Black

getPosition :: Position -> ExceptGame Space
getPosition pos = do
  checkBounds pos
  M.findWithDefault Empty pos <$> currentBoard

-- setPosition creates a new GameState from the previous gameState
-- with the new stone added. 
setPosition :: Position -> ExceptGame ()
setPosition pos = do
  checkBounds pos
  checkOccupied pos
  newBoard  <- M.insert pos <$> nextToPlay <*> currentBoard
  oldGState <- currentGState
  record %= (:) (oldGState & board .~ newBoard)
  record . _head . toPlay %= swapPlayer

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> ExceptGame [Position]
getNeighbors pos = do
  checkBounds pos
  let neighbors = (\p -> (+) <$> pos <*> p) <$> neighborDeltas
  lift $ filterM isWithinBounds neighbors

checkBounds :: Position -> ExceptGame ()
checkBounds pos = do
  oob <- lift (isWithinBounds pos)
  if oob then pure () else throwE OutOfBounds

isWithinBounds :: Position -> State Game Bool
isWithinBounds (Pair x y) = do
  bs <- use boardSize
  pure (x >= 0 && y >= 0 && x < bs && y < bs)

checkOccupied :: Position -> ExceptGame ()
checkOccupied pos = do
  o <- isOccupied pos
  if o then throwE Occupied else pure ()

isOccupied :: Position -> ExceptGame Bool
isOccupied pos = do
  checkBounds pos
  space <- getPosition pos
  pure (space /= Empty)

isIllegalKo :: State Game Bool
isIllegalKo =
  use record
    >>= (\r -> case r of
          k : _ : k' : _ -> pure (k ^. board == k' ^. board)
          _              -> pure False
        )

adjMatchingPos :: Space -> Position -> ExceptGame (S.Set Position)
adjMatchingPos sp pos = do
  neighbors <- getNeighbors pos
  foldM
    (\ret p -> do
      curSpace <- getPosition p
      if curSpace == sp then pure (S.insert p ret) else pure ret
    )
    S.empty
    neighbors

-- Enum a group by adding all neighbors of members + their liberties
-- Base Case: return if new group == old group
-- Find all neighbors, add same player unseen to members
enumGroup :: Group -> ExceptGame Group
enumGroup group = do
  let findSpaceOf sp =
        foldM (\ret s -> adjMatchingPos sp s >>= (pure . S.union ret))
  newMembers <- findSpaceOf (group ^. player)
                            (group ^. members)
                            (group ^. members)
  newLiberties <- findSpaceOf Empty (group ^. liberties) newMembers
  let newGroup = group { _members = newMembers, _liberties = newLiberties }
  if newGroup == group then pure newGroup else enumGroup newGroup

-- Given a position, build the group associated with that position
posToGroup :: Position -> ExceptGame Group
posToGroup pos = do
  player    <- getPosition pos
  liberties <- adjMatchingPos Empty pos
  let newGroup = Group liberties (S.singleton pos) player
  enumGroup newGroup

revokeRecord :: State Game ()
revokeRecord = do
  restRecord <- use record
  record .= tail restRecord

revertWhenIllegalKo :: Outcome -> ExceptGame Outcome
revertWhenIllegalKo o = do
  illegalKo <- lift isIllegalKo
  if illegalKo then lift revokeRecord >> throwE IllegalKo else pure o

-- Given a group, remove all members of that group 
-- credit the opposing player with captures
captureGroup :: Group -> ExceptGame ()
captureGroup deadGroup = do
  record . _head . board %= M.filterWithKey
    (\pos _ -> not $ S.member pos (deadGroup ^. members))
  record . _head . captures . ix ((swapPlayer . _player) deadGroup) += S.size
    (deadGroup ^. members)

-- Given surrounding groups, resolve stone captures + suicide
resolveCapture :: Space -> [Group] -> ExceptGame Outcome
resolveCapture sp groups
  | null zeroLibGroups
  = pure NoKill
  | (sp == Black && not (null blackZLGroups) && null whiteZLGroups)
    || (sp == White && not (null whiteZLGroups) && null blackZLGroups)
  = lift revokeRecord >> throwE Suicide
  | sp == Black
  = mapM_ captureGroup whiteZLGroups >> pure Kill
  | sp == White
  = mapM_ captureGroup blackZLGroups >> pure Kill
  | otherwise
  = throwE IllegalPlayer
 where
  zeroLibGroups = filter ((==) 0 . S.size . _liberties) groups
  blackZLGroups = filter ((==) Black . _player) zeroLibGroups
  whiteZLGroups = filter ((==) White . _player) zeroLibGroups


-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move resulted in a kill
-- Throws a MoveError exception if the move was invalid
placeStone :: Position -> ExceptGame Outcome
placeStone pos = do
  setPosition pos
  bNeighbors <- adjMatchingPos White pos
  wNeighbors <- adjMatchingPos Black pos
  adjGroups  <- mapM posToGroup (S.toList (bNeighbors `S.union` wNeighbors))
  curGroup   <- posToGroup pos
  player     <- getPosition pos
  outcome    <- resolveCapture player (curGroup : adjGroups)
  revertWhenIllegalKo outcome
