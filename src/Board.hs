{-# LANGUAGE TemplateHaskell #-}

module Board where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import           Control.Lens            hiding ( Empty )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import           Control.Monad.Extra

data Pair a = Pair a a deriving (Show, Eq, Ord)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

type Position = Pair Int

data Outcome = Success | Kill

data MoveError = IllegalPlayer |  NoBoard |  IllegalKo | Suicide | OutOfBounds | Occupied deriving (Show, Eq)

data Space = Black | White | Empty deriving (Show, Eq, Ord)

type Board = M.Map Position Space

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

csl :: Traversal' Game GameState
csl = record . _head

currentGState :: MaybeT (State Game) GameState
currentGState = MaybeT $ preuse csl

currentBoard :: MaybeT (State Game) Board
currentBoard = MaybeT $ preuse (csl . board)

activeSpace :: MaybeT (State Game) Space
activeSpace = MaybeT $ preuse (csl . toPlay)

swapActiveSpace :: Space -> Space
swapActiveSpace Black = White
swapActiveSpace White = Black

getPosition :: Position -> MaybeT (State Game) Space
getPosition pos = M.findWithDefault Empty pos <$> currentBoard

setPosition :: Position -> MaybeT (State Game) ()
setPosition pos = do
  newBoard  <- M.insert pos <$> activeSpace <*> currentBoard
  oldGState <- currentGState
  record %= (:) (oldGState & board .~ newBoard)
  record . _head . toPlay %= swapActiveSpace

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> [Position]
getNeighbors pos = (\p -> (+) <$> pos <*> p) <$> neighborDeltas

isOccupied :: Position -> MaybeT (State Game) Bool
isOccupied pos =
  getPosition pos >>= (\sp -> if sp == Empty then pure False else pure True)

adjMatchingPos :: Space -> Position -> MaybeT (State Game) (S.Set Position)
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
enumGroup :: Group -> MaybeT (State Game) Group
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
posToGroup :: Position -> MaybeT (State Game) Group
posToGroup pos = do
  color     <- getPosition pos
  liberties <- adjMatchingPos color pos
  let newGroup = Group liberties (S.singleton pos) color
  enumGroup newGroup

revokeRecord :: MaybeT (State Game) ()
revokeRecord = do
  restRecord <- use (record . _tail)
  record .= restRecord

isIllegalKo :: MaybeT (State Game) Bool
isIllegalKo =
  use record
    >>= (\r -> case r of
          k : k' : _ -> pure (k ^. board == k' ^. board)
          _          -> pure False
        )

revertWhenIllegalKo
  :: Outcome -> ExceptT MoveError (MaybeT (State Game)) Outcome
revertWhenIllegalKo o = do
  illegalKo <- lift isIllegalKo
  if illegalKo then lift revokeRecord >> throwE IllegalKo else pure o

-- Given a group, remove all members of that group and credit the player with captures
captureGroup :: Group -> MaybeT (State Game) ()
captureGroup deadGroup = do
  record . _head . board %= M.filterWithKey
    (\pos _ -> not $ S.member pos (deadGroup ^. members))
  record . _head . captures . ix (_color deadGroup) += S.size
    (deadGroup ^. members)

-- Given surrounding groups, resolve stone captures + suicide
resolveCapture
  :: Space -> [Group] -> ExceptT MoveError (MaybeT (State Game)) Outcome
resolveCapture sp groups
  | null zeroLibGroups
  = pure Success
  | (sp == Black && not (null blackZLGroups) && null whiteZLGroups)
    || (sp == White && not (null whiteZLGroups) && null blackZLGroups)
  = lift revokeRecord >> throwE Suicide
  | sp == Black
  = lift (mapM_ captureGroup whiteZLGroups) >> pure Kill
  | sp == White
  = lift (mapM_ captureGroup blackZLGroups) >> pure Kill
  | otherwise
  = throwE IllegalPlayer
 where
  zeroLibGroups = filter ((==) 0 . S.size . _liberties) groups
  blackZLGroups = filter ((==) Black . _color) zeroLibGroups
  whiteZLGroups = filter ((==) White . _color) zeroLibGroups

checkBounds :: Position -> ExceptT MoveError (MaybeT (State Game)) ()
checkBounds (Pair x y) = do
  bs <- use boardSize
  if x >= 0 && y >= 0 && x < bs && y < bs then pure () else throwE OutOfBounds

checkOccupied :: Position -> ExceptT MoveError (MaybeT (State Game)) ()
checkOccupied pos = do
  o <- lift (isOccupied pos)
  if o then throwE Occupied else pure ()

-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move resulted in a kill
-- Throws a MoveError exception if the move was invalid
placeStone :: Position -> ExceptT MoveError (MaybeT (State Game)) Outcome
placeStone pos = do
  checkBounds pos
  checkOccupied pos
  lift (setPosition pos)
  neighbors <- lift (filterM isOccupied (getNeighbors pos))
  adjGroups <- lift (mapM posToGroup neighbors)
  color     <- lift activeSpace
  outcome   <- resolveCapture color adjGroups
  revertWhenIllegalKo outcome
  revertWhenIllegalKo outcome
