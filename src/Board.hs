{-# LANGUAGE TemplateHaskell #-}

module Board where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import           Control.Lens            hiding ( Empty )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except

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

type ExceptMaybeGame a = ExceptT MoveError (MaybeT (State Game)) a
type MaybeGame a = MaybeT (State Game) a

newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game 19 [newGameState]

csl :: Traversal' Game GameState
csl = record . _head

currentGState :: MaybeGame GameState
currentGState = MaybeT $ preuse csl

currentBoard :: MaybeGame Board
currentBoard = MaybeT $ preuse (csl . board)

activeSpace :: MaybeGame Space
activeSpace = MaybeT $ preuse (csl . toPlay)

swapActiveSpace :: Space -> Space
swapActiveSpace Black = White
swapActiveSpace White = Black

getPosition :: Position -> MaybeGame Space
getPosition pos = M.findWithDefault Empty pos <$> currentBoard

setPosition :: Position -> MaybeGame ()
setPosition pos = do
  newBoard  <- M.insert pos <$> activeSpace <*> currentBoard
  oldGState <- currentGState
  record %= (:) (oldGState & board .~ newBoard)
  record . _head . toPlay %= swapActiveSpace

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> [Position]
getNeighbors pos = (\p -> (+) <$> pos <*> p) <$> neighborDeltas

isOccupied :: Position -> MaybeGame Bool
isOccupied pos =
  getPosition pos >>= (\sp -> if sp == Empty then pure False else pure True)

adjMatchingPos :: Space -> Position -> MaybeGame (S.Set Position)
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
enumGroup :: Group -> MaybeGame Group
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
posToGroup :: Position -> MaybeGame Group
posToGroup pos = do
  color     <- getPosition pos
  liberties <- adjMatchingPos color pos
  let newGroup = Group liberties (S.singleton pos) color
  enumGroup newGroup

revokeRecord :: MaybeGame ()
revokeRecord = do
  restRecord <- use (record . _tail)
  record .= restRecord

isIllegalKo :: State Game Bool
isIllegalKo =
  use record
    >>= (\r -> case r of
          k : k' : _ -> pure (k ^. board == k' ^. board)
          _          -> pure False
        )

revertWhenIllegalKo :: Outcome -> ExceptMaybeGame Outcome
revertWhenIllegalKo o = do
  illegalKo <- (lift . lift) isIllegalKo
  if illegalKo then lift revokeRecord >> throwE IllegalKo else pure o

-- Given a group, remove all members of that group and credit the player with captures
captureGroup :: Group -> MaybeGame ()
captureGroup deadGroup = do
  record . _head . board %= M.filterWithKey
    (\pos _ -> not $ S.member pos (deadGroup ^. members))
  record . _head . captures . ix (_color deadGroup) += S.size
    (deadGroup ^. members)

-- Given surrounding groups, resolve stone captures + suicide
resolveCapture :: Space -> [Group] -> ExceptMaybeGame Outcome
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

checkBounds :: Position -> ExceptMaybeGame ()
checkBounds (Pair x y) = do
  bs <- use boardSize
  if x >= 0 && y >= 0 && x < bs && y < bs then pure () else throwE OutOfBounds

checkOccupied :: Position -> ExceptMaybeGame ()
checkOccupied pos = do
  o <- lift (isOccupied pos)
  if o then throwE Occupied else pure ()

-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move resulted in a kill
-- Throws a MoveError exception if the move was invalid
-- TODO: Figure out how to remove lift calls: https://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers
placeStone :: Position -> ExceptMaybeGame Outcome
placeStone pos = do
  checkBounds pos
  checkOccupied pos
  lift (setPosition pos)
  neighbors <- lift (filterM isOccupied (getNeighbors pos))
  adjGroups <- lift (mapM posToGroup neighbors)
  color     <- lift activeSpace
  outcome   <- resolveCapture color adjGroups
  revertWhenIllegalKo outcome
