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
                 , _record :: [GameState]} deriving (Show, Eq)

data GameState = GameState { _board :: Board
                           , _toPlay :: Space
                           , _captures :: M.Map Space Int} deriving (Show, Eq)

data Group = Group { _liberties :: S.Set Position
                   , _members :: S.Set Position
                   , _color :: Space} deriving (Show, Eq)

makeLenses ''Game
makeLenses ''GameState
makeLenses ''Group

type ExceptGame a = ExceptT MoveError (State Game) a

newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game 19 [newGameState]

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

activeSpace :: ExceptGame Space
activeSpace = do
  maybeActiveSpace <- preuse (csl . toPlay)
  case maybeActiveSpace of
    Just ap -> pure ap
    Nothing -> throwE NoBoard

swapActiveSpace :: Space -> Space
swapActiveSpace Black = White
swapActiveSpace White = Black

getPosition :: Position -> ExceptGame Space
getPosition pos = do
  checkBounds pos
  M.findWithDefault Empty pos <$> currentBoard

setPosition :: Position -> ExceptGame ()
setPosition pos = do
  checkBounds pos
  isOccupied pos
  newBoard  <- M.insert pos <$> activeSpace <*> currentBoard
  oldGState <- currentGState
  record %= (:) (oldGState & board .~ newBoard)
  record . _head . toPlay %= swapActiveSpace

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> [Position]
getNeighbors pos = (\p -> (+) <$> pos <*> p) <$> neighborDeltas

isOccupied :: Position -> ExceptGame Bool
isOccupied pos = do
  checkBounds pos
  space <- getPosition pos
  pure (space /= Empty)

-- TODO: Account for edge of board logic here
adjMatchingPos :: Space -> Position -> ExceptGame (S.Set Position)
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
enumGroup :: Group -> ExceptGame Group
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
posToGroup :: Position -> ExceptGame Group
posToGroup pos = do
  color     <- getPosition pos
  liberties <- adjMatchingPos color pos
  let newGroup = Group liberties (S.singleton pos) color
  enumGroup newGroup

revokeRecord :: State Game ()
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

revertWhenIllegalKo :: Outcome -> ExceptGame Outcome
revertWhenIllegalKo o = do
  illegalKo <- lift isIllegalKo
  if illegalKo then lift revokeRecord >> throwE IllegalKo else pure o

-- Given a group, remove all members of that group and credit the player with captures
captureGroup :: Group -> ExceptGame ()
captureGroup deadGroup = do
  record . _head . board %= M.filterWithKey
    (\pos _ -> not $ S.member pos (deadGroup ^. members))
  record . _head . captures . ix (_color deadGroup) += S.size
    (deadGroup ^. members)

-- Given surrounding groups, resolve stone captures + suicide
resolveCapture :: Space -> [Group] -> ExceptGame Outcome
resolveCapture sp groups
  | null zeroLibGroups
  = pure Success
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
  blackZLGroups = filter ((==) Black . _color) zeroLibGroups
  whiteZLGroups = filter ((==) White . _color) zeroLibGroups

checkBounds :: Position -> ExceptGame ()
checkBounds (Pair x y) = do
  bs <- use boardSize
  if x >= 0 && y >= 0 && x < bs && y < bs then pure () else throwE OutOfBounds

checkOccupied :: Position -> ExceptGame ()
checkOccupied pos = do
  o <- isOccupied pos
  if o then throwE Occupied else pure ()

-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move resulted in a kill
-- Throws a MoveError exception if the move was invalid
-- TODO: Figure out how to remove lift calls: https://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers
placeStone :: Position -> ExceptGame Outcome
placeStone pos = do
  setPosition pos
  neighbors <- filterM isOccupied (getNeighbors pos)
  adjGroups <- mapM posToGroup neighbors
  color     <- activeSpace
  outcome   <- resolveCapture color adjGroups
  revertWhenIllegalKo outcome
