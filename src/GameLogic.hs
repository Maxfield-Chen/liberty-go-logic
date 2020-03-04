module GameLogic where

import           Control.Lens               hiding (Empty)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Set                   as S
import           Data.The
import           Game
import           Logic.Implicit
import           Proofs
import           Theory.Named

printBoard :: Game -> IO ()
printBoard game = do
  let boardPositions = [ [ Pair x y | x <- [0 .. 18] ] | y <- [0 .. 18] ]
  mapM_
    (\row ->
      putStrLn ""
        >> mapM_
             (\pos -> name pos $ \case
               Bound boundPos -> (putStr (show (boundPos `getPosition` game) ++ " "))
               _   -> error "Invalid position in printBoard."
             )
             row
    )
    boardPositions

-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move resulted in a kill
-- Throws a MoveError exception if the move was invalid
placeStone :: Fact (IsBound pos) => (Position ~~ pos) -> ExceptGame Outcome
placeStone pos = do
  setPosition pos
  groups  <- lift $ gets (surroundingGroups pos)
  player  <- lift $ gets (getPosition pos)
  outcome <- resolveCapture player groups
  revertWhenIllegalKo outcome

surroundingGroups :: Fact (IsBound pos) => (Position ~~ pos) -> Game -> [Group]
surroundingGroups pos game =
  let bNeighbors = adjMatchingPos White pos game
      wNeighbors = adjMatchingPos Black pos game
      adjGroups  = map
        (\pos -> name pos $ \case
          Bound pos -> (posToGroup pos game)
          Unbound   -> error "Invalid position in surroundingGroups"
        )
        (S.toList (bNeighbors `S.union` wNeighbors))
      curGroup = posToGroup pos game
      player   = nextToPlay game
  in  curGroup : adjGroups

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

-- setPosition creates a new GameState from the previous gameState
-- with the new stone added.
setPosition :: Fact (IsBound pos) => Position ~~ pos -> ExceptGame ()
setPosition pos = do
  checkOccupied pos
  oldGState <- lift $ gets (fromMaybe newGameState . preview csl)
  ntp       <- lift $ gets nextToPlay
  cb        <- lift $ gets currentBoard
  let newBoard = M.insert (the pos) ntp cb
  record %= (:) (oldGState & board .~ newBoard)
  record . _head . toPlay %= swapPlayer

revokeRecord :: State Game ()
revokeRecord = do
  restRecord <- use record
  record .= tail restRecord

revertWhenIllegalKo :: Outcome -> ExceptGame Outcome
revertWhenIllegalKo o = do
  illegalKo <- lift $ gets isIllegalKo
  if illegalKo then lift revokeRecord >> throwE IllegalKo else pure o

csl :: Traversal' Game GameState
csl = record . _head

currentBoard :: Game -> Board
currentBoard game = fromMaybe M.empty (preview (csl . board) game)

nextToPlay :: Game -> Space
nextToPlay game = fromMaybe Black (preview (csl . toPlay) game)

swapPlayer :: Space -> Space
swapPlayer Black = White
swapPlayer White = Black

getPosition :: Fact (IsBound pos) => Position ~~ pos -> Game -> Space
getPosition pos game = M.findWithDefault Empty (the pos) (currentBoard game)

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Fact (IsBound pos) => Position ~~ pos -> Game -> [Position]
getNeighbors pos game =
  let neighbors = (\delta -> (+) <$> the pos <*> delta) <$> neighborDeltas
  in  filter (bounded (view boardSize game)) neighbors

checkOccupied :: Fact (IsBound pos) => Position ~~ pos -> ExceptGame ()
checkOccupied pos = do
  o <- lift $ isOccupied pos
  if o then throwE Occupied else pure ()

isOccupied :: Fact (IsBound pos) => Position ~~ pos -> State Game Bool
isOccupied pos = do
  space <- gets (getPosition pos)
  pure (space /= Empty)

isIllegalKo :: Game -> Bool
isIllegalKo game = case view record game of
  gs : _ : gs' : _ -> gs ^. board == gs' ^. board
  _                -> False

adjMatchingPos
  :: Fact (IsBound pos) => Space -> Position ~~ pos -> Game -> S.Set Position
adjMatchingPos sp pos game =
  let neighbors = getNeighbors pos game
  in  foldl
        (\ret neighborPos -> name neighborPos $ \case
          Bound neighborPos -> if getPosition neighborPos game == sp
            then S.insert (the neighborPos) ret
            else ret
          Unbound -> error "Invalid NeighborPosition in adjMatchingPos"
        )
        S.empty
        neighbors

-- Enum a group by adding all neighbors of members + their liberties
-- Base Case: return if new group == old group
-- Find all neighbors, add same player unseen to members
-- See if findSpaceOf is a candidate for GDP value. The folding classification may be tricky.
enumGroup :: Group -> Game -> Group
enumGroup group game =
  let findSpaceOf sp game = foldl
        (\ret pos -> name pos $ \case
          Bound pos -> adjMatchingPos sp pos game `S.union` ret
          Unbound   -> error "Invalid Position in enumGroup"
        )
      newMembers =
          findSpaceOf (group ^. player) game (group ^. members) (group ^. members)
      newLiberties = findSpaceOf Empty game (group ^. liberties) newMembers
      newGroup     = group { _members = newMembers, _liberties = newLiberties }
  in  if newGroup == group then newGroup else enumGroup newGroup game

-- Given a position, build the group associated with that position
posToGroup :: Fact (IsBound pos) => Position ~~ pos -> Game -> Group
posToGroup pos game =
  let player    = getPosition pos game
      liberties = adjMatchingPos Empty pos game
      newGroup  = Group liberties (S.singleton (the pos)) player
  in  enumGroup newGroup game
