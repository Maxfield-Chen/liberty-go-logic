module Logic where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import           Control.Lens            hiding ( Empty )
import           Control.Monad.Trans.Except
import           Game

printBoard :: Game -> IO ()
printBoard game = do
  let boardPositions = [ [ Pair x y | x <- [0 .. 18] ] | y <- [0 .. 18] ]
  mapM_
    (\row ->
      putStrLn "" >> mapM_ (putStr . (++ " ") . show . (`getPosition` game)) row
    )
    boardPositions

-- Place a stone, updating the game record if the move is valid.
-- Returns an Outcome indicating if the move resulted in a kill
-- Throws a MoveError exception if the move was invalid
placeStone :: Position -> ExceptGame Outcome
placeStone pos = do
  setPosition pos
  groups  <- lift $ gets (surroundingGroups pos)
  player  <- lift $ gets (getPosition pos)
  outcome <- resolveCapture player groups
  revertWhenIllegalKo outcome

surroundingGroups :: Position -> Game -> [Group]
surroundingGroups pos game =
  let bNeighbors = adjMatchingPos White pos game
      wNeighbors = adjMatchingPos Black pos game
      adjGroups =
          map (`posToGroup` game) (S.toList (bNeighbors `S.union` wNeighbors))
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
setPosition :: Position -> ExceptGame ()
setPosition pos = do
  checkBounds pos
  checkOccupied pos
  oldGState <- lift $ gets (fromMaybe newGameState . preview csl)
  ntp       <- lift $ gets nextToPlay
  cb        <- lift $ gets currentBoard
  let newBoard = M.insert pos ntp cb
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

--csl :: Fact (State val) => Lens (Game ~~ val) GameState
csl :: Traversal' Game GameState
csl = record . _head

currentBoard :: Game -> Board
currentBoard game = fromMaybe M.empty (preview (csl . board) game)

nextToPlay :: Game -> Space
nextToPlay game = fromMaybe Black (preview (csl . toPlay) game)

swapPlayer :: Space -> Space
swapPlayer Black = White
swapPlayer White = Black

getPosition :: Position -> Game -> Space
getPosition pos game = M.findWithDefault Empty pos (currentBoard game)

neighborDeltas = [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]

getNeighbors :: Position -> Game -> [Position]
getNeighbors pos game =
  let neighbors = (\p -> (+) <$> pos <*> p) <$> neighborDeltas
  in  filter (bounded (view boardSize game)) neighbors

-- TODO: Having 3 functions do the same thing is atrocious. Look to migrate away from state
-- and exceptT as much as possible.
-- TODO: Replace ExceptT with GDP style validation
checkBounds :: Position -> ExceptGame ()
checkBounds pos = do
  oob <- lift (isWithinBounds pos)
  if oob then pure () else throwE OutOfBounds

isWithinBounds :: Position -> State Game Bool
isWithinBounds pos = do
  bs <- use boardSize
  pure $ bounded bs pos

bounded :: Int -> Position -> Bool
bounded bs (Pair x y) = x >= 0 && y >= 0 && x < bs && y < bs

checkOccupied :: Position -> ExceptGame ()
checkOccupied pos = do
  o <- isOccupied pos
  if o then throwE Occupied else pure ()

isOccupied :: Position -> ExceptGame Bool
isOccupied pos = do
  checkBounds pos
  space <- lift (gets (getPosition pos))
  pure (space /= Empty)

isIllegalKo :: Game -> Bool
isIllegalKo game = case view record game of
  gs : _ : gs' : _ -> gs ^. board == gs' ^. board
  _                -> False


adjMatchingPos :: Space -> Position -> Game -> S.Set Position
adjMatchingPos sp pos game =
  let neighbors = getNeighbors pos game
  in  foldl
        (\ret neighborPos -> if getPosition neighborPos game == sp
          then S.insert neighborPos ret
          else ret
        )
        S.empty
        neighbors

-- Enum a group by adding all neighbors of members + their liberties
-- Base Case: return if new group == old group
-- Find all neighbors, add same player unseen to members
enumGroup :: Group -> Game -> Group
enumGroup group game =
  let findSpaceOf sp game =
          foldl (\ret s -> adjMatchingPos sp s game `S.union` ret)
      newMembers = findSpaceOf (group ^. player)
                               game
                               (group ^. members)
                               (group ^. members)
      newLiberties = findSpaceOf Empty game (group ^. liberties) newMembers
      newGroup     = group { _members = newMembers, _liberties = newLiberties }
  in  if newGroup == group then newGroup else enumGroup newGroup game

-- Given a position, build the group associated with that position
posToGroup :: Position -> Game -> Group
posToGroup pos game =
  let player    = getPosition pos game
      liberties = adjMatchingPos Empty pos game
      newGroup  = Group liberties (S.singleton pos) player
  in  enumGroup newGroup game

