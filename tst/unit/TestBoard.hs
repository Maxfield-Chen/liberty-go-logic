{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module TestBoard where

import           Control.Lens               ((%~), (&))
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Sort                  (sort)
import           Game
import qualified GameLogic                  as GL
import           Proofs
import           Test.HUnit
import           Theory.Named

-- TODO: Add unit tests for status state machine

inProgressBoard = M.fromList [(Pair 0 0, Black)]
suicideBoard = M.fromList
  [(Pair 2 1, White), (Pair 3 0, White), (Pair 3 2, White), (Pair 4 1, White)]
groupBoard =
  M.fromList [(Pair 0 0, Black), (Pair 0 1, Black), (Pair 0 2, Black)]
--  X is black, o is White, Black to play
--  x o
-- xo   o
--  x o
k1Board = M.fromList
  [ (Pair 2 0, Black)
  , (Pair 1 1, Black)
  , (Pair 2 2, Black)
  , (Pair 2 1, White)
  , (Pair 3 0, White)
  , (Pair 3 2, White)
  , (Pair 4 1, White)
  ]
k2Board = M.insert (Pair 3 1) Black (M.delete (Pair 2 1) k1Board)
k3Board = M.insert (Pair 2 1) White (M.delete (Pair 3 1) k2Board)
k1GS = GameState k1Board Black (M.fromList [(Black, 0), (White, 0)])
k2GS = GameState k2Board White (M.fromList [(Black, 1), (White, 0)])
k3GS = GameState k3Board Black (M.fromList [(Black, 1), (White, 1)])
k1Game = Game standardBoardSize [k1GS] standardKomi M.empty (0,0) InProgress
k2Game = Game standardBoardSize [k2GS, k1GS] standardKomi M.empty (0,0) InProgress
k3Game = Game standardBoardSize [k3GS, k2GS, k1GS] standardKomi M.empty (0,0) InProgress
suicideGS = GameState suicideBoard Black (M.fromList [(Black, 0), (White, 0)])
inProgressGS =
  GameState inProgressBoard White (M.fromList [(Black, 0), (White, 0)])
inProgressGame = Game standardBoardSize [inProgressGS, newGameState] standardKomi M.empty (0,0) InProgress
groupGS = GameState groupBoard White (M.fromList [(Black, 0), (White, 0)])
groupGame = Game standardBoardSize [groupGS] standardKomi M.empty (0,0) InProgress
suicideGame = Game standardBoardSize [suicideGS] standardKomi M.empty (0,0) InProgress

testCurrentBoard = TestCase
  (assertEqual "for currentBoard with begun game,"
               inProgressBoard
               (GL.currentBoard inProgressGame)
  )

testNextToPlay = TestCase
  (assertEqual "for NextToPlay with begun game,"
               White
               (GL.nextToPlay inProgressGame)
  )

testGetPositionOccupied = TestCase
  (assertEqual
    "for getPosition with valid occupied space,"
    Black
    (name (Pair 0 0) $ \case
      Bound pos -> GL.getPosition pos inProgressGame
    )
  )

testGetPositionEmpty = TestCase
  (assertEqual
    "for getPosition with valid unoccupied space,"
    Empty
    (name (Pair 0 1) $ \case
      Bound pos -> GL.getPosition pos inProgressGame
    )
  )

testGetNeighborsFour =
  let pos = Pair 5 5
      n =
          (\p -> (+) <$> pos <*> p)
            <$> [(Pair 0 1), (Pair 0 (-1)), (Pair 1 0), (Pair (-1) 0)]
  in  TestCase
        (assertEqual
          "for getPosition with 4 spaces,"
          (sort n)
          (sort $ name pos $ \case
            Bound pos -> GL.getNeighbors pos inProgressGame
          )
        )

testGetNeighborsThree =
  let pos = Pair 0 5
      n   = [(Pair 1 5), (Pair 0 4), (Pair 0 6)]
  in  TestCase
        (assertEqual
          "for getPosition with 3 spaces,"
          (sort n)
          (sort $ name pos $ \case
            Bound pos -> GL.getNeighbors pos inProgressGame
          )
        )

testGetNeighborsTwo =
  let pos = Pair 0 0
      n   = [(Pair 0 1), (Pair 1 0)]
  in  TestCase
        (assertEqual
          "for getPosition with 2 spaces,"
          (sort n)
          (sort $ name pos $ \case
            Bound pos -> GL.getNeighbors pos inProgressGame
          )
        )

testisOccupiedBlack =
  let pos = Pair 0 0
  in  TestCase
        (assertEqual
          "for isOccupied when occupied"
          True
          (name pos $ \case
            Bound pos -> evalState (GL.isOccupied pos) inProgressGame
          )
        )

testisOccupiedEmpty =
  let pos = Pair 10 10
  in  TestCase
        (assertEqual
          "for isOccupied when empty"
          False
          (name pos $ \case
            Bound pos -> evalState (GL.isOccupied pos) inProgressGame
          )
        )

testAdjacentGroupNone =
  let pos = Pair 2 1
  in  TestCase
        (assertEqual
          "for testAdjacentMatching when none match"
          S.empty
          (name pos $ \case
            Bound pos -> GL.adjMatchingPos White pos k1Game
          )
        )

testAdjacentGroupThree =
  let pos = Pair 2 1
  in  TestCase
        (assertEqual
          "for testAdjacentMatching when three match"
          (S.fromList [Pair 2 0, Pair 1 1, Pair 2 2])
          (name pos $ \case
            Bound pos -> GL.adjMatchingPos Black pos k1Game
          )
        )

testPosToGroupOne =
  let pos   = Pair 0 0
      group = Group (S.fromList [Pair 0 1, Pair 1 0]) (S.singleton pos) Black
  in  TestCase
        (assertEqual
          "for posToGroup when singleton,"
          group
          (name pos $ \case
            Bound pos -> GL.posToGroup pos inProgressGame
          )
        )

testPosToGroupThree =
  let pos   = Pair 0 0
      group = Group (S.fromList [Pair 1 0, Pair 1 1, Pair 1 2, Pair 0 3])
                    (S.fromList [Pair 0 0, Pair 0 1, Pair 0 2])
                    Black
  in  TestCase
        (assertEqual
          "for posToGroup when three group,"
          group
          (name pos $ \case
            Bound pos -> GL.posToGroup pos groupGame
          )
        )

testPosToGroupSingletonBordering =
  let pos   = Pair 3 2
      group = Group (S.fromList [Pair 3 1, Pair 4 2, Pair 3 3])
                    (S.fromList [Pair 3 2])
                    White
  in  TestCase
        (assertEqual
          "for posToGroup when bordering singleton group,"
          group
          (name pos $ \case
            Bound pos -> GL.posToGroup pos k1Game
          )
        )

testSetPositionValid =
  let pos = Pair 1 1
  in  TestCase
        (assertEqual
          "for setPosition when valid"
          ( Right ()
          , inProgressGame & record %~ (:) inProgressGS
            { _board  = M.fromList [(Pair 0 0, Black), (Pair 1 1, White)]
            , _toPlay = Black
            }
          )
          (name pos $ \case
            Bound pos -> runState (runExceptT (GL.setPosition pos)) inProgressGame
          )
        )


testSetPositionOccupied =
  let pos = Pair 0 0
  in  TestCase
        (assertEqual
          "for setPosition when occupied"
          (Left Occupied, inProgressGame)
          (name pos $ \case
            Bound pos -> runState (runExceptT (GL.setPosition pos)) inProgressGame
          )
        )

testRevertWhenIllegalKo = TestCase
  (assertEqual "for revertWhenIllegalKo when Ko"
               (Left IllegalKo, k2Game)
               (runState (runExceptT GL.revertWhenIllegalKo) k3Game)
  )

testRevertWhenIllegalNoKo = TestCase
  (assertEqual "for revertWhenIllegalKo when No Ko"
               (Right (), k2Game)
               (runState (runExceptT GL.revertWhenIllegalKo) k2Game)
  )

testPlaceStoneValidNoKill = TestCase
  (assertEqual
    "for placeStone when Valid without a kill"
    ( Right NoKill
    , inProgressGame & record %~ (:) inProgressGS
      { _board  = M.fromList [(Pair 0 0, Black), (Pair 5 5, White)]
      , _toPlay = Black
      }
    )
    (name (Pair 5 5) $ \case
      Bound pos -> runState (runExceptT (GL.placeStone White pos)) inProgressGame
      Unbound   -> (Left OutOfBounds, newGame)
    )
  )

testPlaceStoneValidKill = TestCase
  (assertEqual
    "for placeStone when Valid with a kill"
    (Right Kill, k2Game)
    (name (Pair 3 1) $ \case
      Bound pos -> runState (runExceptT (GL.placeStone Black pos)) k1Game
      Unbound   -> (Left OutOfBounds, newGame)
    )
  )

testPlaceStoneValidSuicide = TestCase
  (assertEqual
    "for placeStone when Valid with a Suicide"
    (Left Suicide, suicideGame)
    (name (Pair 3 1) $ \case
      Bound pos -> runState (runExceptT (GL.placeStone Black pos)) suicideGame
      Unbound   -> (Left OutOfBounds, newGame)
    )
  )

testComputeScoreEmpty =
  TestCase
    (assertEqual
       "for ComputeScore with an empty board."
       (M.insert Empty (S.fromList (concat boardPositions)) M.empty)
       (GL.estimateTerritory newGame))

testComputeScoreBasicWhite =
  TestCase
    (assertEqual
       "for ComputeScore with a single surrounded white point."
       (let whiteTerritory =
              M.insert White (S.insert (Pair 3 1) S.empty) M.empty
            combinedTerritory =
              M.insert
                Empty
                (S.delete (Pair 3 1) (S.fromList (concat boardPositions)) S.\\ S.fromList (M.keys k1Board))
                whiteTerritory
         in combinedTerritory)
       (GL.estimateTerritory k1Game))

testComputeScoreBasicBlack =
  TestCase
    (assertEqual
       "for ComputeScore with a single surrounded black point."
       (let whiteTerritory =
              M.insert Black (S.insert (Pair 2 1) S.empty) M.empty
            combinedTerritory =
              M.insert
                Empty
                (S.delete (Pair 2 1) (S.fromList (concat boardPositions)) S.\\ S.fromList (M.keys k2Board))
                whiteTerritory
         in combinedTerritory)
       (GL.estimateTerritory k2Game))

setters = TestList
  [ TestLabel "SetPositionValid"           testSetPositionValid
  , TestLabel "SetPositionOccupied"        testSetPositionOccupied
  , TestLabel "testRevertWhenIllegalKo"    testRevertWhenIllegalKo
  , TestLabel "testRevertWhenIllegalNoKo"  testRevertWhenIllegalNoKo
  , TestLabel "testPlaceStoneValidKill"    testPlaceStoneValidKill
  , TestLabel "testPlaceStoneValidNoKill"  testPlaceStoneValidNoKill
  , TestLabel "testPlaceStoneValidSuicide" testPlaceStoneValidSuicide
  ]


getters = TestList
  [ TestLabel "GetCurrentBoard"        testCurrentBoard
  , TestLabel "GetNextToPlay"          testNextToPlay
  , TestLabel "GetPositionOccupied"    testGetPositionOccupied
  , TestLabel "GetPositionEmpty"       testGetPositionEmpty
  , TestLabel "GetNeighborsFour"       testGetNeighborsFour
  , TestLabel "GetNeighborsThree"      testGetNeighborsThree
  , TestLabel "GetNeighborsTwo"        testGetNeighborsTwo
  , TestLabel "testisOccupiedBlack"    testisOccupiedBlack
  , TestLabel "testisOccupiedEmpty"    testisOccupiedEmpty
  , TestLabel "testAdjacentGroupNone"  testAdjacentGroupNone
  , TestLabel "testAdjacentGroupThree" testAdjacentGroupThree
  , TestLabel "testPosToGroupOne"      testPosToGroupOne
  , TestLabel "testPosToGroupThree"    testPosToGroupThree
  , TestLabel "testPosToGroupSingletonBordering"
              testPosToGroupSingletonBordering
  , TestLabel "testComputeScoreEmpty" testComputeScoreEmpty
  , TestLabel "testComputeScoreBasicWhite" testComputeScoreBasicWhite
  , TestLabel "testComputeScoreBasicBlack" testComputeScoreBasicBlack
  ]
