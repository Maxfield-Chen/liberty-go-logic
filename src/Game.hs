{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}


module Game where

import           Control.Lens               (makeLenses)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 ()
import           Data.Aeson.Types           hiding (Pair, defaultOptions)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           GHC.Generics



-- TODO: Refactor modules based on type rather than functionality

data Pair a =
  Pair a a
  deriving ( Show
           , Read
           , Eq
           , Ord
           , Generic
           , ToJSON
           , FromJSON
           , ToJSONKey
           , FromJSONKey
           )

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

type Position = Pair Int

data GameStatus
  = GameRejected
  | GameProposed
  | InProgress
  | BlackPassed
  | WhitePassed
  | Counting
  | BlackProposed
  | WhiteProposed
  | TerritoryAccepted
  deriving ( Eq
           , Show
           , Read
           , Generic
           , ToJSON
           , FromJSON
           )

data Outcome
  = NoKill
  | Kill
  deriving ( Eq
           , Show
           , Generic
           , ToJSON
           , FromJSON
           )

data MoveError
  = IllegalPlayer
  | NoBoard
  | IllegalKo
  | Suicide
  | OutOfBounds
  | Occupied
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Space
  = Black
  | White
  | Empty
  deriving ( Show
           , Read
           , Eq
           , Ord
           , Generic
           , ToJSON
           , FromJSON
           , ToJSONKey
           , FromJSONKey
           )


type Board = M.Map Position Space
type Territory = M.Map Space (S.Set Position)
type Score = (Double, Double)

data Game =
  Game
    { _boardSize      :: Int
    , _record         :: [GameState]
    , _komi           :: Double
    , _finalTerritory :: Territory
    , _finalScore     :: Score
    , _status         :: GameStatus
    }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)


data GameState =
  GameState
    { _board    :: Board
    , _toPlay   :: Space
    , _captures :: M.Map Space Int
    }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data Group =
  Group
    { _liberties :: S.Set Position
    , _members   :: S.Set Position
    , _player    :: Space
    }
  deriving (Show, Eq, Generic, ToJSON)

data Area =
  Area
    { _bordersBlack      :: Bool
    , _bordersWhite      :: Bool
    , _enclosedPositions :: S.Set Position
    }
  deriving (Show, Eq)

makeLenses ''Game
makeLenses ''GameState
makeLenses ''Group
makeLenses ''Area

type ExceptGame a = ExceptT MoveError (State Game) a

standardKomi = 5.5
standardBoardSize = 19
boardPositions = [ [ Pair x y | x <- [0 .. 18 :: Int] ] | y <- [0 .. 18 :: Int] ]
newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game standardBoardSize [newGameState] standardKomi M.empty (0,0) GameProposed

bounded :: (Num n, Ord n) => n -> Pair n -> Bool
bounded bs (Pair x y) = x >= 0 && y >= 0 && x < bs && y < bs
