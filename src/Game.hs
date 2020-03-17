{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE RoleAnnotations  #-}


module Game where

import           Control.Lens               hiding (Empty)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import GHC.Generics
import Data.Aeson.Types hiding (Pair)


data Pair a = Pair a a deriving (Show, Read, Eq, Ord, Generic)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

type Position = Pair Int
instance ToJSON (Game.Pair Int)
instance FromJSON (Game.Pair Int)
instance ToJSONKey (Game.Pair Int)
instance FromJSONKey (Game.Pair Int)

data GameStatus = GameRejected | GameProposed | InProgress | CountingProposed | CountingAccepted | TerritoryProposed | TerritoryAccepted deriving (Eq, Show, Read, Generic)
instance ToJSON GameStatus
instance FromJSON GameStatus

data Outcome = NoKill | Kill deriving (Eq, Show, Generic)
instance ToJSON Outcome

data MoveError = IllegalPlayer |  NoBoard |  IllegalKo | Suicide | OutOfBounds | Occupied deriving (Show, Eq, Generic)
instance ToJSON MoveError

data Space = Black | White | Empty deriving (Show, Read, Eq, Ord, Generic)
instance ToJSON Space
instance FromJSON Space
instance FromJSONKey Space
instance ToJSONKey Space

type Board = M.Map Position Space
type Territory = M.Map Space (S.Set Position)
type Score = (Double, Double)

data Game =
  Game
    { _boardSize :: Int
    , _record :: [GameState]
    , _komi :: Double
    , _finalTerritory :: Territory
    , _finalScore :: Score
    , _status :: GameStatus
    }
  deriving (Show, Read, Eq, Generic)
instance ToJSON Game
instance FromJSON Game

data Area =
  Area
    { _bordersBlack :: Bool
    , _bordersWhite :: Bool
    , _enclosedPositions :: S.Set Position
    }
  deriving (Show, Eq)

data GameState = GameState { _board    :: Board
                           , _toPlay   :: Space
                           , _captures :: M.Map Space Int} deriving (Show, Read, Eq, Generic)
instance ToJSON GameState
instance FromJSON GameState

data Group = Group { _liberties :: S.Set Position
                   , _members   :: S.Set Position
                   , _player    :: Space} deriving (Show, Eq, Generic)
instance ToJSON Group

makeLenses ''Game
makeLenses ''GameState
makeLenses ''Group
makeLenses ''Area

type ExceptGame a = ExceptT MoveError (State Game) a

standardKomi = 5.5
standardBoardSize = 19
boardPositions = [ [ Pair x y | x <- [0 .. 18 :: Int] ] | y <- [0 .. 18 :: Int] ]
newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game standardBoardSize [newGameState] standardKomi M.empty (0,0) InProgress

bounded :: (Num n, Ord n) => n -> Pair n -> Bool
bounded bs (Pair x y) = x >= 0 && y >= 0 && x < bs && y < bs
