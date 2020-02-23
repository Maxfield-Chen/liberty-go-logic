{-# LANGUAGE TemplateHaskell #-}

module Game where

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

