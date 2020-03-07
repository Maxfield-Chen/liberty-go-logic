{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import           Control.Lens               hiding (Empty)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import GHC.Generics
import Data.Aeson hiding (Pair)
import Data.Aeson.Types hiding (Pair)


data Pair a = Pair a a deriving (Show, Eq, Ord, Generic)

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

data Outcome = NoKill | Kill deriving (Eq, Show, Generic)
instance ToJSON Outcome

data MoveError = IllegalPlayer |  NoBoard |  IllegalKo | Suicide | OutOfBounds | Occupied deriving (Show, Eq, Generic)
instance ToJSON MoveError

data Space = Black | White | Empty deriving (Show, Eq, Ord, Generic)
instance ToJSON Space
instance FromJSON Space
instance FromJSONKey Space
instance ToJSONKey Space

type Board = M.Map Position Space

data Result = Result { game :: Game
                     , score :: M.Map Space Double}

data Game = Game { _boardSize :: Int
                 , _record    :: [GameState]} deriving (Show, Eq, Generic)
instance ToJSON Game
instance FromJSON Game

data GameState = GameState { _board    :: Board
                           , _toPlay   :: Space
                           , _captures :: M.Map Space Int} deriving (Show, Eq, Generic)
instance ToJSON GameState
instance FromJSON GameState

data Group = Group { _liberties :: S.Set Position
                   , _members   :: S.Set Position
                   , _player    :: Space} deriving (Show, Eq, Generic)
instance ToJSON Group

makeLenses ''Game
makeLenses ''GameState
makeLenses ''Group

type ExceptGame a = ExceptT MoveError (State Game) a

standardBoardSize = 19
newGameState = GameState M.empty Black (M.fromList [(Black, 0), (White, 0)])
newGame = Game standardBoardSize [newGameState]

bounded :: (Num n, Ord n) => n -> Pair n -> Bool
bounded bs (Pair x y) = x >= 0 && y >= 0 && x < bs && y < bs
