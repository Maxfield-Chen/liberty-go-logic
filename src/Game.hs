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

import           Control.Lens                hiding (Empty)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Types            hiding (Pair, defaultOptions)
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Generics.SOP                as SOP
import           GHC.Generics

import qualified Language.Elm.Expression     as Expression
import qualified Language.Elm.Pretty         as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Type           as Type
import           Language.Haskell.To.Elm


-- TODO: Refactor modules based on type rather than functionality
--       also consider moving all Elm type instances to their own file?
--       unsure what the best practice is there.

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
           , SOP.Generic
           , SOP.HasDatatypeInfo
           )

instance HasElmType Pair where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Pair defaultOptions "Api.Pair.Pair"

instance HasElmDecoder Aeson.Value Pair where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Pair defaultOptions Aeson.defaultOptions "Api.Pair.decoder"

instance HasElmEncoder Aeson.Value Pair where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Pair defaultOptions Aeson.defaultOptions "Api.Pair.encoder"

instance HasElmType a => HasElmType (Pair a) where
  elmType = Type.apps (elmType @Pair) [elmType @a]

instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (Pair a) where
  elmDecoder = Expression.apps (elmDecoder @Aeson.Value @Pair) [elmDecoder @Aeson.Value @a]

instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (Pair a) where
  elmEncoder = Expression.apps (elmEncoder @Aeson.Value @Pair) [elmEncoder @Aeson.Value @a]

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
  | CountingProposed
  | CountingAccepted
  | TerritoryProposed
  | TerritoryAccepted
  deriving ( Eq
           , Show
           , Read
           , Generic
           , ToJSON
           , FromJSON
           , SOP.Generic
           , SOP.HasDatatypeInfo
           )

instance HasElmType GameStatus where
  elmDefinition =
    Just $ deriveElmTypeDefinition @GameStatus defaultOptions "Api.GameStatus.GameStatus"

instance HasElmDecoder Aeson.Value GameStatus where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @GameStatus defaultOptions Aeson.defaultOptions "Api.GameStatus.decoder"

instance HasElmEncoder Aeson.Value GameStatus where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @GameStatus defaultOptions Aeson.defaultOptions "Api.GameStatus.encoder"

data Outcome
  = NoKill
  | Kill
  deriving ( Eq
           , Show
           , Generic
           , ToJSON
           , FromJSON
           , SOP.Generic
           , SOP.HasDatatypeInfo
           )

instance HasElmType Outcome where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Outcome defaultOptions "Api.Outcome.Outcome"

instance HasElmDecoder Aeson.Value Outcome where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Outcome defaultOptions Aeson.defaultOptions "Api.Outcome.decoder"

instance HasElmEncoder Aeson.Value Outcome where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Outcome defaultOptions Aeson.defaultOptions "Api.Outcome.encoder"

data MoveError
  = IllegalPlayer
  | NoBoard
  | IllegalKo
  | Suicide
  | OutOfBounds
  | Occupied
  deriving (Show, Eq, Generic, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType MoveError where
  elmDefinition =
    Just $ deriveElmTypeDefinition @MoveError defaultOptions "Api.MoveError.MoveError"

instance HasElmDecoder Aeson.Value MoveError where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @MoveError defaultOptions Aeson.defaultOptions "Api.MoveError.decoder"

instance HasElmEncoder Aeson.Value MoveError where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @MoveError defaultOptions Aeson.defaultOptions "Api.MoveError.encoder"

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
           , SOP.Generic
           , SOP.HasDatatypeInfo
           )

instance HasElmType Space where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Space defaultOptions "Api.Space.Space"

instance HasElmDecoder Aeson.Value Space where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Space defaultOptions Aeson.defaultOptions "Api.Space.decoder"

instance HasElmEncoder Aeson.Value Space where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Space defaultOptions Aeson.defaultOptions "Api.Space.encoder"

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
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType Game where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Game defaultOptions "Api.Game.Game"

instance HasElmDecoder Aeson.Value Game where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Game defaultOptions Aeson.defaultOptions "Api.Game.decoder"

instance HasElmEncoder Aeson.Value Game where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Game defaultOptions Aeson.defaultOptions "Api.Game.encoder"

data GameState =
  GameState
    { _board    :: Board
    , _toPlay   :: Space
    , _captures :: M.Map Space Int
    }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType GameState where
  elmDefinition =
    Just $ deriveElmTypeDefinition @GameState defaultOptions "Api.GameState.GameState"

instance HasElmDecoder Aeson.Value GameState where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @GameState defaultOptions Aeson.defaultOptions "Api.GameState.decoder"

instance HasElmEncoder Aeson.Value GameState where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @GameState defaultOptions Aeson.defaultOptions "Api.GameState.encoder"

data Group =
  Group
    { _liberties :: S.Set Position
    , _members   :: S.Set Position
    , _player    :: Space
    }
  deriving (Show, Eq, Generic, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType Group where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Group defaultOptions "Api.Group.Group"

instance HasElmDecoder Aeson.Value Group where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Group defaultOptions Aeson.defaultOptions "Api.Group.decoder"

instance HasElmEncoder Aeson.Value Group where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Group defaultOptions Aeson.defaultOptions "Api.Group.encoder"

    -- TODO: Remove this code when https://github.com/folq/haskell-to-elm/pull/4
    --       gets merged into master.
    -- | @Set@ is encoded as a @List@ for custom types.
instance HasElmType a => HasElmType (S.Set a) where
  elmType =
    Type.App "List.List" (elmType @a)

-- | @Set@ is encoded as a @List@ for custom types.
instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (S.Set a) where
  elmEncoder =
    Expression.App "Json.Encode.list" (elmEncoder @Aeson.Value @a)

-- | @Set@ is decoded as a @List@ for custom types.
instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (S.Set a) where
  elmDecoder =
    Expression.App "Json.Decode.list" (elmDecoder @Aeson.Value @a)

instance (Aeson.ToJSONKey k, Aeson.FromJSONKey k, HasElmType v) => HasElmType (M.Map k v) where
  elmType =
    Type.App (Type.App "Dict.Dict" (elmType @Text)) (elmType @v)

instance (Aeson.ToJSONKey k, Aeson.FromJSONKey k, HasElmEncoder Aeson.Value v) => HasElmEncoder Aeson.Value (M.Map k v) where
  elmEncoder =
    Expression.App
      (Expression.App "Json.Encode.dict" "Basics.identity")
        (elmEncoder @Aeson.Value @v)

instance (Aeson.ToJSONKey k, Aeson.FromJSONKey k, HasElmDecoder Aeson.Value v) => HasElmDecoder Aeson.Value (M.Map k v) where
  elmDecoder =
    Expression.App "Json.Decode.dict" (elmDecoder @Aeson.Value @v)

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
