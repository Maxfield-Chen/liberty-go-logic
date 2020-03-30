{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
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


module Build where

import           Control.Lens               hiding (Empty)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import GHC.Generics
import qualified Generics.SOP as SOP
import Data.Aeson.Types hiding (Pair, defaultOptions)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap

import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Type as Type
import qualified Language.Elm.Expression as Expression
import Language.Haskell.To.Elm

import Game


createElmDefinitions :: IO ()
createElmDefinitions = do
  let
    definitions =
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @Pair) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @GameStatus) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @Outcome) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @MoveError) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @Space) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @Game) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @GameState) ++
      (Simplification.simplifyDefinition <$>
        jsonDefinitions @Group)

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
