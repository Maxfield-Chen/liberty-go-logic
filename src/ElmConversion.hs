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


module ElmConversion where

import           Control.Lens                hiding (Empty)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Types            hiding (Pair, defaultOptions)
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Generics.SOP                as SOP
import           GHC.Generics

import qualified Data.Text                   as T
import qualified Data.Text.IO                as I
import qualified Language.Elm.Expression     as Expression
import qualified Language.Elm.Pretty         as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Type           as Type
import           Language.Haskell.To.Elm
import           System.IO

import           Game

sourceDir = "./elm/src/"

modules = do
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

  Pretty.modules definitions

writeModules =
  forM_ (HashMap.toList modules) $ \(moduleList, contents) -> do
    let modulePath =
          mconcat $
          zipWith
            (\p s -> [p, s])
            moduleList
           ((replicate (length moduleList - 1) "/") ++ [".elm"])
        filePath = mconcat $ [sourceDir] ++ modulePath
    handle <- openFile (T.unpack filePath) WriteMode
    hPrint handle contents
