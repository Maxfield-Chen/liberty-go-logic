{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE RoleAnnotations     #-}

module Proofs where

import           Prelude
import           Theory.Lists
import           Theory.Named
import           Theory.Equality
import           Logic.Proof
import           Data.The
import           Data.Refined
import           Data.Coerce
import           Logic.Implicit
import           Board

-- Predicates about the possible shapes of positions
data IsBound n
data IsUnbound n

data BoundedCase n val where
  Bound_ ::Fact (IsBound val) => (n ~~  val) -> BoundedCase n val
  Unbound_ ::Fact (IsUnbound val) => BoundedCase n val

pattern Bound ::  Fact (IsBound val) => (Position ~~ val) -> (Position ~~ val)
--brittany-disable-next-binding
pattern Bound val <- (classifyBound -> Bound_ val)

pattern Unbound :: Fact (IsUnbound val) => (Position ~~ val)
--brittany-disable-next-binding
pattern Unbound <- (classifyBound -> Unbound_ )

-- TODO: After game refactor to avoid excessive state, pull in isWithinBounds
classifyBound :: forall val . (Position ~~ val) -> BoundedCase Position val
classifyBound val | True      = note (axiom :: Proof (IsBound val)) (Bound_ val)
                  | otherwise = note (axiom :: Proof (IsUnbound val)) Unbound_

