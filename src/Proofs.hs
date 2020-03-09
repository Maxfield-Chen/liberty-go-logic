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

module Proofs where

import           Data.The
import           Game
import           Logic.Implicit
import           Logic.Proof
import           Prelude
import           Theory.Named


-- Predicates about the possible shapes of positions
data IsBound n
data IsUnbound n

data BoundedCase n val where
  Bound_ ::Fact (IsBound val) => (n ~~ val) -> BoundedCase n val
  Unbound_ ::Fact (IsUnbound val) => BoundedCase n val

pattern Bound ::  (Num n, Ord n) => Fact (IsBound val) => (Pair n ~~ val) -> (Pair n ~~ val)
--brittany-disable-next-binding
pattern Bound val <- (classifyBound -> Bound_ val)

pattern Unbound :: (Num n, Ord n) => Fact (IsUnbound val) => (Pair n ~~ val)
--brittany-disable-next-binding
pattern Unbound <- (classifyBound -> Unbound_ )

-- TODO: Figure out why this gives me an error when I specify 19 as an external variable
-- Relates to Int vs nonspecific ord / num type
classifyBound :: forall n val . (Num n, Ord n) =>  (Pair n ~~ val) -> BoundedCase (Pair n) val
classifyBound val
  | bounded 19 (the val) = note (axiom :: Proof (IsBound val)) (Bound_ val)
  | otherwise            = note (axiom :: Proof (IsUnbound val)) Unbound_
