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
import           Control.Lens
import           Game

--TODO: Delete this code after integrating proofs into the Game DS
-- Predicates about the possible shapes of States
data HasState game
data NoState game

data StateCase game val where
  State_ ::Fact (HasState val) => (game ~~ val) -> StateCase game val
  NoState_ ::Fact (NoState val) => StateCase game val

pattern State :: Fact (HasState val) => (Game ~~ val) -> (Game ~~ val)
--brittany-disable-next-binding
pattern State val <- (classifyState -> State_ val)

pattern NoState :: Fact (NoState val) => (Game ~~ val)
--brittany-disable-next-binding
pattern NoState <- (classifyState -> NoState_)

classifyState :: forall val . (Game ~~ val) -> StateCase Game val
classifyState val = case view record (the val) of
  (_ : _) -> note (axiom :: Proof (HasState val)) (State_ val)
  _       -> note (axiom :: Proof (NoState val)) NoState_


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

-- TODO: Refactor the use current game size rather than hardcoded value
classifyBound :: forall val . (Position ~~ val) -> BoundedCase Position val
classifyBound val
  | bounded 19 (the val) = note (axiom :: Proof (IsBound val)) (Bound_ val)
  | otherwise            = note (axiom :: Proof (IsUnbound val)) Unbound_

