module Incremental.Internal.Node
  ( module Incremental.Internal.Node
  , module Incremental.Internal.Mutable
  ) where

import Prelude

import Effect (Effect)
import Incremental.Internal.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn6, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn6)
import Effect.Unsafe (unsafePerformEffect)
import Incremental.Internal.Global (globalCurrentStabilizationNum)
import Incremental.Internal.MutableArray (MutableArray)
import Incremental.Internal.MutableArray as MutableArray
import Incremental.Internal.Mutable (Any, Field(..), Immutable, Mutable, _get, _read, _write)
import Incremental.Internal.Optional (Optional)
import Incremental.Internal.Optional as Optional
import Unsafe.Coerce (unsafeCoerce)

foreign import data Node :: Type -> Type

-- * Node fields

_source :: forall a. Field (Node a) Immutable (Source a)
_source = Field "source"

foreign import get_source :: forall a. EffectFn1 (Node a) (Source a)

_dependents :: forall a. Field (Node a) Immutable (MutableArray SomeNode)
_dependents = Field "dependents"

foreign import get_dependents :: forall a. EffectFn1 (Node a) (MutableArray SomeNode)

type Observer a = EffectFn1 a Unit

_observers :: forall a. Field (Node a) Immutable (MutableArray (Observer a))
_observers = Field "observers"

foreign import get_observers :: forall a. EffectFn1 (Node a) (MutableArray (Observer a))

_value :: forall a. Field (Node a) Mutable (Optional a)
_value = Field "value"

foreign import get_value :: forall a. EffectFn1 (Node a) (Optional a)
foreign import set_value :: forall a. EffectFn2 (Node a) (Optional a) Unit

_height :: forall a. Field (Node a) Mutable Int
_height = Field "height"

foreign import get_height :: forall a. EffectFn1 (Node a) Int
foreign import set_height :: forall a. EffectFn2 (Node a) Int Unit

_adjustedHeight :: forall a. Field (Node a) Mutable Int
_adjustedHeight = Field "adjustedHeight"

foreign import get_adjustedHeight :: forall a. EffectFn1 (Node a) Int
foreign import set_adjustedHeight :: forall a. EffectFn2 (Node a) Int Unit

_inRecomputeQueue :: forall a. Field (Node a) Mutable Boolean
_inRecomputeQueue = Field "inRecomputeQueue"

_nextInRecomputeQueue :: forall a. Field (Node a) Mutable (Optional SomeNode)
_nextInRecomputeQueue = Field "nextInRecomputeQueue"

_name :: forall a. Field (Node a) Mutable String
_name = Field "name"

foreign import get_name :: forall a. EffectFn1 (Node a) String
foreign import set_name :: forall a. EffectFn2 (Node a) String Unit

-- Note: this is not just a freshness timestamp!
-- This should be only set when the node, treated as an Event, fires.
_changedAt :: forall a. Field (Node a) Mutable Int
_changedAt = Field "changedAt"

foreign import get_changedAt :: forall a. EffectFn1 (Node a) Int
foreign import set_changedAt :: forall a. EffectFn2 (Node a) Int Unit

foreign import _new ::
  forall a.
  EffectFn6
    (Optional Any) -- Optional.none
    (Source a)
    (MutableArray SomeNode)
    (MutableArray (Observer a))
    (Optional a)
    Int
  (Node a)

-- * Node source

type Source a =
  { compute :: EffectFn1 (Node a) (Optional a)
    -- Compute the node value, if none is returned then we don't change value and don't propagate.
  , dependencies :: Effect (Array SomeNode)
  }

-- * Existential

type SomeNode = Node Any

toSomeNode :: forall a. Node a -> SomeNode
toSomeNode = unsafeCoerce

toSomeNodeArray :: forall a. Array (Node a) -> Array SomeNode
toSomeNodeArray = unsafeCoerce

-- * Creation

create :: forall a. EffectFn1 (Source a) (Node a)
create = mkEffectFn1 \source -> do
  dependents <- MutableArray.empty
  observers <- MutableArray.empty
  runEffectFn6 _new
    Optional.none
    source
    dependents
    observers
    Optional.none -- value
    0             -- height

-- * Utils

refcount :: forall a. EffectFn1 (Node a) Int
refcount = mkEffectFn1 \node -> do
  observers <- runEffectFn1 get_observers node
  numDependents <- runEffectFn1 MutableArray.length observers
  dependents <- runEffectFn1 get_dependents node
  numObservers <- runEffectFn1 MutableArray.length dependents
  pure (numDependents + numObservers)

valueExc :: forall a. EffectFn1 (Node a) a
valueExc = mkEffectFn1 \node -> do
  value_opt <- runEffectFn1 get_value node
  pure (Optional.fromSome value_opt)

annotate :: forall a. EffectFn2 (Node a) String Unit
annotate = mkEffectFn2 \node nm ->
  runEffectFn3 _write _name node nm

name :: forall a. EffectFn1 (Node a) String
name = mkEffectFn1 \node ->
  runEffectFn2 _read _name node

name' :: forall a. Node a -> String
name' node = unsafePerformEffect (runEffectFn1 name node)

isChangingInCurrentStabilization :: forall a. EffectFn1 (Node a) Boolean
isChangingInCurrentStabilization = mkEffectFn1 \node -> do
  currentStabilizationNum <- runEffectFn1 Ref.read globalCurrentStabilizationNum
  changedAt <- runEffectFn1 get_changedAt node
  pure (changedAt == currentStabilizationNum)
