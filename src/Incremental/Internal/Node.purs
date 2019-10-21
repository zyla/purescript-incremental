module Incremental.Internal.Node where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn6, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn6)
import Incremental.Internal.MutableArray (MutableArray)
import Incremental.Internal.MutableArray as MutableArray
import Incremental.Internal.Optional (Optional)
import Incremental.Internal.Optional as Optional
import Unsafe.Coerce (unsafeCoerce)

foreign import data Node :: Type -> Type

-- * Node fields

foreign import kind Mutability
foreign import data Mutable :: Mutability
foreign import data Immutable :: Mutability

newtype Field s (m :: Mutability) a = Field String

_source :: forall a. Field (Node a) Immutable (Source a)
_source = Field "source"

_dependents :: forall a. Field (Node a) Immutable (MutableArray SomeNode)
_dependents = Field "dependents"

type Observer a = EffectFn1 a Unit

_observers :: forall a. Field (Node a) Immutable (MutableArray (Observer a))
_observers = Field "observers"

_value :: forall a. Field (Node a) Mutable (Optional a)
_value = Field "value"

_height :: forall a. Field (Node a) Mutable Int
_height = Field "height"

_inRecomputeQueue :: forall a. Field (Node a) Mutable Boolean
_inRecomputeQueue = Field "inRecomputeQueue"

_nextInRecomputeQueue :: forall a. Field (Node a) Mutable (Optional SomeNode)
_nextInRecomputeQueue = Field "nextInRecomputeQueue"

_name :: forall a. Field (Node a) Mutable String
_name = Field "name"

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

foreign import _get :: forall s a. Fn2 (Field s Immutable a) s a
foreign import _read :: forall s a. EffectFn2 (Field s Mutable a) s a
foreign import _write :: forall s a. EffectFn3 (Field s Mutable a) s a Unit

-- * Node source

type Source a =
  { compute :: EffectFn1 (Node a) a
  , dependencies :: Effect (Array SomeNode)
  }

-- * Existential

foreign import data Any :: Type

type SomeNode = Node Any

toSomeNode :: forall a. Node a -> SomeNode
toSomeNode = unsafeCoerce

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
  numDependents <- runEffectFn1 MutableArray.length (runFn2 _get _dependents node)
  numObservers <- runEffectFn1 MutableArray.length (runFn2 _get _observers node)
  pure (numDependents + numObservers)

valueExc :: forall a. EffectFn1 (Node a) a
valueExc = mkEffectFn1 \node -> do
  value_opt <- runEffectFn2 _read _value node
  pure (Optional.fromSome value_opt)

annotate :: forall a. EffectFn2 (Node a) String Unit
annotate = mkEffectFn2 \node nm ->
  runEffectFn3 _write _name node nm

name :: forall a. EffectFn1 (Node a) String
name = mkEffectFn1 \node ->
  runEffectFn2 _read _name node
