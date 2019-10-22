module Incremental.Internal.Mutable where

import Prelude

import Data.Function.Uncurried (Fn2)
import Effect.Uncurried (EffectFn2, EffectFn3)

foreign import kind Mutability
foreign import data Mutable :: Mutability
foreign import data Immutable :: Mutability

newtype Field s (m :: Mutability) a = Field String

foreign import _get :: forall s a. Fn2 (Field s Immutable a) s a
foreign import _read :: forall s a. EffectFn2 (Field s Mutable a) s a
foreign import _write :: forall s a. EffectFn3 (Field s Mutable a) s a Unit

foreign import data Any :: Type
