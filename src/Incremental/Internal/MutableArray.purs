module Incremental.Internal.MutableArray where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Unsafe.Coerce (unsafeCoerce)

foreign import data MutableArray :: Type -> Type

foreign import empty :: forall a. Effect (MutableArray a)

unsafeToArray :: forall a. MutableArray a -> Array a
unsafeToArray x = unsafeCoerce x

foreign import push :: forall a. EffectFn2 (MutableArray a) a Unit
foreign import length :: forall a. EffectFn1 (MutableArray a) Int
