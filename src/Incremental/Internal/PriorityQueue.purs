module Incremental.Internal.PriorityQueue where

import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4)
import Incremental.Internal.Node (Any, Field, Mutable)
import Incremental.Internal.Optional (Optional)

foreign import data PQ :: Type -> Type

foreign import new :: forall a.
  EffectFn4
    (Optional Any)                 -- Optional.none
    (Field a Mutable Int)          -- Priority
    (Field a Mutable Boolean)      -- Is the entry present in this queue?
    (Field a Mutable (Optional a)) -- Next entry with the same priority
  (PQ a)

foreign import add :: forall a. EffectFn2 (PQ a) a Boolean
foreign import remove :: forall a. EffectFn2 (PQ a) a Boolean
foreign import isNonEmpty :: forall a. EffectFn1 (PQ a) Boolean
foreign import removeMin :: forall a. EffectFn1 (PQ a) (Optional a)