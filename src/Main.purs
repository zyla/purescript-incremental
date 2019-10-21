module Main where

import Prelude

import Data.Function.Uncurried (mkFn2)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Incremental.Internal as I
import Incremental.Internal.Node as Node

main :: Effect Unit
main = do

  a <- runEffectFn1 I.newVar "a0"
  runEffectFn2 Node.annotate (I.readVar a) "a"
  b <- runEffectFn1 I.newVar "b0"
  runEffectFn2 Node.annotate (I.readVar b) "b"

  runEffectFn2 I.addObserver (I.readVar a) (logWith "a")
  runEffectFn2 I.addObserver (I.readVar b) (logWith "b")

  a_b <- runEffectFn3 I.map2 (mkFn2 (<>)) (I.readVar a) (I.readVar b)
  runEffectFn2 Node.annotate a_b "a <> b"
  runEffectFn2 I.addObserver a_b (logWith "a <> b")

  log "a <- 'a1'; stabilize"
  runEffectFn2 I.setVar a "a1"
  I.stabilize

  log "b <- 'b1'; stabilize"
  runEffectFn2 I.setVar b "b1"
  I.stabilize

  log "a <- 'a2'; b <- 'b2'; stabilize"
  runEffectFn2 I.setVar a "a2"
  runEffectFn2 I.setVar b "b2"
  I.stabilize

logWith :: forall a. Show a => String -> EffectFn1 a Unit
logWith s = mkEffectFn1 \x -> log (s <> ": " <> show x)
