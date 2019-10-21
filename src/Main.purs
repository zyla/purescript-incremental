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

  a_a <- runEffectFn3 I.map2 (mkFn2 (<>)) (I.readVar a) (I.readVar a)
  runEffectFn2 Node.annotate a_a "a <> a"
  runEffectFn2 I.addObserver a_a (logWith "a <> a")

  -- should have height 2
  a_b_b <- runEffectFn3 I.map2 (mkFn2 (<>)) a_b (I.readVar b)
  runEffectFn2 Node.annotate a_b_b "(a <> b) <> b"
  runEffectFn2 I.addObserver a_b_b (logWith "(a <> b) <> b")

  log "---\na <- 'a1'; stabilize"
  runEffectFn2 I.setVar a "a1"
  I.stabilize

  log "---\nb <- 'b1'; stabilize"
  runEffectFn2 I.setVar b "b1"
  I.stabilize

  log "---\na <- 'a2'; b <- 'b2'; stabilize"
  runEffectFn2 I.setVar a "a2"
  runEffectFn2 I.setVar b "b2"
  I.stabilize

  c <- runEffectFn1 I.newVar (I.readVar a)
  runEffectFn2 Node.annotate (I.readVar c) "c"
  bnd <- runEffectFn2 I.bind_ (I.readVar c) identity
  runEffectFn2 Node.annotate bnd "join c"
  runEffectFn2 I.addObserver bnd (logWith "join c")

  join_c_b <- runEffectFn3 I.map2 (mkFn2 (<>)) bnd (I.readVar b)
  runEffectFn2 Node.annotate join_c_b "join c <> b"
  runEffectFn2 I.addObserver join_c_b (logWith "join c <> b")

  log "---\na <- 'a3'; stabilize"
  runEffectFn2 I.setVar a "a3"
  I.stabilize

  log "---\nc <- (a <> b) <> b; stabilize"
  runEffectFn2 I.setVar c a_b_b
  I.stabilize

  log "---\nc <- a; stabilize"
  runEffectFn2 I.setVar c (I.readVar a)
  I.stabilize

  log "---\nb <- 'b3'; stabilize"
  runEffectFn2 I.setVar b "b3"
  I.stabilize

logWith :: forall a. Show a => String -> EffectFn1 a Unit
logWith s = mkEffectFn1 \x -> log (s <> ": " <> show x)
