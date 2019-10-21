module Incremental.Internal where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect, foreachE, whileE)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import Incremental.Internal.MutableArray as MutableArray
import Incremental.Internal.Node (Node, SomeNode, Observer, toSomeNode)
import Incremental.Internal.Node as Node
import Incremental.Internal.Optional as Optional
import Incremental.Internal.PriorityQueue as PQ
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

newtype Var a = Var (Node a)

globalRecomputeQueue :: PQ.PQ SomeNode
globalRecomputeQueue = unsafePerformEffect $
  runEffectFn4 PQ.new
    Optional.none
    Node._height
    Node._inRecomputeQueue
    Node._nextInRecomputeQueue

newVar :: forall a. EffectFn1 a (Var a)
newVar = mkEffectFn1 \val -> do
  node <- runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node ->
        runEffectFn1 Node.valueExc node
    , dependencies: pure []
    }
  runEffectFn3 Node._write Node._value node (Optional.some val)
  pure (Var node)

setVar :: forall a. EffectFn2 (Var a) a Unit
setVar = mkEffectFn2 \(Var node) val -> do
  runEffectFn3 Node._write Node._value node (Optional.some val)
  _ <- runEffectFn2 PQ.add globalRecomputeQueue (toSomeNode node)
  pure unit

readVar :: forall a. Var a -> Node a
readVar (Var x) = x

-- * Observers and dependents

addObserver :: forall a. EffectFn2 (Node a) (Observer a) Unit
addObserver = mkEffectFn2 \node observer -> do
  runEffectFn2 MutableArray.push (runFn2 Node._get Node._observers node) observer
  runEffectFn1 handleRefcountChange node

addDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
addDependent = mkEffectFn2 \node dependent -> do
  -- FIXME: Broken if the same dependent is added twice!
  runEffectFn2 MutableArray.push (runFn2 Node._get Node._dependents node) dependent
  runEffectFn1 handleRefcountChange node

handleRefcountChange :: forall a. EffectFn1 (Node a) Unit
handleRefcountChange = mkEffectFn1 \node -> do
  refcount <- runEffectFn1 Node.refcount node
  if refcount == 1 then
    runEffectFn1 connect node
  else if refcount == 0 then
    runEffectFn1 disconnect node
  else
    pure unit

connect :: forall a. EffectFn1 (Node a) Unit
connect = mkEffectFn1 \node -> do
  let source = runFn2 Node._get Node._source node
  dependencies <- source.dependencies
  foreachE dependencies \dependency -> do
    runEffectFn2 addDependent dependency (toSomeNode node)
    dependencyHeight <- runEffectFn2 Node._read Node._height dependency
    ourHeight <- runEffectFn2 Node._read Node._height node
    if dependencyHeight + 1 > ourHeight then
      runEffectFn3 Node._write Node._height node (dependencyHeight + 1)
    else
      pure unit

disconnect :: forall a. EffectFn1 (Node a) Unit
disconnect = mkEffectFn1 \node -> do
  -- TODO
  pure unit

-- * Recompute

stabilize :: Effect Unit
stabilize = do
  whileE (runEffectFn1 PQ.isNonEmpty globalRecomputeQueue) do
    node_opt <- runEffectFn1 PQ.removeMin globalRecomputeQueue
    let node = Optional.fromSome node_opt

    do
      name <- runEffectFn1 Node.name node
      Console.log $ "stabilize: processing node " <> show name

    let source = runFn2 Node._get Node._source node
    -- oldValue_opt <- runEffectFn2 Node._read Node._value node
    newValue <- runEffectFn1 source.compute node
    -- if shouldNotCutOff oldValue_opt newValue
    runEffectFn3 Node._write Node._value node (Optional.some newValue)
    let dependents = runFn2 Node._get Node._dependents node
    
    -- FIXME: foreachE not desugared, closure is allocated for each element!
    foreachE (MutableArray.unsafeToArray dependents) \dependent -> do
      added <- runEffectFn2 PQ.add globalRecomputeQueue dependent
      if added then do
        name <- runEffectFn1 Node.name dependent
        Console.log $ "stabilize: node " <> show name <> " added to recompute queue"
      else do
        name <- runEffectFn1 Node.name dependent
        Console.log $ "stabilize: node " <> show name <> " already in recompute queue"
      pure unit

    let observers = runFn2 Node._get Node._observers node
    foreachE (MutableArray.unsafeToArray observers) \observer -> do
      -- FIXME: should be done outside stabilize loop, to avoid interfering with the process
      runEffectFn1 observer newValue

-- * Computational nodes

map :: forall a b. EffectFn2 (a -> b) (Node a) (Node b)
map = mkEffectFn2 \fn a -> do
  let deps = [toSomeNode a]
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.valueExc a
        pure (fn value_a)
    , dependencies: pure deps
    }

map2 :: forall a b c. EffectFn3 (Fn2 a b c) (Node a) (Node b) (Node c)
map2 = mkEffectFn3 \fn a b -> do
  let deps = [toSomeNode a, toSomeNode b]
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.valueExc a
        value_b <- runEffectFn1 Node.valueExc b
        pure (runFn2 fn value_a value_b)
    , dependencies: pure deps
    }

-- * Utils

effectCrash :: forall a. String -> Effect a
effectCrash msg = unsafeCoerce ((\_ -> unsafeCrashWith msg) :: Unit -> a) :: Effect a
