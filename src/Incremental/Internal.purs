module Incremental.Internal where

import Prelude

import Control.Monad.ST.Internal (foreach)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect, foreachE, whileE)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import Incremental.Internal.Effect (foreachUntil)
import Incremental.Internal.Global (globalCurrentStabilizationNum)
import Incremental.Internal.MutableArray as MutableArray
import Incremental.Internal.Node (Node, SomeNode, Observer, toSomeNode, toSomeNodeArray)
import Incremental.Internal.Node as Node
import Incremental.Internal.Optional (Optional)
import Incremental.Internal.Optional as Optional
import Incremental.Internal.PriorityQueue as PQ
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

-- * Globals

globalRecomputeQueue :: PQ.PQ SomeNode
globalRecomputeQueue = unsafePerformEffect $
  runEffectFn4 PQ.new
    Optional.none
    Node._height
    Node._inRecomputeQueue
    Node._nextInRecomputeQueue

globalLastStabilizationNum :: Ref Int
globalLastStabilizationNum = unsafePerformEffect $ Ref.new 0

-- * Var

newtype Var a = Var (Node a)

newVar :: forall a. EffectFn1 a (Var a)
newVar = mkEffectFn1 \val -> do
  node <- runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do
        value <- runEffectFn1 Node.valueExc node
        pure (Optional.some value)
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

-- * Event

newtype Event a = Event (Node a)

newEvent :: forall a. Effect (Event a)
newEvent = do
  node <- runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do
        runEffectFn2 Node._read Node._value node
    , dependencies: pure []
    }
  pure (Event node)

triggerEvent :: forall a. EffectFn2 (Event a) a Unit
triggerEvent = mkEffectFn2 \(Event node) val -> do
  runEffectFn3 Node._write Node._value node (Optional.some val)
  _ <- runEffectFn2 PQ.add globalRecomputeQueue (toSomeNode node)
  pure unit

readEvent :: forall a. Event a -> Node a
readEvent (Event x) = x

-- * Observers and dependents

addObserver :: forall a. EffectFn2 (Node a) (Observer a) Unit
addObserver = mkEffectFn2 \node observer -> do
  oldRefcount <- runEffectFn1 Node.refcount node
  runEffectFn2 MutableArray.push (runFn2 Node._get Node._observers node) observer
  runEffectFn2 handleRefcountChange node oldRefcount

removeObserver :: forall a. EffectFn2 (Node a) (Observer a) Unit
removeObserver = mkEffectFn2 \node observer -> do
  oldRefcount <- runEffectFn1 Node.refcount node
  runEffectFn2 MutableArray.remove (runFn2 Node._get Node._observers node) observer
  runEffectFn2 handleRefcountChange node oldRefcount

addDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
addDependent = mkEffectFn2 \node dependent -> do
  trace $ "addDependent " <> show (Node.name' node) <> " -> " <> show (Node.name' dependent)

  oldRefcount <- runEffectFn1 Node.refcount node
  runEffectFn2 MutableArray.push (runFn2 Node._get Node._dependents node) dependent
  runEffectFn2 handleRefcountChange node oldRefcount

removeDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
removeDependent = mkEffectFn2 \node dependent -> do
  oldRefcount <- runEffectFn1 Node.refcount node
  runEffectFn2 MutableArray.remove (runFn2 Node._get Node._dependents node) dependent
  runEffectFn2 handleRefcountChange node oldRefcount

handleRefcountChange :: forall a. EffectFn2 (Node a) Int Unit
handleRefcountChange = mkEffectFn2 \node oldRefcount -> do
  newRefcount <- runEffectFn1 Node.refcount node
  if oldRefcount == 0 && newRefcount > 0 then
    runEffectFn1 connect node
  else if oldRefcount > 0 && newRefcount == 0 then
    runEffectFn1 disconnect node
  else
    pure unit

-- Preconditions:
-- - node does not have value computed
-- - node does not have any dependents
--
-- Postconditions:
-- - all dependencies are connected and have value computed
-- - node has value computed
-- - node has correct height
connect :: forall a. EffectFn1 (Node a) Unit
connect = mkEffectFn1 \node -> do
--  trace $ "connect " <> show (Node.name' node)

  let source = runFn2 Node._get Node._source node

  dependencies <- source.dependencies
  foreachE dependencies \dependency -> do
    runEffectFn2 addDependent dependency (toSomeNode node)
    dependencyHeight <- runEffectFn2 Node._read Node._height dependency
    ourHeight <- runEffectFn2 Node._read Node._height node
    if dependencyHeight + 1 > ourHeight then do
      runEffectFn3 Node._write Node._height node (dependencyHeight + 1)
      runEffectFn3 Node._write Node._adjustedHeight node (dependencyHeight + 1)
    else
      pure unit

--  trace $ "connect: node " <> show (Node.name' node) <> ": compute"
  value <- runEffectFn1 source.compute node
  runEffectFn3 Node._write Node._value node value

disconnect :: forall a. EffectFn1 (Node a) Unit
disconnect = mkEffectFn1 \node -> do
  let source = runFn2 Node._get Node._source node

  dependencies <- source.dependencies
  foreachE dependencies \dependency -> do
    runEffectFn2 removeDependent dependency (toSomeNode node)

-- * Recompute

stabilize :: Effect Unit
stabilize = do
  trace "stabilize begin"
  currentStabilizationNum <- Ref.modify (_ + 1) globalLastStabilizationNum 
  Ref.write currentStabilizationNum globalCurrentStabilizationNum

  whileE (runEffectFn1 PQ.isNonEmpty globalRecomputeQueue) do
    node_opt <- runEffectFn1 PQ.removeMin globalRecomputeQueue
    let node = Optional.fromSome node_opt

    name <- runEffectFn1 Node.name node

    height <- runEffectFn2 Node._read Node._height node
    adjustedHeight <- runEffectFn2 Node._read Node._adjustedHeight node

    if adjustedHeight > height then do
      trace $ "stabilize: node " <> show name <> ": height bump " <> show height <> " -> " <> show adjustedHeight

      let dependents = runFn2 Node._get Node._dependents node
      foreachE (MutableArray.unsafeToArray dependents) \dependent -> do
        runEffectFn2 ensureHeight dependent (adjustedHeight + 1)

      runEffectFn3 Node._write Node._height node adjustedHeight

      -- Reconsider the node with new height
      _ <- runEffectFn2 PQ.add globalRecomputeQueue node
      pure unit

    else do
      trace $ "stabilize: node " <> show name <> ": compute at height " <> show height

      let source = runFn2 Node._get Node._source node
      -- oldValue_opt <- runEffectFn2 Node._read Node._value node
      newValue_opt <- runEffectFn1 source.compute node

      if Optional.isSome newValue_opt
        -- && shouldNotCutOff oldValue_opt newValue
      then do
        let newValue = Optional.fromSome newValue_opt
        runEffectFn3 Node._write Node._value node (Optional.some newValue)
        runEffectFn3 Node._write Node._changedAt node currentStabilizationNum
        
        -- FIXME: foreachE not desugared, closure is allocated for each element!
        let dependents = runFn2 Node._get Node._dependents node
        foreachE (MutableArray.unsafeToArray dependents) \dependent -> do
          added <- runEffectFn2 PQ.add globalRecomputeQueue dependent
          if added then do
            dependentName <- runEffectFn1 Node.name dependent
            trace $ "stabilize: node " <> show dependentName <> " added to recompute queue"
          else do
            dependentName <- runEffectFn1 Node.name dependent
            trace $ "stabilize: node " <> show dependentName <> " already in recompute queue"
          pure unit

        let observers = runFn2 Node._get Node._observers node
        foreachE (MutableArray.unsafeToArray observers) \observer -> do
          -- FIXME: should be done outside stabilize loop, to avoid interfering with the process
          -- (like in Specular - a FIFO queue)
          runEffectFn1 observer newValue
      else
        trace $ "stabilize: node " <> show name <> " cut off"

  Ref.write (-1) globalCurrentStabilizationNum
  trace "stabilize end"

-- * Computational nodes

constant :: forall a. EffectFn1 a (Node a)
constant = mkEffectFn1 \value -> do
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> pure (Optional.some value)
    , dependencies: pure []
    }

map :: forall a b. EffectFn2 (a -> b) (Node a) (Node b)
map = mkEffectFn2 \fn a -> do
  let deps = [toSomeNode a]
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.valueExc a
        pure (Optional.some (fn value_a))
    , dependencies: pure deps
    }

mapOptional :: forall a b. EffectFn2 (a -> Optional b) (Node a) (Node b)
mapOptional = mkEffectFn2 \fn a -> do
  let deps = [toSomeNode a]
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn2 Node._read Node._value a
        pure (if Optional.isSome value_a
              then fn (Optional.fromSome value_a)
              else Optional.none)
    , dependencies: pure deps
    }

map2 :: forall a b c. EffectFn3 (Fn2 a b c) (Node a) (Node b) (Node c)
map2 = mkEffectFn3 \fn a b -> do
  let deps = [toSomeNode a, toSomeNode b]
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.valueExc a
        value_b <- runEffectFn1 Node.valueExc b
        pure (Optional.some (runFn2 fn value_a value_b))
    , dependencies: pure deps
    }

-- Problem with bind and connect:
-- We have to:
-- - first compute LHS
-- - then compute ourselves
-- - only then we know the second dependency
-- - compute the second dependency
--
-- Problem with bind and compute:
-- - if we compute and the LHS changes, our height changes! We may need to re-add ourselves to recompute queue
bind_ :: forall a b. EffectFn2 (Node a) (a -> Node b) (Node b)
bind_ = mkEffectFn2 \lhs fn -> do
  main_node_ref <- Ref.new Optional.none
  rhs_node <- runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do

        trace "Computing bind_rhs"

        value_lhs <- runEffectFn1 Node.valueExc lhs
        let rhs = fn value_lhs

        -- adjust dependencies and height of the main node, taking new dependency into account
        main_opt <- Ref.read main_node_ref
        let main = Optional.fromSome main_opt

        runEffectFn2 addDependent rhs (toSomeNode main)
        dependencyHeight <- runEffectFn2 Node._read Node._height rhs
        runEffectFn2 ensureHeight main (dependencyHeight + 1)

        -- disconnect from old dependency, if present
        old_rhs_opt <- runEffectFn2 Node._read Node._value node
        if Optional.isSome old_rhs_opt then do
          runEffectFn2 removeDependent (Optional.fromSome old_rhs_opt) (toSomeNode main)
        else pure unit

        pure (Optional.some rhs)
    , dependencies: do
        pure [toSomeNode lhs]
    }
  runEffectFn2 Node.annotate rhs_node "bind aux"
  main <- runEffectFn1 Node.create 
    { compute: mkEffectFn1 \_ -> do
        rhs <- runEffectFn1 Node.valueExc rhs_node
        value <- runEffectFn1 Node.valueExc rhs
        pure (Optional.some value)
    , dependencies: do
        rhs_opt <- runEffectFn2 Node._read Node._value rhs_node
        if Optional.isSome rhs_opt then
          pure [toSomeNode rhs_node, toSomeNode (Optional.fromSome rhs_opt)]
        else
          -- if we don't know the rhs yet, `compute` in rhs proxy will add it
          pure [toSomeNode rhs_node]
    }
  Ref.write (Optional.some main) main_node_ref 
  pure main

fold :: forall a b. EffectFn3 (Fn2 a b (Optional b)) b (Node a) (Node b)
fold = mkEffectFn3 \fn initial a -> do
  let deps = [toSomeNode a]
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do
        state_opt <- runEffectFn2 Node._read Node._value node
        let state = if Optional.isSome state_opt then Optional.fromSome state_opt else initial

        hasInput <- runEffectFn1 Node.isChangingInCurrentStabilization a

        result <-
          if hasInput then do
            input <- runEffectFn1 Node.valueExc a
            pure (runFn2 fn input state)
          else
            pure (Optional.some state)

--        trace $ "fold: " <> (unsafeCoerce result :: String) <> ", " <> (unsafeCoerce result).constructor.name
        pure result
    , dependencies: pure deps
    }

sample :: forall a b c. EffectFn3 (Fn2 a b (Optional c)) (Node a) (Node b) (Node c)
sample = mkEffectFn3 \fn signal clock -> do
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do
        hasInput <- runEffectFn1 Node.isChangingInCurrentStabilization clock

        result <-
          if hasInput then do
            signal_value <- runEffectFn1 Node.valueExc signal
            clock_value <- runEffectFn1 Node.valueExc clock
            pure (runFn2 fn signal_value clock_value)
          else
            pure Optional.none

        pure result
    , dependencies: pure [toSomeNode signal, toSomeNode clock]
    }

leftmost :: forall a. EffectFn1 (Array (Node a)) (Node a)
leftmost = mkEffectFn1 \inputs -> do
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do
        runEffectFn2 foreachUntil inputs $ mkEffectFn1 \input -> do
          isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization input
          if isFiring then
            runEffectFn2 Node._read Node._value input
          else
            pure Optional.none
    , dependencies: pure (toSomeNodeArray inputs)
    }

traceChanges :: forall a. EffectFn2 (EffectFn1 a Unit) (Node a) (Node a)
traceChanges = mkEffectFn2 \fn input -> do
  runEffectFn1 Node.create 
    { compute: mkEffectFn1 \node -> do
        value_opt <- runEffectFn2 Node._read Node._value input
        isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization input
        if isFiring then runEffectFn1 fn (Optional.fromSome value_opt) else pure unit
        pure value_opt
    , dependencies: pure [toSomeNode input]
    }

-- * Adjust height

ensureHeight :: forall a. EffectFn2 (Node a) Int Unit
ensureHeight = mkEffectFn2 \node newHeight -> do
  oldAdjustedHeight <- runEffectFn2 Node._read Node._adjustedHeight node
  runEffectFn3 Node._write Node._adjustedHeight node (max oldAdjustedHeight newHeight)

-- * Utils

effectCrash :: forall a. String -> Effect a
effectCrash msg = unsafeCoerce ((\_ -> unsafeCrashWith msg) :: Unit -> a) :: Effect a

isTracing :: Boolean
isTracing = false

trace :: String -> Effect Unit
trace = if isTracing then Console.log else \_ -> pure unit
