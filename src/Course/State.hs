{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec oldState = snd . runState oldState

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval oldState = fst . runState oldState

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: forall s.
  s
  -> State s ()
put newState = State $ const ((), newState)

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  f <$> (State stateToA) = State (\s -> 
    (\(nextVal, nextState) -> const (f nextVal, nextState)) (stateToA s) 
    s)
  -- mapFn <$> (State stateFn) = State (\s -> let (nextVal, nextState) = stateFn s in (mapFn nextVal, nextState))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (\s -> (a, s))
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (State s1ToF) <*> (State s2ToA) = State (\s -> 
    (\(f, s') ->                      -- s' is result of s1ToF
      (\(a, s'') -> const (f a, s'')) -- s'' is result of S2ToA
        (s2ToA s')) (s1ToF s)
    s)
  -- === below works! ===
  -- State s1ToF <*> State s2ToA = State (\s -> 
  --   let (f, s') = s1ToF s 
  --       (a, s'') = s2ToA s'
  --   in (f a, s''))

-- | Implement the `Monad` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
--
-- >>> runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
-- (10,16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  f =<< (State sToA) = State (\s -> 
    let (a, s') = sToA s 
    in runState (f a) s')

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
-- findM pred (a:.as) = pred a >>= \b -> if b then pure $ Full a else findM pred as
findM pred (a:.as) = do
  b <- pred a
  if b then pure $ Full a else findM pred as

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- >>> firstRepeat $ 1 :. 2 :. 0 :. 9 :. 2 :. 1 :. Nil
-- Full 2
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
-- firstRepeat as = 
--   let p x = (\s -> 
--         if S.member x s 
--           then pure True 
--           else put (S.insert x s) >>= const (pure False)
--         ) =<< get
--   in fst $ runState (findM p as) S.empty
-- === with do notation: ===
firstRepeat as = 
  let p x = do
        s <- get
        if S.member x s 
          then pure True 
          else do
            _ <- put (S.insert x s)
            pure False
  in fst $ runState (findM p as) S.empty

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct :: forall a.
  Ord a =>
  List a
  -> List a
-- distinct as = 
--   let p a = do 
--         s <- get
--         let s' = S.insert a s
--         _ <- put s'
--         pure True
--       toList s = P.foldr (:.) Nil s
--   in toList $ snd $ runState (filtering p as) S.empty 
distinct = runFilterWithPredicateInState filtering S.notMember
  where 
    runFilterWithPredicateInState 
      :: ((a -> State (S.Set a) Bool) -> List a -> State (S.Set a) c)  -- filt
      -> (a -> S.Set a -> Bool) -- pred
      -> List a -- as
      -> c 
    runFilterWithPredicateInState filt pred as = eval (filt (State . lift2 (lift2 (,)) pred S.insert) as) S.empty

-- for each item in the list, starting with an empty Set:
--    if the item is not in the Set, add it to the Set and filter it in
--    otherwise, filter it out
-- the chain of lift2s is plumbing to allow Set#nonMember to be used with Set#insert inside State
-- eventually, eval discards the now-unneeded State Set

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
-- 4^2=16, 1 + 6 = 7
-- 7^2=49, 4 + 9 = 13
-- 1^2 + 3^2 = 7
-- repeat, false
-- >>> isHappy 7
-- True
-- 7^2=49
-- 4^2+9^2=97
-- 9^2+7^2=130
-- 1+9+0=10
-- 1+0=1
-- true!
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy i = contains 1 $ firstRepeat $ produce sumOfSquareOfDigits (fromInteger i)
  where 
    sumOfSquareOfDigits n = sum $ square . digitToInt <$> toString n
    toString x = listh (show x)
    square = join (*)