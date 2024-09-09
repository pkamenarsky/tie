{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Tie
  ( Id (..)
  , TypedRef (..)
  , Mode (..)
  , Resolved
  , Unresolved

  , Key
  , Ref

  , Dep

  , ResolveError (..)

  , mkRef

  , pull
  , pullCasc
  , put
  , put2
  , put3
  , sequenceN
  , traverseN
    
  , tie
  ) where

import           Control.DeepSeq (NFData)
import qualified Control.Monad.Trans.State.Strict as ST

import           Data.Dynamic
import           Data.Kind (Type)
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy (Proxy))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           GHC.Generics

--------------------------------------------------------------------------------

type Any = Dynamic

toAny :: Typeable a => a -> Any
toAny = toDyn

fromAny :: Typeable a => Any -> a
fromAny = flip fromDyn (error "fromDyn: this is a bug")

--------------------------------------------------------------------------------

data Pair k = forall v. Typeable v => Pair (Id (Proxy v -> k)) (Dep k (v Resolved))

newtype DepMap k f = DepMap (M.Map k (Propagate Any))
  deriving Show

instance (Eq k, Ord k) => Monoid (DepMap k f) where
  mempty = DepMap mempty

instance (Eq k, Ord k) => Semigroup (DepMap k f) where
  DepMap m1 <> DepMap m2 = DepMap (m1 <> m2)

deplookup :: Ord k => Typeable f => Typeable v => (Proxy v -> k) -> DepMap k f -> Propagate (f (v Resolved))
deplookup k (DepMap m) = fmap fromAny $ fromJust $ M.lookup (k Proxy) m

fromPairList :: Ord k => Typeable k => [Pair k] -> DepMap k (Dep k)
fromPairList pairs = DepMap $ M.fromList
  [ case k of
      Id k'        -> (k' Proxy, Retain $ toAny v)
      RemoveRec k' -> (k' Proxy, Remove)
  | Pair k v <- pairs
  ]

--------------------------------------------------------------------------------

data Id a = Id { unId :: a } | RemoveRec { unId :: a }
  deriving (Functor, Eq, Ord, Show, Generic)
  deriving anyclass NFData

newtype TypedRef (v :: Mode -> Type) a = TypedRef { unTypedRef :: a }
  deriving Functor
  deriving newtype (Eq, Ord, Show, NFData)

mkRef :: a -> TypedRef v a
mkRef = TypedRef

data Mode = Unresolved | Resolved

type Unresolved = 'Unresolved
type Resolved = 'Resolved

type family Key (mode :: Mode) a where
  Key Unresolved a = Id a
  Key Resolved a = a

type family Ref (mode :: Mode) k v where
  Ref Unresolved k v = TypedRef v k
  Ref Resolved k v = v Resolved

--------------------------------------------------------------------------------

data Dep k a where
  Done      :: a -> Dep k a
  Pull      :: (Typeable k, Typeable v) => (Proxy v -> k) -> Dep k (Maybe (v Resolved))
  PullCasc  :: (Typeable k, Typeable v) => (Proxy v -> k) -> Dep k (v Resolved)
  Put       :: Typeable v => Id (Proxy v -> k) -> Dep k (v Resolved) -> Dep k (v Resolved)
  Fmap      :: (b -> a) -> Dep k b -> Dep k a
  Ap        :: Dep k (b -> a) -> Dep k b -> Dep k a
  SequenceN :: [Dep k a] -> Dep k [a]

instance Functor (Dep k) where
  fmap = Fmap

instance Applicative (Dep k) where
  pure = Done
  (<*>) = Ap

pull :: Typeable k => Typeable v => (Proxy v -> k) -> Dep k (Maybe (v Resolved))
pull = Pull

pullCasc :: Typeable k => Typeable v => (Proxy v -> k) -> Dep k (v Resolved)
pullCasc = PullCasc

put :: Typeable v => Id id -> (TypedRef v id -> Proxy v -> k) -> (TypedRef v id -> id -> Dep k (v Resolved)) -> Dep k (v Resolved)
put (Id k) g f = Put (Id (g $ TypedRef k)) (f (TypedRef k) k)
put (RemoveRec k) g f = Put (RemoveRec (g $ TypedRef k)) (f (TypedRef k) k)

put2 :: Typeable v => TypedRef u b -> Id a -> (TypedRef u b -> TypedRef v a -> Proxy v -> k) -> (TypedRef v a -> a -> Dep k (v Resolved)) -> Dep k (v Resolved)
put2 r (Id k) g f = Put (Id (g r (TypedRef k))) (f (TypedRef k) k)
put2 r (RemoveRec k) g f = Put (RemoveRec (g r (TypedRef k))) (f (TypedRef k) k)

put3 :: Typeable v => TypedRef u b -> TypedRef w c -> Id a -> (TypedRef u b -> TypedRef w c -> TypedRef v a -> Proxy v -> k) -> (TypedRef v a -> a -> Dep k (v Resolved)) -> Dep k (v Resolved)
put3 r s (Id k) g f = Put (Id (g r s (TypedRef k))) (f (TypedRef k) k)
put3 r s (RemoveRec k) g f = Put (RemoveRec (g r s (TypedRef k))) (f (TypedRef k) k)

-- | Do not propagate removed values, unlike `sequenceA`
sequenceN :: [Dep k a] -> Dep k [a]
sequenceN = SequenceN

-- | Do not propagate removed values, unlike `traverse`
traverseN :: (a -> Dep k b) -> [a] -> Dep k [b]
traverseN f = sequenceN . fmap f

-----------------------------------------------------------------------------------

data Propagate a = Retain a | Remove
  deriving (Show, Functor)

instance Applicative Propagate where
  pure a = Retain a

  Retain f <*> Retain a = Retain (f a)
  _        <*> _        = Remove

--------------------------------------------------------------------------------

gather :: Dep k a -> ([Pair k], [k])
gather (Done _) = ([], [])
gather (Pull k) = ([], [k Proxy])
gather (PullCasc k) = ([], [k Proxy])
gather (Put k v)
  | (idMap, refs) <- gather v = (Pair k v:idMap, refs)
gather (Fmap _ a) = gather a
gather (Ap f a) = gather f <> gather a
gather (SequenceN as) = foldMap gather as

--------------------------------------------------------------------------------

type Resolve k a = M.Map k Any -> a

prune :: Ord k => S.Set k -> DepMap k (Dep k) -> Dep k a -> ST.State (M.Map k (Propagate (Resolve k Any))) (Propagate (Resolve k a))
prune _       _ (Done a) = pure $ Retain $ \_ -> a

prune parents recMap (Put (Id _)        dep) = prune parents recMap dep
prune _       _      (Put (RemoveRec _) _)   = pure Remove

prune parents recMap (Pull k)
  | S.member (k Proxy) parents = pure $ Retain $ \m -> Just $ fromAny $ fromJust $ M.lookup (k Proxy) m
  | otherwise                 = case deplookup k recMap of
      Remove -> pure $ Retain $ \_ -> Nothing
      Retain dep -> do
        m <- ST.get
        case M.lookup (k Proxy) m of
          Just (Retain lkup) -> pure $ Retain $ fmap (Just . fromAny) lkup
          Just Remove        -> pure $ Retain $ \_ -> Nothing
          Nothing   -> do
            lkup <- prune (S.insert (k Proxy) parents) recMap dep
            ST.modify (M.insert (k Proxy) (fmap (fmap toAny) lkup))

            pure $ Retain $ case lkup of
              Retain f -> \m' -> Just (f m')
              Remove   -> \_  -> Nothing

prune parents recMap (PullCasc k)
  | S.member (k Proxy) parents = pure $ Retain $ \m -> fromAny $ fromJust $ M.lookup (k Proxy) m
  | otherwise                 = case deplookup k recMap of
      Remove -> pure Remove
      Retain dep -> do
        m <- ST.get
        case M.lookup (k Proxy) m of
          Just (Retain lkup) -> pure $ Retain $ fmap fromAny lkup
          Just Remove        -> pure Remove
          Nothing   -> do
            lkup <- prune (S.insert (k Proxy) parents) recMap dep
            ST.modify (M.insert (k Proxy) (fmap (fmap toAny) lkup))

            pure lkup

prune parents recMap (Fmap f a) = fmap (fmap (fmap f)) $ prune parents recMap a

prune parents recMap (Ap f a)   = do
  f' <- prune parents recMap f
  a' <- prune parents recMap a

  case (f', a') of
    (Retain f'', Retain a'') -> pure $ Retain (f'' <*> a'')
    _                        -> pure Remove

prune parents recMap (SequenceN as) = do
  as' <- traverse (prune parents recMap) as
  pure $ Retain $ \m ->
    [ a m
    | Retain a <- as'
    ]

--------------------------------------------------------------------------------

knit :: Ord k => M.Map k Any -> Dep k a -> Propagate a
knit      _ (Done a) = Retain a

knit recMap (Put (Id _)        a) = knit recMap a
knit      _ (Put (RemoveRec _) _) = Remove

knit recMap (Pull k) = case M.lookup (k Proxy) recMap of
  Just a  -> Retain $ Just $ fromAny a
  Nothing -> Retain Nothing

knit recMap (PullCasc k) = case M.lookup (k Proxy) recMap of
  Just a  -> Retain $ fromAny a
  Nothing -> Remove

knit recMap (Fmap f a) = f <$> knit recMap a

knit recMap (Ap f a) = knit recMap f <*> knit recMap a

knit recMap (SequenceN as) = Retain [ a' | a <- as, Retain a' <- [ knit recMap a ] ]

--------------------------------------------------------------------------------

data ResolveError k = RefErrors [k] [k] | RootWasRemoved
  deriving (Eq, Ord, Show)

tie :: Typeable k => Ord k => Dep k a -> Either (ResolveError k) a
tie root = case (duplicateIds, missingIds, knit recMap root) of
  ([], [], Remove)       -> Left RootWasRemoved
  ([], [], Retain root') -> Right root'
  (_, _, _)              -> Left $ RefErrors duplicateIds missingIds

  where
    (rootIds, rootRefs) = gather root
    idMap = fromPairList rootIds

    missingIds = S.toList $ S.fromList rootRefs `S.difference` S.fromList
      [ unId recId Proxy
      | Pair recId _ <- rootIds
      ]

    duplicateIds
      = M.keys
      $ M.filter (> 1)
      $ M.fromListWith (+) [ (unId recId Proxy, 1 :: Int) | Pair recId _ <- rootIds ]

    recMapApp = M.fromList
      [ (k, v)
      | (k, Retain v) <- M.toList $ flip ST.execState mempty (prune mempty idMap root)
      ]

    recMap = fmap ($ recMap) recMapApp
