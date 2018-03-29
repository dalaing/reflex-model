{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Model (
    SingletonState
  , removeEvent
  , setEvent
  , changeEvent
  , GroupState
  , liftSingleton
  , groupStateToEndo
  , listGroup
  ) where

import Control.Lens

import Control.Monad.Fix (MonadFix)
import Data.Semigroup
import Data.Monoid hiding ((<>))

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex

data SingletonState s =
  SingletonState {
    _ssChange :: Endo s
  , _ssRemove :: Any
  }

instance Semigroup (SingletonState s) where
  SingletonState c1 r1 <> SingletonState c2 r2 = 
    SingletonState (c1 <> c2) (r1 <> r2)

instance Monoid (SingletonState s) where
  mempty = 
    SingletonState mempty mempty
  mappend =
    (<>)

removeEvent :: (Reflex t, EventWriter t (SingletonState s) m) => Event t a -> m ()
removeEvent e =
  tellEvent $ SingletonState mempty (Any True) <$ e

setEvent :: (Reflex t, EventWriter t (SingletonState s) m ) => Lens' s a -> Event t a -> m ()
setEvent l e =
  tellEvent $ (\x -> SingletonState (Endo $ \s -> s & l .~ x) mempty) <$> e

changeEvent :: (Reflex t, EventWriter t (SingletonState s) m ) => Lens' s a -> Event t (a -> a) -> m ()
changeEvent l e =
  tellEvent $ (\f -> SingletonState (Endo $ \s -> s & l %~ f) mempty) <$> e

data GroupState k v =
  GroupState {
    _gsChange :: Endo (Map k v)
  , _gsRemove :: Set k
  }

instance Ord k => Semigroup (GroupState k v) where
 GroupState c1 r1 <> GroupState c2 r2 =
   GroupState (c1 <> c2) (r1 <> r2) 

instance Ord k => Monoid (GroupState k v) where
  mempty =
    GroupState mempty mempty
  mappend =
    (<>)

liftSingleton :: Ord k => k -> SingletonState v -> GroupState k v
liftSingleton k (SingletonState (Endo change) (Any remove)) =
  GroupState
    (Endo (Map.adjust change k))
    (if remove then Set.singleton k else Set.empty)

groupStateToEndo :: Ord k => GroupState k v -> Endo (Map k v)
groupStateToEndo (GroupState changes removes) = Endo $
  Map.filterWithKey (\k _ -> Set.notMember k $ removes) .
  appEndo changes

listGroup :: (Ord k, MonadFix m, Adjustable t m, MonadHold t m, PostBuild t m)
          => Dynamic t (Map k v) 
          -> (k -> Dynamic t v -> EventWriterT t (SingletonState v) m a) 
          -> EventWriterT t (GroupState k v) m (Dynamic t (Map k a))
listGroup dMap mk = do
  listWithKey dMap $ \k dv ->
    withEventWriterT (liftSingleton k) (mk k dv)

-- TODO the equivalent for listHoldWithKey


