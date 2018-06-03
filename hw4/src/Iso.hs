{-# LANGUAGE RankNTypes #-}

module Iso where

import Control.Applicative (Const (..))
import Control.Lens (Profunctor (..))
import Data.Tagged (Tagged (..), untag)
import Data.Functor.Identity (Identity (..), runIdentity)

type Iso b a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)

iso :: (b -> a) -> (a -> b) -> Iso b a
iso too = dimap too . fmap

getTo :: Iso b a -> b -> a
getTo is = getConst . is Const

getFrom :: Iso b a -> a -> b
getFrom is = runIdentity . untag . is . Tagged . Identity

from :: Iso b a -> Iso a b
from is = iso (getFrom is) (getTo is)
