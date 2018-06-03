{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}

module Lens where

import Data.Functor.Const    (Const (..), getConst)
import Data.Functor.Identity (Identity (..), runIdentity)


type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

view :: Lens s t a b -> s -> a              -- lookup value (getter)
view lenz = getConst . lenz Const

over :: Lens s t a b -> (a -> b) -> s -> t  -- change value (modifier)
over lenz f = runIdentity . lenz (Identity . f)

set  :: Lens s t a b -> b -> s -> t         -- set    value (setter)
set lenz x = over lenz (const x)

(.~) :: Lens s t a b -> b -> s -> t
(.~) = set
(^.) :: s -> Lens s t a b -> a
(^.) s lenz = view lenz s
(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\b -> (b, x)) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\a -> (x, a)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter modify obj = do
    let modified = modify $ getter obj
    setter obj <$> modified

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = do
        let getter = either (view l1) (view l2)
        let setter = either (\obj1 b -> Left $ set l1 b obj1) (\obj2 b -> Right $ set l2 b obj2)
        lens getter setter

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = do
    let new = f $ view l s
    (new, set l new s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = do
    let old = view l s
    (old, set l (f old) s)
