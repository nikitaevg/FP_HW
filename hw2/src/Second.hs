{-# LANGUAGE InstanceSigs #-}
module Second where

import Data.Char (isDigit)

stringSum :: String -> Maybe Int
stringSum str = sum <$> traverse getNum (words str)
          where
            getNum :: String -> Maybe Int
            getNum word = if all isDigit word
                          then Just $ read word
                          else Nothing

newtype Optional a = Optional (Maybe (Maybe a)) deriving (Show)

myId :: Optional a -> Optional b
myId (Optional (Just Nothing)) = Optional $ Just Nothing
myId (Optional Nothing)        = Optional Nothing
myId _                         = undefined

instance Functor Optional where
    fmap f (Optional (Just (Just x))) = Optional $ Just $ Just $ f x
    fmap _ x                          = myId x

instance Applicative Optional where
    pure x = Optional $ Just $ Just x
    (<*>) (Optional (Just (Just f))) x = f <$> x
    (<*>) _ x                          = myId x

instance Monad Optional where
    return x = Optional $ Just $ Just x          -- (1)
    (>>=) (Optional (Just (Just x))) f = f x     -- (2)
    (>>=) x _                          = myId x  -- (3)
{- 1 law: return a >>= f == f a
 -        return a == Optional $ Just $ Just a   (1)
 -        (Optional $ Just $ Just a) >>= f = f a (2)
 -
 - 2 law: m >>= return == m
 - m = Optional $ Just $ Just a
 - Optional $ Just $ Just a >>= return == return a (2)
 - return a == Optional $ Just $ Just a            (1)
 -
 - m = Optional $ Just Nothing
 - Optional (Just Nothing) >>= f =(3)= myId $ Optional (Just Nothing) == Optional (Just Nothing)
 -
 - m = Optional Nothing
 - Optional Nothing >>= f =(3)= myId $ Optional Nothing == Optional Nothing
 -
 - 3 law: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
 - m = Optional $ Just $ Just a
 - (Optional (Just (Just a)) >>= f) >>= g =(2)= f a >>= g
 - Optional (Just (Just a)) >>= (\x -> f x >>= g) =(2)= (\x -> f x >>= g) a == f a >>= g
 -
 - m = Optional (Just Nothing)
 - (Optional (Just Nothing) >>= f) >>= g =(3)= Optional (Just Nothing) >>= g =(3)= Optional $ Just Nothing
 - Optional $ Just Nothing >>= (\x -> f x >>= g) =(3)= Optional (Just Nothing)
 -
 - m = Optional Nothing
 - (Optional Nothing >>= f) >>= g =(3)= Optional Nothing >>= g =(3)= Optional Nothing
 - Optional Nothing >>= (\x -> f x >>= g) =(3)= Optional Nothing
 -}

instance Foldable Optional where
    foldMap = optional mempty
          where
            optional :: b -> (a -> b) -> Optional a -> b
            optional _ f (Optional (Just (Just x))) = f x
            optional d _ _                          = d

instance Traversable Optional where
    traverse f (Optional (Just (Just x))) = (Optional . Just . Just) <$> f x
    traverse _ x                          = pure $ myId x

data NonEmpty a = a :| [a]

fromList :: [a] -> NonEmpty a
fromList (x:xs) = x :| xs
fromList _      = undefined

toList :: NonEmpty a -> [a]
toList (x:|xs) = x:xs

instance Foldable NonEmpty where
    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x :| xs) = f x `mappend` foldMap f xs

instance Functor NonEmpty where
    fmap f (x :| xs) = f x :| map f xs

instance Applicative NonEmpty where
    pure x = x :| []
    (<*>) (f:|fs) (x:|xs) = fromList [g y | g <- f:fs, y <- x:xs]

instance Monad NonEmpty where
    return x = x :| []
    (>>=) (l:|ls) f = x :| (xs ++ ys)
                 where
                   x :| xs = f l
                   ys = ls >>= toList . f

instance Traversable NonEmpty where
    traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
