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

data Optional a = Optional (Maybe (Maybe a))

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
    return x = Optional $ Just $ Just x
    (>>=) (Optional (Just (Just x))) f = f x
    (>>=) x _                          = myId x

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
    fmap f (x :| xs) = (f x) :| map f xs

instance Applicative NonEmpty where
    pure x = x :| []
    (<*>) (f:|fs) (x:|xs) = fromList $ [g y | g <- (f:fs), y <- (x:xs)]

instance Monad NonEmpty where
    return x = x :| []
    (>>=) (l:|ls) f = x :| (xs ++ ys)
                 where
                   x :| xs = f l
                   ys = ls >>= toList . f

instance Traversable NonEmpty where
    traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
