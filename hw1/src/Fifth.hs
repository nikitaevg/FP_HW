{-# LANGUAGE InstanceSigs #-}
module Fifth where

import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..), Semigroup (..))
import Fourth (NonEmpty ((:|)))

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr my_concat []
            where
              my_concat Nothing acc     = acc
              my_concat (Just list) acc = list ++ acc

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr my_concat (mempty, mempty)
            where
              my_concat (Left x) (accl, accr)  = (x `mappend` accl, accr)
              my_concat (Right x) (accl, accr) = (accl, x `mappend` accr)

instance Semigroup (NonEmpty t) where
    (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y:ys))

data ThisOrThat a b = This a | That b | Both a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) (This a) (This b)     = This (a <> b)
    (<>) (That a) (That b)     = That (a <> b)
    (<>) (This a) (That b)     = Both a b
    (<>) (Both a b) (This c)   = Both (a <> c) b
    (<>) (Both a b) (That c)   = Both a (b <> c)
    (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)
    (<>) (This c) (Both a b)   = Both (c <> a) b
    (<>) (That c) (Both a b)   = Both a (c <> b)
    (<>) x y                   = y <> x -- That <> This

newtype Name = Name String deriving (Eq)

instance Semigroup Name where
    (<>) (Name s) (Name t) = Name $ s ++ ('.':t)

instance Monoid Name where
    mempty = Name ""
    mappend x (Name "") = x
    mappend (Name "") y = y
    mappend x y         = (<>) x y

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
    mempty = Endo id
    mappend = (<>)

data Builder = One Char | Many [Builder] deriving (Show, Eq)

instance Semigroup Builder where
    (<>) x (Many [])         = x
    (<>) (Many []) y         = y
    (<>) a@(One _) b@(One _) = Many [a, b]
    (<>) (Many xs) (Many ys) = Many (xs ++ ys)
    (<>) a@(One _) (Many ys) = Many $ a:ys
    (<>) (Many xs) b@(One _) = Many (xs ++ [b])

instance Monoid Builder where
    mempty = Many []
    mappend = (<>)

foldrWithCtor :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldrWithCtor ctor = foldr (mappend . ctor) mempty

fromString :: String -> Builder
fromString = foldrWithCtor One

toString :: Builder -> String
toString (One ch)   = [ch]
toString (Many chs) = foldrWithCtor toString chs

--TESTS--

checkConcat :: Bool
checkConcat = maybeConcat [Just[1,2,3], Nothing,
                           Just[4,5,6], Nothing] == [1,2,3,4,5,6] &&
              eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] ==
                            (Sum {getSum = 8}, [1,2,3,4,5])

checkName :: Bool
checkName = Name "spb" `mappend` Name "ru" `mappend` mempty == Name "spb.ru"

checkThisOrThat :: Bool
checkThisOrThat = (This $ Sum 4) <> Both (Sum 5) (Max 10) <> (That $ Max 20) ==
                                Both (Sum 9) (Max 20)

checkBuilder :: Bool
checkBuilder = let str = "abacabadabacaba" in toString (fromString str) == str

checkEndo :: Bool
checkEndo = let endo1 = Endo (5 +)
                endo2 = Endo (flip (-) 5)
            in
            (getEndo $ endo1 `mappend` endo2 `mappend` mempty) 20 == 20

fifthTests = [checkConcat, checkName, checkThisOrThat, checkBuilder, checkEndo]

checkFifth :: [Int]
checkFifth = map fst $ filter (not . snd) $ zip [1..] fifthTests
