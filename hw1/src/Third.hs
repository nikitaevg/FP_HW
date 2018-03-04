module Third where

import Data.List.NonEmpty (NonEmpty (..), fromList, (<|))
import Data.Maybe (isNothing)

data DayOfWeek = Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday
               deriving (Show, Eq, Ord, Enum)

weekLen :: Int
weekLen = 6

nextDay :: DayOfWeek -> DayOfWeek
nextDay = flip afterDays 1

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day x = toEnum $ mod (fromEnum day + x) weekLen

isWeekend :: DayOfWeek -> Bool
isWeekend day
           | day == Saturday || day == Sunday = True
           | otherwise                        = False

daysToParty :: DayOfWeek -> Int
daysToParty day = mod (fromEnum Friday - fromEnum day) weekLen

newtype Citizen = Citizen
    { cName :: String
    } deriving (Eq, Show)

data Bible = Library | Church deriving (Show)

data Family = One | Two | Three | Four
    deriving (Enum, Show)

newtype House = House Family deriving (Show)

newtype Walls = Walls
    { wLen :: Int
    } deriving (Show)

newtype Castle = Castle
    { cLord :: Maybe Citizen
    } deriving (Show)

data Protection = Protection
    { pCastle :: Castle
    , pWalls  :: Maybe Walls
    } deriving (Show)

data City = City
    { cProtection :: Maybe Protection
    , cBible      :: Maybe Bible
    , cHouses     :: NonEmpty House
    } deriving (Show)

countPeople :: City -> Int
countPeople (City _ _ houses) = foldl (\x (House family) -> x + fromEnum family + 1) 0 houses

buildCastle :: City -> Castle -> (City, Bool)
buildCastle (City Nothing bible houses) castle = (City
                                                    (Just
                                                        (Protection castle Nothing))
                                                    bible houses,
                                                 True)
buildCastle x _                                = (x, False)

buildChurchOrLibrary :: City -> Bible -> (City, Bool)
buildChurchOrLibrary (City protection Nothing houses) bible =
                                    (City protection (Just bible) houses, True)
buildChurchOrLibrary x _                                    = (x, False)

buildNewHouse :: City -> Family -> City
buildNewHouse (City protection bible houses) family =
                                    City protection bible (House family<|houses)

data NewLordError = NoCastle | HaveAlready deriving (Eq, Show)

meetNewLord :: City -> Citizen -> (City, Maybe NewLordError)
meetNewLord city@(City Nothing _ _) _                            = (city, Just NoCastle)
meetNewLord city@(City (Just (Protection (Castle lord) walls)) bible houses) newLord
            | isNothing lord =
                  (City
                     (Just (Protection
                            (Castle (Just newLord)) walls))
                     bible houses,
                   Nothing)
            | otherwise = (city, Just HaveAlready)

buildWalls :: City -> Walls -> (City, Bool)
buildWalls city@(City (Just (Protection (Castle (Just lord)) Nothing)) bible houses) walls
            | countPeople city >= 10 =
                     (City
                        (Just (Protection
                                    (Castle (Just lord)) (Just walls)))
                        bible houses,
                     True)
            | otherwise              = (city, False)
buildWalls city _ = (city, False)

data Nat = Z | S Nat deriving (Show)

add :: Nat -> Nat -> Nat
add a Z     = a
add a (S b) = S (add a b)

mul :: Nat -> Nat -> Nat
mul a Z     = Z
mul a (S b) = add (mul a b) a

sub :: Nat -> Nat -> Nat
sub a Z         = a
sub (S a) (S b) = sub a b
sub Z _         = Z

fromNat :: Nat -> Int
fromNat Z     = 0
fromNat (S a) = 1 + fromNat a

toNat :: Integer -> Nat
toNat 0 = Z
toNat a = S (toNat $ a - 1)

equal :: Nat -> Nat -> Bool
equal Z Z = True
equal Z _ = False
equal _ Z = False
equal a b = sub a b == Z && sub b a == Z

less :: Nat -> Nat -> Bool
less a b = sub a b == Z && not (equal a b)

even :: Nat -> Bool
even Z         = True
even (S Z)     = False
even (S (S a)) = Third.even a

divide :: Nat -> Nat -> Nat
divide a b = if less a b
          then Z
          else divImpl a b a

divImpl :: Nat -> Nat -> Nat -> Nat
divImpl a b mu@(S mu') = if let res = mul b mu in less res a || equal res a
                        then mu
                        else divImpl a b mu'
divImpl _ _ Z          = 0

remainder a b = let divRes = divide a b in sub a $ mul divRes b


instance Eq Nat where
    x == y = equal x y

instance Ord Nat where
    compare x y
        | less x y  = LT
        | equal x y = EQ
        | otherwise = GT

instance Num Nat where
    x + y = add x y
    x - y = sub x y
    x * y = mul x y
    abs x = x
    signum (S _) = S Z
    signum _     = Z
    fromInteger = toNat

data Tree a = Nil | Node [a] (Tree a) (Tree a) deriving (Eq, Show)

empty :: Tree a -> Bool
empty Nil = True
empty _   = False

size :: (Ord a) => Tree a -> Int
size (Node list left right) = size left + length list + size right
size Nil                    = 0

contains :: (Ord a) => Tree a -> a -> Bool
contains Nil _ = False
contains (Node (x:_) left right) v
                             | x > v = contains left v
                             | x < v = contains right v
                             | x == v = True
contains (Node [] _ _) _ = False

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node [x] Nil Nil
insert (Node list@(x:_) left right) v
                             | x > v = Node list (insert left v) right
                             | x < v = Node list left (insert right v)
                             | x == v = Node (v:list) left right

remove :: (Ord a) => Tree a -> a -> Tree a
remove Nil _ = Nil
remove (Node list@(x:xs) left right) v
                             | x > v = Node list (remove left v) right
                             | x < v = Node list left (remove right v)
                             | x == v && length list > 1 = Node xs left right
                             | x == v && right == Nil && left == Nil = Nil
                             | x == v && right == Nil = left
                             | x == v = Node list2 left new_right
                                            where
                                              (list2, new_right) = delete_left right
                                              delete_left :: (Ord a) => Tree a -> ([a], Tree a)
                                              delete_left (Node list Nil right) = (list, right)
                                              delete_left (Node l left r) =
                                                let (ret, new_left) = delete_left left in
                                                    (ret, Node l new_left r)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl insert Nil

--TESTS--

city1 = City (Just
              (Protection
                 (Castle Nothing) Nothing))
              (Just Library)
              (House Two :| [House Four, House Four])

city2 = City Nothing
             Nothing
             (House One :| [])

testCity :: Bool
testCity = testCity1 && testCity2
         where
           testCity1 = countPeople city1 == 10 &&
                       not (snd $ buildCastle city1 (Castle Nothing)) &&
                       not (snd $ buildChurchOrLibrary city1 Library) &&
                       let newCity1 = fst $ meetNewLord city1 (Citizen "abacaba") in
                       snd (buildWalls newCity1 (Walls 10))
           testCity2 = countPeople city2 == 1 &&
                       (snd (meetNewLord city2 (Citizen "abacaba")) == Just NoCastle) &&
                       not (snd $ buildWalls city2 (Walls 10)) &&
                       let newCity1 = fst $ buildCastle city2 (Castle Nothing)
                           newCity2 = fst $ meetNewLord newCity1 (Citizen "abacaba")
                           newCity3 = buildNewHouse (
                                            buildNewHouse (
                                                buildNewHouse newCity2 Four
                                                ) Four
                                            ) Four
                       in
                       snd $ buildWalls newCity3 (Walls 10)

testNat :: Bool
testNat = (toNat 10 * toNat 5 + divide (toNat 8) (toNat 2) - toNat 4) == toNat 50

testTree :: Bool
testTree = let tree = Third.fromList [5, 3, 1, 4, 2, 7, 6, 9, 8]
           in
           contains tree 4 && contains tree 9 &&
           contains (insert tree 15) 15 &&
           not (contains (remove tree 4) 4)

thirdTests :: [Bool]
thirdTests = [testCity, testNat, testTree]

checkThird :: [Int]
checkThird = map fst $ filter (not . snd) $ zip [1..] thirdTests
