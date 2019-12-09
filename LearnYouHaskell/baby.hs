import qualified Data.Map as Map
lucky :: (Integral a)=> a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "sorry you're out of luck, pal"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a)=> a ->a
factorial 0 = 1
factorial n = n* factorial(n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors  (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first(x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y 

third:: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a 
head' [] = error "Cant tell head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a)=> [a]-> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: "++ show x 
tell (x:y:[]) = "The list has two elements" ++ show x++" and " ++ show y 
tell (x:y:_) = "List is long. First two elements are: "++show x ++ "and "++show y 

length' :: (Num b) => [a] -> b 
length' [] = 0
length' (_:xs) = 1 + length' xs 

sum' :: (Num a) => [a] -> a 
sum' [] = 0
sum' (x:xs) = x+ sum' xs

capital :: String -> String
capital "" = "Empty string, whoops"
capital all@(x:xs) = "The first letter of "++all++" is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height 
    |  bmi  <= skinny = "underweight"
    | bmi  <= normal = "normal"
    | bmi  <= fat = "overweight"
    | otherwise = "obese"
    where bmi = weight/height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b 

myCompare :: (Ord a) => a->a-> Ordering
a `myCompare` b 
    | a> b = GT 
    |a == b = EQ
    | otherwise = LT

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs,let bmi = w/h^2]

cylinder :: (RealFloat a) => a->a->a 
cylinder r h = 
    let sideArea = 2*pi*r*h 
        topArea = pi*r^2
    in sideArea + 2*topArea

describeList :: [a] -> String
describeList xs = "The list is "++ case xs of [] -> "empty."
                                              [x] -> "a singleton list"
                                              xs -> " a longer list"

maximum' :: (Ord a) => [a] -> a 
maximum' [] = error" maximum of empty list"
maximum' [x] = x 
maximum' (x:xs)
    | x> maxTail = x 
    | otherwise = maxTail
    where maxTail = maximum' xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs)=
    let smallerSorted = quicksort[a | a<-xs, a<=x]
        biggerSorted = quicksort[a | a<-xs, a> x]
    in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a->a->a->a 
multThree x y z = x*y*z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x 

applyTwice :: (a -> a) -> a -> a 
applyTwice f x = f (f x)

zipWith' ::(a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f y x = f x y

largestDivisible :: (Integral a) => a 
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a)=> a-> [a]
chain 1 = [1]
chain n
    | even n = n:chain(n `div` 2)
    | odd n  = n:chain(n*3 +1)

numLongChains :: Int 
numLongChains = length (filter isLong (map chain[1..100]))
    where isLong xs = length xs > 15


numLongChains' :: Int 
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl(\acc x -> acc + x) 0 xs

elem' :: (Eq a)=> a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x==y then True else acc) False ys

map' :: (a->b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs 

maximum'' :: (Ord a) => [a] -> a 
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl  (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a 
product' = foldr1 (*)

filter' :: (a->Bool) -> [a] -> [a]
filter' p=foldr (\x acc -> if p x then x :acc else acc) []

head'' :: [a] -> a 
head'' = foldr1 (\x _ -> x)

last'' :: [a]->a
last'' = foldl1 (\ _ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<100000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile(<1000000). filter odd. map(^2) $ [1..]

oddSquareSum' :: Integer
oddSquareSum' = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<1000000) oddSquares
    in sum belowLimit

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Point = Point Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle ( Point x1 y1 )( Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r 
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1+b )) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape 
baseRect width height = Rectangle (Point 0 0) (Point width height)

{-
data Person = Person String String Int Float String String deriving (Show)


firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age:: Person -> Int
age(Person _ _ age _ _ _) =age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor -}

data Person = Person {firstName :: String
                      , lastName :: String
                      , age :: Int
                      } deriving (Eq, Show , Read)


data Car = Car {company :: String, model :: String, year :: Int}deriving (Show)

data MyMaybe a = MyNothing | MyJust a deriving (Show)

data Vector a= Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t 
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t 
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t 
(Vector i j k) `scalarMult` (Vector l m n) = i*l+ j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ] 

type Phonebook  = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]

data Either' a b = Left' a | Right' b deriving(Eq, Ord, Read, Show)

data LockerState= Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

{-data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) -}

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty       .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a  = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a 
singleton x = Node x EmptyTree EmptyTree 

treeInsert :: (Ord a) => a -> Tree a -> Tree a 
treeInsert x EmptyTree = singleton x 
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

data TrafficLight  = Red | Yellow | Green
instance Eq TrafficLight where 
    Red == Red =True
    Green == Green = True
    Yellow == Yellow = True
    _==_ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
     yesno Red = False
     yesno _ = True



data Barry t k p = Barry {yabba :: p , dabba :: t k}