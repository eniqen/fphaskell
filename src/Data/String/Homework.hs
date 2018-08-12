module Data.String.Homework where


1.2.1
{-
Реализуйте функцию трех аргументов lenVec3, которая вычисляет длину трехмерного вектора. Аргументы функции задают декартовы координаты конца вектора, его начало подразумевается находящимся в начале координат. Для извлечения квадратного корня воспользуйтесь функцией sqrt, определенной в стандартной библиотеке.
GHCi> lenVec3 2 3 6
7.0-}

lenVec3 x y z =  sqrt (x ^ 2 + y ^ 2 + z ^ 2)

1.2.2
{-
Напишите реализацию функции sign, которая возвращает 1, если ей передано положительное число, (-1), если отрицательное, и 0 в случае, когда передан 0.
GHCi> sign (-100)
-1
-}

sign x = if x > 0 then 1 else if x < 0 then (-1) else 0

1.3.1
{-
Реализуйте оператор |-|, который возвращает модуль разности переданных ему аргументов:
GHCi>  5 |-| 7
2-}
x |-| y = abs (x - y)

1.4.1
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

1.4.2
import Data.Char
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (&&)(isDigit x)(isDigit y) then (digitToInt x) * 10 + digitToInt y else 100

1.4.3
dist :: (Double, Double) -> (Double, Double) -> Double
dist (xa, ya)(xb, yb) = sqrt (((xb - xa) ^ 2) + ((yb - ya) ^ 2))


1.5.1
doubleFact :: Integer -> Integer
doubleFact n = if ((abs n) <= 1) then 1 else n * doubleFact (n - 2)

1.5.2
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci(n - 1) + fibonacci(n - 2)
            | n < 0 = (-1) ^ (-n + 1) * fibonacci (-n)

1.5.3
helper :: Integer -> Integer -> Integer -> Integer
helper 0 prev next = prev
helper 1 prev next = next
helper curent prev next = helper (curent - 1)(next)(prev + next)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci n | n > 0 = helper n 0 1
            | n < 0 = (-1) ^ (-n + 1) * (helper (-n) 0 1)

1.6.1
seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = helper 3 2 1 n

helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper n3 _ _ 2 = n3
helper n3 n2 n1 n = helper (n3+n2 - 2*n1) n3 n2 (n-1)


1.6.2
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper(abs x)(0)(0)

helper :: Integer -> Integer -> Integer -> (Integer, Integer)

helper next acc count | next < 10 = (next + acc, count + 1)
                      | otherwise = helper(div next 10)((mod next 10) + acc)(count + 1)

2.1.1
getSecondFrom :: t -> t1 -> t2 -> t1
getSecondFrom a b c = b

2.1.2
import Data.Function

multSecond = g `on` h

g :: Num b => b -> b -> b
g x y = x * y
h :: (a, b) -> b
h b = snd b

2.1.3
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x)(f y)(f z)

2.2.1
doItYourself = f . g . h
f = logBase 2
g = (^3)
h = max 42

2.3.1
class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
    toString p | p == True = "true"
               | otherwise = "false"

instance Printable () where
    toString () = "unit type"

2.3.2
instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "("++ toString a ++"," ++ toString b ++")"

2.4.1
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab arg | (doesEnrageMork arg, doesEnrageGork arg) == (True, False) = stomp arg
                    | (doesEnrageMork arg, doesEnrageGork arg) == (False, True) = stab arg
                    | (doesEnrageMork arg, doesEnrageGork arg) == (True, True)  = (stomp.stab) arg
                    | otherwise = arg

2.4.2
{-Имея функцию
ip = show a ++ show b ++ show c ++ show d
определите значения
a
,
b
,
c
,
d
так, чтобы добиться следующего поведения:
GHCi> ip
"127.224.120.12"-}
a = 127.2
b = 24.1
c = 20.1
d = 2

2.4.4
avg :: Int -> Int -> Int -> Double
avg x y z =  (fromIntegral x + fromIntegral y + fromIntegral z) / 3

3.1.1
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b = (++)[a, b]

3.1.2
nTimes:: a -> Int -> [a]
nTimes v count = add v count []
        where
            add v 0 acc = acc
            add v count acc = add(v)(count -1)(v : acc)

3.1.3
oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = reverse $ filter xs []
    where
        filter [] acc = acc
        filter (x : xs) acc = if odd x then filter xs (x : acc) else filter xs acc

3.1.4
isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = reverse a == a

3.2.1
import Data.Char
readDigits :: String -> (String, String)
readDigits xs = (takeWhile isDigit xs, dropWhile isDigit xs)

3.2.2
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 xs = filter (\x -> p1 x || p2 x) xs

3.2.4
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concat . map fun

fun :: Num a => a -> [a]
fun x = [(^) x 2, (^) x 3]

3.2.6
import Data.Char
delAllUpper :: String -> String
delAllUpper = unwords . filter (\x -> not $ all isUpper x) . words

3.2.7
import Data.Char
delAllUpper :: String -> String
delAllUpper = unwords . filter (\x -> not $ all isUpper x) . words

3.3.1
{-Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]-}

fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

3.4.1
concatList :: [[a]] -> [a]
concatList = foldr (++) []

3.4.2
lengthList :: [a] -> Int
lengthList = foldr f 0 where
    f x acc = 1 + acc

3.4.3
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

3.5.1
meanList :: [Double] -> Double
meanList = fun . foldr (\x (s, c) -> (x + s, c + 1)) (0.0, 0) where
    fun (s, c) = s / c

3.5.2
evenOnly :: [a] -> [a]
evenOnly = fun . foldl (\(i, a) x -> (i + 1, if not $ odd i then x : a else a)) (1, []) where
    fun (index, arr) = reverse arr

3.5.3
evenOnly :: [a] -> [a]
evenOnly xs = foldr (\(v, i) acc -> if(even i) then v : acc else acc) [] (zip xs [1..])

3.6.1
lastElem :: [a] -> a
lastElem = foldl1 (\a b -> b)

3.6.2
revRange :: (Char,Char) -> [Char]
revRange (start, end)= unfoldr g end
  where g x = if x < start then Nothing else Just(x, pred x)

4.1.1
{-Тип данных Color определен следующим образом
data Color = Red | Green | Blue
Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.
GHCi> show Red
"Red"-}

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

4.1.2
{-Определите частичную (определенную на значениях от '0' до '9') функцию charToInt.
GHCi> charToInt '0'
0
GHCi> charToInt '9'
9-}

import Data.Maybe
charToInt :: Char -> Int
charToInt x = fromJust $ lookup x $ zip ['0'..'9'] [0..9]

4.1.3
{-Определите (частичную) функцию stringToColor, которая по строковому представлению цвета как в прошлой задаче возвращает исходный цвет.
GHCi> stringToColor "Red"
Red-}

import Data.Maybe
data Color = Red | Green | Blue

stringToColor :: String -> Color
stringToColor x = fromJust $ lookup x [("Red", Red), ("Green", Green),("Blue", Blue)]

4.1.4
{-Тип LogLevel описывает различные уровни логирования.
data LogLevel = Error | Warning | Info
Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
GHCi> cmp Error Warning
GT
GHCI> cmp Info Warning
LT-}

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Error Info = GT
cmp Warning Error = LT
cmp Warning Info  = GT
cmp Info Warning = LT
cmp Info Error = LT
cmp _ _ = EQ

4.1.5
{-Пусть объявлен следующий тип данных:
data Result = Fail | Success
И допустим определен некоторый тип данных SomeData и некоторая функция
doSomeWork :: SomeData -> (Result,Int)
возвращающая результат своей работы и либо код ошибки в случае неудачи, либо
0
в случае успеха.
Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения,
 либо строку "Fail: N" в случае неудачи, где N — код ошибки.-}

processData :: SomeData -> String
processData t = case doSomeWork t of
    (_, 0) -> "Success"
    (_, x) -> "Fail: " ++ show x

4.2.1
-- Реализуйте функцию distance, возвращающую расстояние между двумя точками.
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

4.2.2
{-Определим тип фигур Shape:

data Shape = Circle Double | Rectangle Double Double
У него два конструктора: Circle r — окружность радиуса r, и Rectangle a b — прямоугольник с размерами сторон a и b.
Реализуйте функцию area, возвращающую площадь фигуры. Константа pi уже определена в стандартной библиотеке.-}

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area shape = case shape of
    (Circle r) -> pi * (r ^ 2)
    (Rectangle a b) -> a * b

4.2.3
{-В одном из прошлых заданий мы встречали тип Result и функцию doSomeWork:
data Result = Fail | Success
doSomeWork :: SomeData -> (Result,Int)
Функция doSomeWork возвращала результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
Такое определение функции не является наилучшим, так как в случае успеха мы вынуждены возвращать некоторое значение, которое не несет никакой смысловой нагрузки.
Используя функцию doSomeWork, определите функцию doSomeWork' так, чтобы она возвращала код ошибки только в случае неудачи.
Для этого необходимо определить тип Result'.
Кроме того, определите instance Show для Result' так, чтобы show возвращал "Success" в случае успеха и "Fail: N" в случае неудачи, где N — код ошибки.-}

data Result' = Success' | Fail' Int
instance Show Result' where
    show Success' = "Success"
    show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' some = case doSomeWork some of
    (_, 0) -> Success'
    (_, n) -> Fail' n

4.2.4
-- Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.

data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare shape = case shape of
    Rectangle a b -> a == b
    _ -> False

4.3.1
{-
Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и запись должна содержать три поля:
timestamp — время, когда произошло событие (типа UTCTime);
logLevel — уровень события (типа LogLevel);
message — сообщение об ошибке (типа String).
Определите функцию
logLevelToString
, возвращающую текстуальное представление типа
LogLevel
, и функцию
logEntryToString
, возвращающую текстуальное представление записи в виде:

<время>: <уровень>: <сообщение>
-}
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Text.Printf

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString = show

instance Show LogEntry where
  show (LogEntry ts level message) = printf "%s: %s: %s" (timeToString ts) (logLevelToString level) (message)

4.3.2
-- Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1.
data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 {lastName = lastName person1}

4.3.3
{-Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan",
то после применения этой функции оно превратится в "I.".
Однако, если имя было короче двух символов, то оно не меняется.-}

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p@(Person (f:s:xs) _ _) = p {firstName = f : "."}
abbrFirstName p = p

4.4.3
{-Реализуйте функцию, которая ищет в строке первое вхождение символа,
который является цифрой, и возвращает Nothing, если в строке нет цифр.-}

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs

4.4.4
{-Реализуйте функцию findDigitOrX, использующую функцию findDigit (последнюю реализовывать не нужно).
findDigitOrX должна находить цифру в строке, а если в строке цифр нет, то она должна возвращать символ 'X'.
Используйте конструкцию case.-}

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
    Just x -> x
    Nothing -> 'X'

4.4.5
{-Maybe можно рассматривать как простой контейнер, например, как список длины 0 или 1.
Реализовать функции maybeToList и listToMaybe, преобразующие Maybe a в [a] и наоборот
(вторая функция отбрасывает все элементы списка, кроме первого).-}

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList _ = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

4.4.6
eitherToMaybe :: Either a a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

4.5.1
{-
Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле,
что существуют взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.-}

data List a = Nil | Cons a (List a)
fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a xs) = a : (fromList xs)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

4.5.2

{-Рассмотрим еще один пример рекурсивного типа данных:
data Nat = Zero | Suc Nat
Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее.
Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.
Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.-}

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc n) = mul n1 (fac n)

4.5.3
{-
Тип бинарных деревьев можно описать следующим образом:
data Tree a = Leaf a | Node (Tree a) (Tree a)
Реализуйте функцию height, возвращающую высоту дерева, и функцию size, возвращающую количество узлов в дереве (и внутренних, и листьев).
Считается, что дерево, состоящее из одного листа, имеет высоту 0.
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (max (height a) (height b))

size (Leaf a) = 1
size (Node (Leaf a) (Leaf b)) = 3
size (Node a b) = 1 + size a + size b
