import Language.Haskell.TH (prim)
import Data.Char(ord, isDigit, isUpper, toUpper)
import Data.List(group)
--strongly typed language
--type inference: it can know its type automatically

--name::type
-- a :: Integer 
-- a = 5

--(приема -> връща(последното е нещото, което се връща))
-- f1 :: Int -> Int
-- f1 x = x + 1

--Identation matters!!!

f3::Int->Int->Int->Int
f3 x y z = x + y + z

--Типовете винаги се пишат с главна буква

--Вградени типове и операции
-- Bool - True, False
-- operations: &&, ||, not
-- Integer - поемат по-големи данни от 32 бит
-- операции: +, -, *, ^, div/mod
-- операции за сравнение: <, <=, >, >=, ==, /=(различно)
-- цяло в реално (fromIntegral)


-- Задачи:
--зад:1 Да се дефинира функция, която получава 2 числа и връща минималното от двете
myMin::Int->Int->Int
myMin x y = if x <= y then x else y

--with guards (охраняващи изрази) [like cond in scheme]
myMin' ::Int -> Int -> Int
myMin' x y 
    | x <= y      = x
    | otherwise   = y 

-- зад 2. Да се дефинира функция inside a b x, която проверява дали х е в [a,b]
inside:: Int -> Int -> Int -> Bool
inside a b x = a <= x && x <=b

--зад 3: Да се дефинира функция, която намира ср.аритм. на 2 числа
-- х + у връща цяло число и хаскел не може автоматично да го конверира в реално
-- затова използваме fromIntegral
average::Int -> Int -> Double
average x y = fromIntegral(x + y) / 2

--зад 4: Да се дефинира функция, която намира сумата на квадратите на числата а и в
sumSquares:: Int -> Int -> Int
-- sumSquares x y = x * x + y * y
sumSquares x y = square x + square y
    where
        square z = z * z --local function definition/ it is only available in the current func

--зад 5. fib::Int->Integer, намира н-тото число на Фибоначи
fib::Int -> Integer
fib n
    | n == 0    =1
    | n == 1    =1
    | otherwise = fib(n - 1) + fib (n - 2)

--Pattern matching (съпоставяне с образец)
fib':: Int-> Integer 
fib' 0 = 1
fib' 1 = 1
fib' n = fib(n-1) + fib(n - 2)

--итеративно решение
fibIter :: Int -> Integer
fibIter n = helper 1 1 2
    where
        helper i current next
            | i >= n      = current
            | otherwise   = helper(i+1) next (current + next) 

--lambda
------------argument -> body      
lambda1 :: Num a => a -> a -> a
lambda1 x = \ y -> x + y -- calling ((lambda1 5) 4) or (lambda1 5 4)
lambda2 :: Integer -> Integer -> Integer
lambda2 = \ x y -> x + y

--inside with lambda (inside' 3 5 4)
-- вместо да получаваме х като аргумент ще получаваме a i b
-- и ще върнем x като ламбда функция, които ще провери дали е между a и б
--  взимаме 2 инт-а и връщаме функция, която получава инт и връща буул            
inside':: Int -> Int -> (Int -> Bool)
inside' a b = \ x -> a >= x && b <=x

-- composition of function '.'
composedf x = (\y -> y*y ).( \y -> x + y)


-- to infix notation
-- print (div 10 2)
-- print (10 `div` 2)

--зад 7. Да се деф. ф-я xSquaredPlusOne::Int -> Int, която пресмята x^2 + 1, използвайки композиция
plusOne::Int -> Int
plusOne x = x + 1

square :: Int -> Int
square x = x * x
xSquaredPlusOne::Int -> Int
xSquaredPlusOne x = (plusOne . square) x
--xSquaredPlusOne = (plusOne . square) 

--    print (xSquaredPlusOne 3)
--    print $ xSquaredPlusOne 3 -- позволява да изпускаме скоби

-- zad.8 

-- частично прилагане (идеята е да прилагаме ф-ята само към част от аргументи)
p0:: Int->Int->Int->Int
p1:: Int -> Int -> Int
p2:: Int-> Int
p3:: Int

p0 x y z = x + y + z
p1 y z   = p0 1 y z
p2 z     = p0 1 2 z -- p0 приложено частично към първите си два аргумента
p3       = p0 1 2 3

-- Всяка функция е на един аргумент, всичко друго е синтактична захар  тоест:
-- f::Int-> Int->Int -> Int
-- f:: Int -> (Int -> (Int -> Int))

--Опертор сечение
plusOne' = (+1)
plusX x = (+x)

lessThanFive = (<5) --check whether smth is smaller than 5
greaterThanFive = (5<) --check whether smth is greater than 5

{- Списъци в Haskell - работят по подобен начин като тези в Racket, но синтаксиса е малко по-различен, и благодарение на pattern matching-а на практика ще работим с тях по малко по-различен начин
    []   - празен списък
    x:xs - операторът ':' конструира списък с глава x и опашка xs, като (cons x xs) в Racket
    head (x:xs) - връща главата x, подобно на car/first в Racket
    tail (x:xs) - връща опашката xs, подобно на cdr/rest в Racket

    [1,2,3] - списък с елементи 1, 2 и 3, еквивалентно на 1:(2:(3:[]))

В Haskell обаче списъците са хомогенни, за разлика от тези в Racket (които поради динамичното типизиране на езика можеха да съдържат различни типове)
    [а]     - списък от елементи от типа а
    [[a]]   - списък от списъци с елементи от типа а
    [[[a]]] - списък от списъци от списъци с елементи от типа а
    [[[[a]]]] и т.н. -}



        -- Конструиране на списъци
    -- print [1,2,3]        -- -> [1,2,3]
    -- print $ 1:[]         -- -> [1]
    -- print $ 1:[2,3]      -- -> [1,2,3]
    -- print $ 1:(2:(3:[])) -- -> [1,2,3]
          -- конкатенацията на списъци (append в Racket) тук е инфиксен оператор: (++)
    -- print $ [1,2,3] ++ [4,5,6] -- -> [1,2,3,4,5,6]
  
    -- разгледайте файла "Някои функции за работа със списъци"
    
    -- print $ map (\x -> x + 3) [1,2,3] -- -> [4,5,6]
    -- -- вместо ламбда може да ползваме операторно сечение
    -- print $ map (+3) [1,2,3]          -- -> [4,5,6]

    -- print $ filter odd [1,2,3,4]  -- -> [1,3]
    -- print $ filter (<3) [1,2,3,4] -- -> [1,2]

    -- print $ foldr (+) 0 [1,2,3]     -- -> 6

    -- print $ all odd [1,3,5]  -- -> True
    -- print $ any even [1,2,3] -- -> True

-- Да напишем функция sum, която сумира елементите на списък:
sum'::[Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
--sum' xs = foldr (+) 0 xs

-- проверка дали списък е празен
-- 'a' е произволен тип - може да бъде Int, Float, Char, и т.н.
null'::[a] -> Bool
null'[] = True
null' _ = False

-- взимане на глава на списък
head'::[a] -> a
head'(x:_) = x

-- взимане на опашка на списък
tail'::[a] -> [a]
tail'(_:xs) = xs

-- дължина на списък
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- проверка дали число е елемент на списък от числа
elem'::Int -> [Int] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | x == y    = True
    | otherwise = elem' y xs

-- функции от по-висок ред
map'::(a->b) -> [a] -> [b]
map' _ []  = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] ->[a]
filter' _ [] = []
filter' p (x:xs) = if p x then x :filter' p xs else filter' p xs

--foldr (+) 0 xs
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

--- Задача 1. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число са във възходящ ред. Функцията да получава число, но да работи със списък от цифрите му.
isAscending :: Integer -> Bool
isAscending num = isSorted $ digits num
    where
        digits x = if x < 10 then [x] else digits (x `div` 10) ++ [x `mod` 10]
        isSorted [] = True
        isSorted (x:[]) = True
        isSorted (x1:x2:xs) = x1<=x2 && isSorted(x2:xs)


-- remove(X, L) - remove first occurence of X in L
removeFirstOccurrence:: Integer ->[Integer]->[Integer]
removeFirstOccurrence _ [] = []
removeFirstOccurrence x (y:xs)
    | x == y      = xs
    | otherwise   = y: removeFirstOccurrence x xs

removeEveryOccurence::Integer ->[Integer] -> [Integer]
removeEveryOccurence _ [] = []
removeEveryOccurence x (y:ys)
    | x == y     = removeEveryOccurence x ys
    | otherwise  = y: removeEveryOccurence x ys



-- Задача 2. Нека as = [a1, a2 … , ak] и bs = [b1, b2 … , bk] са непразни списъци с еднакъв брой числа. Да се дефинира предикат isImage :: [Int] -> [Int] -> Bool, който да връща „истина“ точно 
-- когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.
isImage:: [Int] -> [Int] -> Bool
isImage as bs = as == map(+x) bs
    where x = head as - head bs

isImage'::[Int] ->[Int] -> Bool
isImage' (a:as)(b:bs) = as == map (+ (a-b)) (b:bs)

-- 3ти вариант - ползвайки синоними за да избегнем повторенията
isImage'' :: [Int] -> [Int] -> Bool
isImage'' as@(a:_) bs@(b:_) = as == map (+ (a - b)) bs
-- @ е начин за създаване на синоними, т.е. тук as и (а:_) са две имена на една и съша стойност - списък с първи елемент а и произволна опашка.
-- Както споменах по-горе, чрез pattern matching-а все едно декомпозираме стойността, но понякога може да искаме хем да се обръщаме към съставните ѝ части, хем към цялата стойност,
-- например в тази задача искаме да може да вземем главите на списъците за да сметнем x, но искаме и самите списъци, за да приложим map и да сравним за равенство.
-- В такива случаи са удобни синонимите - чрез тях може да имаме както променлива, с която да взимаме цялата стойност, така и променливи за всяка нейна съставна част.


-- print $ takeWhile even [2,4,6,1,3,5] -- -> [2,4,6] (спираме при първия нечетен елемент)
-- print $ dropWhile even [2,4,6,1,3,5] -- -> [1,3,5]


-- Задача 3. Да се дефинира функция pack :: [Int] -> [[Int]], която получава списък от цели числа, и връща списък от списъци, в който поредиците от последователни елементи са "опаковани" в отделни списъци.
   -- print $ pack [1,1,1,1,2,3,3,1,1,4,5,5,5,5] -- -> [[1,1,1,1],[2],[3,3],[1,1],[4],[5,5,5,5]]



pack :: [Int] -> [[Int]]
pack []  =  []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)
-- concat - flattens list of lists


{- List Comprehension - конструиране на списък чрез определяне на неговия обхват, подобно на range в Racket, но не е отделна функция
   [a..b]    - конструира списък [a, a+1, a+2,  ..., b]
   [a,h,..b] - конструира списък [a, a+h, a+2h, ..., b]

   [x | x <- xs, predicate] - конструира списък от тези елементи на xs, за които predicate e верен, подобно на filter
   [x | x <- xs, predicate1, predicate2, ..., predicateN] - конструира списък от тези елементи на xs, за които са верни всичките предикати. Доста по-удобно от N вложени filter-а.

   [f x | x <- xs] - ако f е функция на един аргумент ще получим списък, в който към всеки елемент е приложена f (подобно на map)

   може да комбинираме двете горни функционалности:
   [f x | x <- xs, predicate x] - като map върху filter-нат списък

   [(x,y) | x <- xs, y <- ys] - ще конструира списък от двойки с първи елемент от xs и втори от ys
                              - взимаме 1ви елемент от xs -> изчерпваме ys -> взимаме 2ри от xs -> изчерпваме ys -> ...
                              - всъщност ще се получи нещо като декартово произведение, ако мислим за списъците като множества -}



    -- print [x | x <- [1..10], even x]           -- -> [2,4,6,8,10]
    -- print [x | x <- [1..5], True]              -- -> [1,2,3,4,5]
    -- print [x | x <- [1..5], False]             -- -> []
    -- print [x | x <- [1..20], x < 10, x*x > 10] -- -> [4,5,6,7,8,9]
    -- print [x*x | x <- [1..5]]                  -- -> [1,4,9,16,25]
    -- print [x*x | x <- [1..5], odd x]           -- -> [1,9,25]
    -- print $ product [x*x | x <- [1..5], odd x] -- -> 225
    -- print [(x,y) | x <- [1,2,3], y <- ['a','b']] -- -> [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

-- Задачи за list comprehension                            
-- Задача 4. Да се дефинира функция divisors :: Int -> [Int], която генерира списък от всички (собствени) делители на дадено число
divisors::Int->[Int]
divisors n = [ k | k <- [1..n], n `mod` k == 0]

-- Задача 6. Да се дефинира функция removeNb :: Int -> [(Int, Int)], която приема естествено число n и връща списък от двойки естествени числа (a, b) – такива, че:
-- 1. a и b са по-малки от n,
-- 2. тяхното произведение е равно на сумата от числата от 1 до n без a и b.
  -- print $ removeNb 10  -- -> [(6,7),(7,6)]
removeNb::Int -> [(Int,Int)]
removeNb n = [(a, b) | a <- [1..n], b<-[1..n], a*b == sum [x | x <-[1..n], x /=a, x /=b]]



-- Низовете в Haskell са просто списъци от Char-ове (type String = [Char]).
-- С тях се работи по същия начин както се работи със списъци: можем да ги обхождаме с рекурсия, да ги map-ваме, да ги генерираме с list comprehension и т.н.

-- Задача 7. Да се дефинира функция digits :: String -> String, която получава низ и намира цифрите в него.
digits::String ->String
digits str = [ ch | ch <- str, isDigit ch]

-- Задача 8. Да се дефинира функция digitsSum :: String -> Int, която намира сумата на цифрите в даден низ.
digitsSum:: String ->Int
digitsSum ""   = 0
digitsSum (c:cs)
    | isDigit c = ord c - ord '0' + digitsSum cs -- ord връща ASCII кода на цифрата, но за да получим самата цифра изваждаме ASCII кода на 0 (например за 5: ord '5' - ord '0' = 53 - 48 = 5)
    | otherwise = digitsSum cs

-- Задача 9. Да се дефинира функция capitalize :: String -> String, която прави всички малки букви в даден String главни.
capitalize :: String -> String
capitalize cs = map toUpper cs -- toUpper е вградена функция от библиотеката Data.Char, която конвертира малка буква към главна (и не променя главни букви)

-- Задача 10. Да се дефинира функция isCapitalized :: String -> Bool, която проверява дали всички букви в даден String са главни.
isCapitalized :: String -> Bool
isCapitalized ""     = True
isCapitalized (c:cs) = isUpper c && isCapitalized cs

-- 2ри вариант - с all
isCapitalized' :: String -> Bool
isCapitalized' str = all isUpper str

-- Задача 11. Да се дефинира функция nOrMoreVowels :: [String] -> Int -> [String], която получава списък от думи и число n и връща само думите с поне n гласни.
isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> Int
--countVowels word = length $ filter isVowel word    -- 1ви вариант
countVowels word = length [c | c <- word, isVowel c] -- 2ри вариант

nOrMoreVowels :: [String] -> Int -> [String]
--nOrMoreVowels words n = filter (\word -> countVowels word >= n) words -- 1ви вариант
nOrMoreVowels words n = [word | word <- words, countVowels word >= n]   -- 2ри вариант

-- Задача 12. Да се дефинира функция isPrefixOf :: String -> String -> Bool, която проверява дали първия ѝ аргумент е префикс на втория.
isPrefixOf'::String->String->Bool
isPrefixOf' [] _    = True;
isPrefixOf' _ []    = False;
isPrefixOf' (x:xs) (y:ys) = (x==y) && isPrefixOf' xs ys


-- Задача 13. Да се дефинира функция isInfixOf :: String -> String -> Bool, която проверява дали първия ѝ аргумент е инфикс на втория.
-- 1ви вариант
isInfixOf'::String->String->Bool
isInfixOf' [] _ = True;
isInfixOf' _ [] = False;
isInfixOf' xs ys = isPrefixOf' xs ys || isInfixOf' xs (tail ys)

-- 2ри вариант
-- първо - функция, която генерира всички опашки на даден списък
tails' :: [a] -> [[a]]
tails' []        = [[]] -- ако върнем директно [], то празният списък няма да участва като опашка
tails' ys@(_:xs) = ys : tails' xs

-- второ - проверяваме дали първият низ е префикс на някоя от опашките на втория низ. 
-- Това дефакто правим и в първия вариант, но там обхождаме опашките една по една, а тук ползваме вградената функция any
isInfixOf'' :: String -> String -> Bool
isInfixOf'' xs ys = any (isPrefixOf' xs) (tails' ys)


-- Задача 14. Да се дефинира функция longestSubstring :: String -> Int, която намира дължината на най-дългия подниз в даден низ, състоящ се от еднакви символи.
    -- print $ longestSubstring  "111228888236555" -- -> 4 (заради 8888)
longestSubstring :: String -> Int
longestSubstring str = maximum [length substring | substring <- group str]
-- group е вградена функция (от Data.List), която разделя списък на списъци, всеки от които има само еднакви елементи. Например group "Mississippi" -> ["M","i","ss","i","ss","i","pp","i"].
-- Действието е подобно на pack който дефинирахме по-горе, но не е ограничено до списъци от цели числа, и имплементацията е по-различна.




f l = [x + y | x <-l, y <-l] -- декартово произведение на елементите от х и ги слага в един списък


-- Задача 15. Да се дефинира функция tighten, която "сгъстява" низ от думи, като премахва интервалите в началото и в края на низа, а между всеки две думи оставя по един интервал. 
-- Ако низът не съдържа думи, резултатът е празен низ.
    -- print $ tighten "  This   is  a   sentence    " -- -> "This is a sentence"
tighten :: String -> String
tighten ""             = ""
tighten (' ': cs)      = tighten cs -- премахва интервали в началото
tighten(c: ' ': ' ': cs) = tighten(c: ' ': cs) -- премахваме повторение на интервали между думи, докато не остане само един
tighten (c:' ':c':cs)  = c : ' ' : tighten (c':cs) -- Тъй като вече сме преминали през горния ред и сме продължили надолу, можем да сме сигурни, че c' не е интервал, т.е. е буква. 
                                                   -- Значи продължаваме "сгъстяването" в остатъка от низа
tighten (c:cs)         = c : tighten cs -- Тъй като сме минали през всичките горни редове и сме стигнали до тук, можем да сме сигурни, че сме "вътре" в дума, т.е. c е буква и след него следва още една буква.
                                        -- Значи взимаме първата буква и обработваме остатъка.
tighten' :: String -> String
tighten' = unwords . words
-- print $ words "  This   is  a   sentence    "   -- -> ["This","is","a","sentence"]
-- print $ unwords ["This","is","a","sentence"]    -- -> "This is a sentence"


--incrementAllBy :: [Int] -> Int -> [Int], която получава списък и число и го добавя към всеки елемент на списъка
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy [] _     =[]
incrementAllBy list@(x:xs) n = map (+n) list

-- incrementAllBy :: [Int] -> Int -> [Int]
-- incrementAllBy [] _ = []
-- incrementAllBy (x:xs) n = (x + n) : incrementAllBy xs n


--filterSmallerThan , която получава списък и число и премахва елементите на списъка, които са по-малко от числото
-- filterSmallerThan::[Int] ->Int -> [Int]
-- filterSmallerThan list x = filter (<x) list

filterSmallerThan [] _     = []
filterSmallerThan (y: ys) x
    | y > x           = filterSmallerThan ys x
    | otherwise       = y : filterSmallerThan ys x

main :: IO()    
main = do
   print $ filterSmallerThan [1,2,3] 2

