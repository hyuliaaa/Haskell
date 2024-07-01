import Data.Char(digitToInt)
import Data.Text.Internal.Fusion.Size (larger)
import Data.List(nub)

-- Вектори/n-торки (tuples) - наредени n-торки от елементи. Типовете им са определи предварително, но могат да бъдат различни.

a :: (Int, Int)
a = (1, 2)
-- може да достъпваме първи и втори елемент с fst и snd съответно (само при двойки)
b :: (Int, Float, Float)
b = (1, 2.3, 4.5)
-- може да ги сравняваме с ==, но само ако са с еднаква дължина

addPair::(Int, Int) -> Int
addPair p = fst p + snd p

-- може да ползваме pattern matching с n-торки
addPair' :: (Int, Int) -> Int
addPair' (x, y) = x + y

-- функциите fst и snd ги има само за наредени двойки - за n-торки с по-голямо n начинът да достъпим компонентите им е чрез pattern matching

divide:: Int -> Int -> (Int,Int)
divide x y = (x `div` y, x `mod` y)

-- ако някоя координата не ни интересува в даден случай, може в pattern-а да я заместим с '_' - wildcard
addTriple :: (Int, Int, Int) -> Int
addTriple (x, _, z) = x + z

type Name = String
type Subject = String
type Grade = (Name, Subject, Float)

grade1::Grade
grade1 = ("John", "Algebra", 2.76)
grade2::Grade
grade2 = ("Jane", "Geometry", 5.667)

-- Задача 0. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число са във възходящ ред. Функцията да получава число, но да работи със списък от цифрите му
-- show - converts something into string

--print $ zip [1,2,3] [4,5,6] ->[(1,4),(2,5),(3,6)]
--print $ zip [1,2,3] (tail [1,2,3]) -> [(1,2), (2,3)]

isAscending :: Integer -> Bool
isAscending num = all ordered $ zip digits (tail digits)
    where
        digits = map digitToInt $ show num -- show е вградена функция, която конвертира нещо до низ (т.е. списък от Char-ове), а digitToInt (от Data.Char) конвертира символ на цифра до тази цифра като Int
        ordered (x, y) = x <= y
       -- като zip-нем списъка със същия, но "отместен" наляво (чрез взимане на опашката), получаваме списък от двойки последователни елементи, например:
       -- [1,2,3,4,...]
       -- [2,3,4,...]
       -- [(1,2),(2,3),(3,4),...]
       -- с all ordered провяваме дали при всяка двойка първият елемент е по-малък от вторият, тоест дали наистина всяка цифра е по-малка от следващата


--print $ zipWith (+) [1,2,3][4,5,6] -? [5,7,9]
--print $ zipWith (<) [1,2,3][4,5,6] -? [True,True,True

-- В горния вариант първо конструираме двойките последователни елементи, и след това сравняваме съответните елементи. Може да направим двете неща с една стъпка, като вместо zip ползваме zipWith (<).
isAscending' :: Integer -> Bool
isAscending' num = and $ zipWith (<) digits (tail digits)
    where digits = map digitToInt $ show num
-- and :: [Bool] -> Bool
-- and = foldr (&&) True

--all even [2, 4, 6]       -- Output: True
--all even [2, 3, 6]       -- Output: False

-- Задача 1. Да се дефинира функция numBiggerElements :: [Int] -> [(Int, Int)], която за даден списък от числа xs връща като резултат 
-- списък с елементи от вида (xi, ni), където xi е i-тият елемент на xs, а ni е броят на елементите на xs, които са по-големи от xi.
numBiggerElements :: [Int] -> [(Int, Int)]
numBiggerElements xs = [(x, length [y | y <- xs, y > x]) | x <- xs]


-- Задача 2. Да се дефинира функция splitByParity :: [Int] -> ([Int], [Int]), която получава списък от цели числа и го разделя на два списъка - от нечетни и четни.
--splitByParity [1, 2, 3, 4]     -- -> ([1,3], [2,4])
splitByParity :: [Int] -> ([Int], [Int])
splitByParity xs = (filter odd xs, filter even xs)

-- Задача 3. Да се дефинира функция partition' :: (a -> Bool) -> [a] -> ([a], [a]), която получава предикат и списък и разделя списъка на 2 части:
-- 1) елементите, които удовлетворяват предиката
-- 2) елементите, които не го удовлетворяват
-- Функцията е с ', тъй като вече съществува вградена функция под името partition.
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)

-- Да се пререши splitByParity, ползвайки partition.
splitByParity' :: [Int] -> ([Int], [Int])
splitByParity' xs = partition' odd xs

-- Задача 4. Да се дефинира функция quickSort :: [Int] -> [Int], която реализира бързо сортиране върху списък.
quickSort::[Int] -> [Int]
quickSort []   = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where (smaller, larger) = partition' (<p) xs


-- Задача 5. Да се дефинира тип Vector, определящ се от три координати - x, y и z. Да се дефинират функции за работа с типа:
type Vector = (Double, Double, Double)
-- а) sumVectors :: Vector -> Vector -> Vector, която намира сумата на два вектора
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1,y1,z1) (x2, y2,z2) = (x1+x2, y1+y2, z1 + z2)
-- b) scaleVector :: Vector -> Double -> Vector, която умножава скалар и вектор
scaleVector :: Vector -> Double -> Vector
scaleVector (x, y, z) p = (x*p, y*p, z*p)

-- c) dotProduct :: Vector -> Vector -> Double, която намира скаларното произведение на два вектора
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Задача 6. Да се дефинира тип Product, определящ се от име, количество и цена. Да се дефинира тип Shop (“база от данни”), който представлява инвентара от продукти на даден магазин.
type Product = (String, Int, Float)
type Shop = [Product]

p1, p2, p3, p4 :: Product
p1 = ("Milk", 5, 1.20)
p2 = ("Cheese", 20, 1.80)
p3 = ("Bread", 10, 0.50)
p4 = ("Chocolate", 3, 2.00)

shop::Shop
shop = [p1, p2, p3, p4]

-- Задача 7. Да се дефинира функция getPrice :: Product -> Float, която връща цената на даден продукт.
getPrice::Product -> Float
getPrice (_,_, price) = price;

-- Задача 8. Да се дефинира функция getTotal :: Shop -> Float, която връща оборота на даден магазин, ако е продаден целият инвентар.
getTotal::Shop->Float
getTotal [] = 0
getTotal ((_, quantity, price) : xs) = (fromIntegral quantity)*price + getTotal xs

-- 2ри вариант - с map и sum
getTotal' :: Shop -> Float
getTotal' xs = sum(map (\ (_, quantity, price) -> fromIntegral quantity * price) xs)

-- Задача 9. Да се дефинира функция buy :: String -> Int -> Shop -> Shop, която симулира “закупуването” на даден продукт, като приема име, количество и магазин. Да се вземе предвид, че не може след продажбата 
-- в магазина да имаме отрицателно количество за даден продукт. Ако искаме да купим продукт, но неговата наличност е недостатъчна, нека операцията да е празна, т.е. да не променя нищо.
-- Ако след покупка количеството е станало 0, продуктът да се премахне от инвентара.
-- print $ buy "Milk" 3 shop -- -> [("Milk",2,1.2),("Cheese",20,1.8),("Bread",10,0.5),("Chocolate",3,2.0)]
buy _ _ [] = error "No such product"
buy toBuyName toBuyQuantity (x@(name, quantity, price) : xs) -- синоними с @ може да правим и при n-торки - така може да реферираме както до цялата n-торка като x, така и до отделните ѝ елементи с техните имена
    | toBuyName == name && toBuyQuantity < quantity  = (name, quantity - toBuyQuantity, price) : xs -- намаляме количеството
    | toBuyName == name && toBuyQuantity == quantity = xs                                           -- премахваме продукта изцяло
    | toBuyName == name && toBuyQuantity > quantity  = x : xs                                       -- не променяме нищо
    | otherwise                                      = x : buy toBuyName toBuyQuantity xs           -- продължаваме да търсим в магазина


-- Задача 10. Да се дефинира функция getNeeded :: Int -> Shop -> [Product], която връща списък от продукти, чиято наличност е по-малка или равна на даден праг (количество).
getNeeded :: Int -> Shop -> [Product]
getNeeded _ [] = []
getNeeded needed (x@(name, quantity, price) : xs)
    | quantity <= needed = x : getNeeded needed xs
    | otherwise          = getNeeded needed xs

-- 2ри вариант - filter
getNeeded' :: Int -> Shop -> [Product]
getNeeded' needed xs = filter ((<= needed) . getQuantity) xs
    where getQuantity (_, quantity, _) = quantity


-- Задача 11. Да се дефинира функция closestToAverage :: Shop -> String, която намира името на продукта, чиято цена е най-близка до средната за всички в даден магазин.

-- първо - да намерим средната цена
getAverage :: Shop -> Float
getAverage xs = sum prices / fromIntegral (length prices)
    where prices = [price | (_, _, price) <- xs]


-- вече към същинската функция
-- 1ви вариант - с fold-ване за намиране на минималния елемент
closestToAverage :: Shop -> String
closestToAverage xs = name
    where 
        (name, _, _) = foldl1 compareProducts xs -- Тъй като резултатът от функцията е 3ка, а на нас ни трябва само 1ва ѝ координата, си запазваме резултатът тук и извличаме само това, което ни е нужно от него.
        compareProducts p1@(_, _, price1) p2@(_, _, price2) = if abs (price1 - average) < abs (price2 - average) then p1 else p2
        average = getAverage xs
        -- Алтернативно - може първо да извадим средното от всяка цена, и после да сравняваме директно тези разлики:
        -- (name, _, _) = foldl1 compareProducts (map (substract average) xs)
        -- substract x (name, quantity, price) = (name, quantity, abs(price - x))
        -- compareProducts p1@(_, _, difference1) p2@(_, _, difference2) = if difference1 < difference2 then p1 else p2


-- Задача 12. Да се дефинира функция cheaperAlternative, която намира броя на продуктите, за които има продукт със същото име, но по-ниска цена.
cheaperAlternative :: Shop -> Int
cheaperAlternative xs = length $ filter hasTwoPrices $ groupPrices xs
    where
        names = nub [name | (name, _, _) <- xs] -- nub е вградена ф-ия от библиотеката Data.List, която изтрива повторенията в списък. Тук ще получим списък от имената на продукти, без повторения

        groupPrices :: Shop -> [[Float]]
        groupPrices xs = [[price | (name', _, price) <- xs, name' == name] | name <- names] -- тук ще получим списък от списъци, всеки съдържащ различните цени на даден продукт
        -- например в shop2 ще имаме [[1.0,1.0],[2.5,5.0],[2.3]], т.к. bread присъства 2 пъти с цена 1, cheese - 2 пъти с цени 2.5 и 5.0, а butter - веднъж с 2.3  

        hasTwoPrices xs = length (nub xs) > 1 -- ако има две различни цени, то след като премахнем повторенията трябва списъкът да е с дължина поне 2




main :: IO()
main = do
    print $ nub [1,1,2,3,55]
