import Data.List(maximumBy, minimumBy)
import Data.Function(on)
import Data.Sequence (Seq())
import Control.Arrow (ArrowChoice(right, left))
import Text.XHtml ()
import Data.Either (rights)
import Text.PrettyPrint (Style(ribbonsPerLine))
import GHC.Base (leInt)

{- Генерични и полиморфни функции:
Полиморфните функции имат единствена дефиниция, която работи върху всички типове. Пример: length - работи върху списъци от всички типове (length :: [a] -> Integer).

Генеричните функции могат да се прилагат към данни от различни типове, но за различните типове се използват различни дефиниции (различни методи на генеричната функция).
Пример: + (има една дефиниция за Int, друга за Double), show, == -}

        {- Класове
Понятието клас в Haskell се определя като колекция от типове, за които се поддържа множество от додефинирани операции, наречени методи.
Множеството (колекцията) от типове, за които са дефинирани съответно множеството от функции, се нарича клас (type class). Типовете, които принадлежат на даден клас, се наричат екземпляри на този клас.
Макар и донякъде да има прилики с класовете от ООП, тук говорим за по-различно понятие. Ако ще правим аналогия, type classes са по-близо до интерфейси (но все пак не са точно същото).

Пример: за функцията elem по принцип може да смятаме че е декларирана като elem :: a -> [a] -> Bool, но тъй като тя ще сравнява елементи за равенство, то тя ще има смисъл само при типове които всъщност 
поддържат тази операция. Множеството на тези типове всъщност е класа Eq, който вллючва Int, Char, String, Bool, Double и др. (НЕ включва функционални типове - не може да сравняваме функции за равенство).

И така elem всъщност е elem :: Eq a => a -> [a] -> Bool, т.е. elem ще работи само за онези типове а, които са от Eq.
В тази декларация може да се види синтаксиса за деклариране на функция която ще работи само за даден клас - в началото добавяме класа, името на типа който ще му принадлежи, и '=>', след което си продължаваме
с нормалната декларация. Ако имахме няколко класа, трябва да ги обградим в скоби и да ги изредим, разделени със запетаи - например f :: (Eq a, Eq b) => ..., или пък g :: (Eq a, Visible a) => ...

Декларацията на даден клас включва неговото име, сигнатурите на класа (те еднозначно определят класа; това са функциите, които следва задължително да бъдат дефинирани за всички типове,
които са екземпляри на този клас). След сигнатурите може да се добавят и дефиниции по подразбиране. Например Eq е дефиниран така:
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x==y)
    x == y = not (x/=y)
Функциите, които трябва да бъдат дефинирани за да бъде един тип Eq са (==) и (/=). Те имат дефиниции по подразбиране, които обаче са взаимно рекурсивни, така че няма как да разчитаме изцяло на тях.
Това което ни позволяват е да дефинираме само едната - например само (==), при което ще имаме готова, работеща дефиниция на (/=), която ще е просто отрицанието на (==).

Може да имаме и производни класове (derived classes) - такива, които са подкласове на други класове. Например класът на наредените типове Ord - такива типове могат да бъдат сравнявани както за равенство,
така и за наредба (<, <=, >, >=), тоест те включват всичките операции на класа Eq, плюс няколко допълнителни, следователно са подклас на Eq.
class Eq a => Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min :: a -> a -> a
    compare :: a -> a -> Ordering
    ... (дефиниции по подразбиране за всичко без compare)
Бихме могли да кажем че Ord "наследява" Eq, но отново трябва да внимаваме - това не са класовете от ООП. -}


-- Алгебрични типове - служат за създаване на наши собствени типове
-- Ползваме ключовата дума data. Даваме име на типа, след което изреждаме конструкторите му - това са всъщност функции, които връщат стойност от типа.
-- За различните конструктори може да си мислим като за различни начини за конструиране на типа.
data Temp = Cold | Hot deriving Show

-- Типове може да бъдат екземпляри на множество класове - за целта се използва ключовата дума deriving. В тези случаи се задават дефиниции по подразбиране на функциите на тези класове
data Season = Spring |
              Summer |
              Autumn |
              Winter deriving Show -- така типът ще бъде екземпляр на класа Show, което означава че може да го print-нем в main-а

weather :: Season -> Temp
weather Summer = Hot
weather _      = Cold

--Можем да имаме конструктури, които имат аргументи

data People = Person Name Age deriving Show -- има конструктур с Name & Age
type Name = String
type Age = Int
-- print $ (Person "Jack" 20)

-- НЕ МОЖЕМ да преизпозлваме имена на конструктури
--data Employee = Person Name Age String  --> multiple declaritions of person
-- Не може да ползваме едно и също име на конструктор, дори и да е в различни типове, дори и да има различен брой аргументи.

-- Задача 1. Да се дефинира типа Shape с 4 конструктора: 
-- Circle,    който има 1 аргумент  - радиус
-- Rectangle, който има 2 аргумента - ширина и височина
-- Triangle,  който има 3 аргумента - 3 страни
-- Cylinder,  който има 2 аргумента - радиус на основата и височина
-- Типа Shape да се направи екземпляр на класа Show и за него да се дефинира метода show, позволяващ print-ването му

data Shape = Circle Double | Rectangle Double Double | Triangle Double Double Double | Cylinder Double Double
-- Тук за да го направим екземпляр на Show няма да ползваме deriving, тъй като това ще ни даде дефиниция по подразбиране на show, а ние искаме да си напишем наша.
instance Show Shape where
    show (Circle radius) = "A circle with radius: " ++ show radius
    show (Rectangle a b) = "A rectangle with sides: " ++ show a ++ "and" ++ show b
    show (Cylinder r h)   = "A cylinder with radius " ++ show r ++ " and height " ++ show h
    show (Triangle a b c) = "A triangle with sides: " ++ show a ++ ", " ++ show b ++ " and " ++ show c

c1, r1, cl1, tr1 :: Shape
c1  = Circle 3
r1  = Rectangle 4 5
cl1 = Cylinder 3 3
tr1 = Triangle 3 4 5

-- Задача 2. За Shape да се дефинират:
-- a) функция perimeter :: Shape -> Double, която намира периметъра на фигурата
perimeter::Shape ->Double
perimeter (Circle radius) = 2 * pi * radius
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c
perimeter _ = error "Unsupported shape"

-- b) функция area :: Shape -> Double, която намира лицето на фигурата
area::Shape -> Double
area (Circle radius) = pi * radius * radius
area (Rectangle a b) = a * b
area (Cylinder r h)      = 2 * area base + h * perimeter base
    where base = Circle r
area tr@(Triangle a b c) = sqrt (p * (p - a) * (p - b) * (p - c))
    where p = (perimeter tr) / 2

-- c) предикат isRound :: Shape -> Bool, който проверява дали дадена фигура е кръгла (някоя от стените е окръжност)
isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Cylinder _ _) = True
isRound _              = False

-- d) предикат is2D :: Shape -> Bool, който проверява дали дадена фигура е равнинна (лежи в една равнина)
is2D::Shape -> Bool
is2D (Cylinder _ _) = False
is2D _              = True

-- Задача 3. Да се дефинира функция sumArea, която приема списък от фигури и връща сумата от лицата на фигурите в списъка.
sumArea:: [Shape] -> Double
sumArea lst = sum (map area lst)
--sumArea lst = foldr1 (+) (map area lst)   -- валидно

-- Да се дефинира още една функция biggestShape, която намира фигурата с най-голямо лице.
biggestShape :: [Shape] -> Shape
biggestShape lst = foldl1 (\sh1 sh2 -> if area sh1 >= area sh2 then sh1 else sh2) lst

-- 2ри вариант - ползвайки maximumBy, комбинирано с on за да сравняваме по лице
biggestShape' :: [Shape] -> Shape
biggestShape' lst = maximumBy (compare `on` area) lst


-- Задача 4. Да се дефинира тип Point, който задава точка в равнината и точка в пространството. Типа да се направи екземпляр на класа Eq и за него да се дефинира равенство на точки от една и съща размерност.
data Point = P2 Double Double | P3 Double Double Double deriving Show

printPoint :: Point -> String
printPoint (P2 x y)   = "(" ++ show x ++ ", " ++ show y ++ ")"
printPoint (P3 x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Eq Point where
    (P2 x1 y1)    == (P2 x2 y2)    = x1 == x2 && y1 == y2
    (P3 x1 y1 z1) == (P3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

-- Задача 5. Да се дефинира функция distance за работа с типа Point, която намира разстоянието между две (съвместими) точки. 
-- Ако точките са с различна размерност (т.е. имат различен брой координати) функцията да връща съобщение за грешка.
distance::Point -> Point -> Double
distance (P2 x1 y1) (P2 x2 y2 ) = sqrt $ (x2-x1)^2 + (y2-y1)^2
distance (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2
distance _ _                         = error "Invalid points!"


-- Задача 6. Да се дефинира функция getClosestPoint, която приема списък от точки и още една точка p. 
-- Като резултат функцията да връща тази точка от списъка, която е най-близо до точката p.
getClosestPoint :: [Point] -> Point -> Point
getClosestPoint lst p = foldl1 (\ p1 p2 -> if distance p p1 <= distance p p2 then p1 else p2) lst



-- Задача 7. Да се дефинира рекурсивен алгебричен тип двоично дърво, който има стойности от тип Int по върховете си. Нека типът е екземпляр на класа Show.
--             празно            ст-т   ляво     дясно дърво
data IntTree = IntEmpty | IntNode Int IntTree IntTree deriving Show

intTree :: IntTree                                          --    5
intTree = IntNode 5 (IntNode 2 IntEmpty                     --   / \
                            (IntNode 3 IntEmpty IntEmpty))  --  2   6
                    (IntNode 6 IntEmpty IntEmpty)           --   \
                                                            --    3  


-- Задача 8. Да се преработи алгебричния тип за двоично дърво, така че при конструирането му да може да се определя типа на възлите.

data BTree a = Empty | Node a (BTree a)(BTree a ) deriving Show
t1 :: BTree Int                             --    5
t1 = Node 5 (Node 2 Empty                   --   / \
                    (Node 3 Empty Empty))   --  2   6
            (Node 6 Empty Empty)            --   \
                                            --    3 

t2 :: BTree Int                             --    5
t2 = (Node 5 (Node 3 Empty Empty)           --   / \
             (Node 4 (Node 5 Empty Empty)   --  3   4
                     (Node 7 Empty Empty))) --     / \
                                            --    5   7

charTree :: BTree Char                      --   a
charTree = Node 'a' (Node 'b' Empty Empty)  --  / \
                    (Node 'c' Empty Empty)  -- b   c

t3 :: BTree Int                             --     1     
t3 = Node 1 (Node 2 (Node 5 Empty Empty)    --    / \    
                     Empty)                 --   2   3 
            (Node 3 (Node 7 Empty Empty)    --  /   / \  
                    (Node 6 Empty Empty))   -- 5   7   6

-- За двоичното дърво да се дефинират следните функции:
-- a) size, която намира броя на възлите на двоично дърво
size::BTree a -> Int
size Empty                 = 0
size (Node _ left right)   =  1 + size left + size right

-- b) height, която намира височината на двоично дърво
height::BTree a -> Int
height Empty                 = 0
height (Node _ left right)   = 1 +  max (height left) (height right)

-- c) sumTree, която намира сумата от възлите на двоично дърво. Забележка: функцията трябва да работи само за такива дървета, чиито възли наистина могат да се сумират! (например за BTree Char няма да могат)
sumTree::Num a => BTree a -> a
sumTree Empty                  = 0
sumTree (Node x left right)   = x + sumTree left + sumTree right

-- d) sumLeaves, която намира сумата на елементите по листата на двоично дърво
sumLeaves::Num a => BTree a -> a
sumLeaves Empty                  = 0 
sumLeaves (Node x Empty Empty)   = x
sumLeaves (Node _ left right)    = sumLeaves left + sumLeaves right

-- e) inorder, която обхожда двоично дърво в ред Ляво-Корен-Дясно
inOrder::BTree a -> [a]
inOrder Empty    = []
inOrder (Node  x left right)  = inOrder left ++ [x] ++ inOrder right

-- Пример за използване на такъв вид обхождане: при представяне на аритметичен израз чрез дърво - по листата ще има стойности (числа, неизвестни), а по вътрешните върхове - инфиксни операции (+, -, *, /, ^)
expression :: BTree Char                                            --      -
expression = Node '-' (Node '+' (Node '5' Empty Empty)              --     / \
                                (Node '*' (Node '2' Empty Empty)    --    +   4
                                          (Node 'x' Empty Empty)))  --   / \
                      (Node '4' Empty Empty)                        --  5   *
                                                                    --     / \
                                                                    --    2   x

getExpression :: BTree Char -> String                               --    2   x
getExpression Empty                = ""
getExpression (Node c Empty Empty) = [c]
getExpression (Node c left right)  = "(" ++ (getExpression left) ++ [c] ++ (getExpression right) ++ ")"
-- това е точно inorder обхождане, с добавени скоби и допълнителен случай при листо (тъй като там не трябва да имаме скоби)


-- f) равенство на дървета (чрез операцията (==)). Забележка: за да може да сравняваме дървета за равенство, трябва първо да можем да сравняваме стойностите във възлите им!
instance Eq a => Eq (BTree a) where
    Empty         == Empty         = True
    Node x1 l1 r1 == Node x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2


-- Задача 9. Да се дефинира функция average :: BTree Int -> Double, която приема двоично дърво от цели числа и пресмята средно-аритметичното от записаното във върховете му.
-- 1ви вариант
average::BTree Int -> Double
average tree = fromIntegral (sumTree tree) / fromIntegral (size tree) -- тук ще обходим цялото дърво 2 пъти - 1во за да сумираме възлите, 2ро за да намерим броя им

-- 2ри вариант
average' :: BTree Int -> Double 
average' tree = fromIntegral (sum nodes) / fromIntegral (length nodes)
    where nodes = inOrder tree -- тук ще обходим дървото само веднъж - за да конструираме списъка nodes, но после ще обхождаме резултатния списък 2 пъти. 

-- Задача 10. Да се дефинира функция getLevel :: Int -> BTree a -> [a], която приема цяло число k и двоично дърво t и връща списък от елементите на k-то ниво на t.
getLevel::Int-> BTree a -> [a]
getLevel _ Empty    = []
getLevel 1 (Node x left right)  = [x]
getLevel k (Node _ left right) = getLevel(k-1) left ++ getLevel(k-1) right

-- Задача 11. Да се дефинира функцията getLevelsTree, която приема двоично дърво от произволен тип и заменя всеки негов възел с двойка от стойността на възела и номера на нивото му.
-- Например:
--     1             (1,0)
--    / \             / \
--   2   3   =>   (2,1) (3,1)
--  /   / \       /     /  \
-- 5   7   6    (5,2) (7,2) (6,2)

getLevelsTree :: BTree a -> BTree (a, Int)
getLevelsTree bt = helper bt 0
    where 
        helper Empty               _   = Empty
        helper (Node x left right) lvl = Node (x, lvl) (helper left (lvl + 1)) (helper right (lvl + 1))


-- Задача 12. Да се дефинира функция mirrorTree :: BTree a -> BTree a, която преобразува двоично дърво в "огледалното" му.
-- Например:
--     1           1    
--    / \         / \
--   2   3   =>  3   2
--  /   / \     / \   \
-- 5   7   6   6   7   5
mirrorTree::BTree a -> BTree a
mirrorTree Empty               = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

-- Задача 13. Да се дефинира функция mapTree, която приема функция f и двоично дърво t и прилага f към всеки възел на t.
mapTree::(a->b) ->BTree a ->BTree b
mapTree _ Empty = Empty
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)
