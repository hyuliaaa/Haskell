import Data.Char
import Data.List
sum'::[Int]->Int
sum' xs = if null xs then 0 else head xs + sum' (tail xs)


sum'' :: [Int] -> Int
sum'' []= 0
sum'' (x:xs) = x + sum'' xs


null'::[a]->Bool
null' [] = True
null' _  = False


head' :: [a] -> a
head' (x:xs) = x


length' :: [a] ->Int
length' [] = 0
length' (x:xs) =  1 + length' xs



elem' :: Int -> [Int] ->Bool
elem' y []= False
elem' y (x:xs) = y==x || elem' y xs


map' :: (a -> b) -> [a] -> [b]
map' f []      = []
map' f (x:xs)  = f x : map' f xs


filter' :: (a -> Bool) -> [a] ->[a]
filter' p []       = []
filter' p (x:xs)  
    | p x       = x : filter' p xs
    | otherwise = filter' p xs


--procheti za foldr
--priema binary function which has 2 parameters
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)


reverse' :: [a] -> [a]
--current value is 1st parameter and null value is second
reverse' lst = foldr' ( \x  xs-> xs ++ [x]) [] lst

isAscending:: Int-> Bool
isAscending num = isSorted (makeList num)
    where 
        makeList x = if x < 10 then [x] else makeList (x `div` 10) ++ [x `mod` 10]

        isSorted []       =True
        isSorted (x:[])   =True
        isSorted (x:y:xs) = x <= y && isSorted(y:xs)




pack::[Int]->[[Int]]
pack []       = []
pack (x:xs)   =  (x : takeWhile (==x) xs) : pack(dropWhile (==x) xs)



divisors:: Int->[Int]
divisors n = [k | k <- [1..n`div`2], n `mod` k ==0 ]

prodSumDiv :: Int -> Int -> Int -> Int
prodSumDiv a b k = product[x | x <- [a..b] , sum(divisors x)`mod` k==0]




pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <-[1..n], y<-[1..n], z<-[1..n], x*x+ y* y==z*z]


perfect::Int->Bool
perfect n = sum[x | x <-divisors n] ==n

perfectList::Int ->[Int]
perfectList n = [x | x<-[1..n], perfect x]


sumDivisors:: Int-> Int
sumDivisors xs=sum[x | x<-divisors xs]

prime::Int-> Bool
prime n = null[x | x<-[2..n-1], n `mod` x==0 ] && n /=1


incrementAllBy::[Int]->Int->[Int]
incrementAllBy [] n = []
incrementAllBy (x:xs) n = x + n : incrementAllBy xs n


filtersmallerThan::[Int]->Int->[Int]
filtersmallerThan [] n = []
filtersmallerThan xs n= filter (<n) xs

--nizove
digits:: String ->String
digits str= [ch | ch <-str, isDigit ch]

digitsSum:: String ->Int
digitsSum "" = 0
digitsSum (c:cs)
    |isDigit c  = (ord c -ord '0') + digitsSum cs -- ord връща аски кода на цифрата
    |otherwise  =digitsSum cs 

digitsSum' ::String ->Int
digitsSum' cs = sum [ord c - ord '0' | c<-digits cs]    

isCapitalyzed:: String -> Bool
isCapitalyzed "" = True
isCapitalyzed (c:cs) = isUpper c && isCapitalyzed cs


isVowel::Char->Bool
isVowel c = elem c "aeiouy"


countVowels:: String ->Int
countVowels word= length[c | c<-word, isVowel c]


nOrMoreVowels::[String] -> Int -> [String]
nOrMoreVowels words n = [word |word<-words, countVowels word >=n]

isPrefixOf'::String->String->Bool
isPrefixOf' []  _    =True
isPrefixOf' _ []     = False
isPrefixOf' (x:xs)(y:ys) = (x==y) && isPrefixOf xs ys

isInfixOf'::String ->String ->Bool
isInfixOf' []  _  = True
isInfixOf' _   [] = False
isInfixOf' xs ys  =isPrefixOf xs ys || isInfixOf xs (tail ys)


longestSubst:: String ->Int
longestSubst str= maximum [length substring | substring <-group str]


tighten::String->String
tighten = unwords.words