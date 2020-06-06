import Prelude hiding (sum, length, (++), (!!), maximum, elem, reverse,
                       take, drop, concat, zipWith, takeWhile, dropWhile)

sum:: [Integer]->Integer
sum []=0
sum(x:xs)= x + sum xs


length :: [Integer] ->Integer
length[]=0
length (x:xs)= 1 + length xs

(++) :: [a]->[a]->[a]
(++) [] m =m
(++) (x:xs) m = x  :(++) xs m

(!!) :: [a] ->Int -> a
(!!) (x:xs) a = if a==0 then x else xs !! (a-1)

isSorted :: [Int] -> Bool
isSorted []= True
isSorted (x:[])= True
isSorted (x:y:xs) = if x > y then False else isSorted(y:xs)

maximum :: [Int] -> Int
maximum (x:xs)= foldr max x xs

elem ::Eq a => a -> [a] -> Bool
elem  a []= False
elem a (x:xs)= if a==x then True else elem a xs  




isPrefix ::Eq a => [a] -> [a] -> Bool
isPrefix [] m =True
isPrefix l [] = False;
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False

drop:: a -> [a] -> [a]





import Prelude hiding (gcm, lcm, length, maxumum, map, filter, foldr, id)

-- `div` , the quotes make it an operator

-- if it is without the quotes we should use it at the begging as a function
id :: a->a 
    
id x = x  



compose f g x = f (g x)




--isFixpoint f x = if (x == f(x)) then True else False


isFixpoint :: Eq a => (a -> a) -> a -> Bool
 -- iskame a da byde sravnim tip, ako go nqma => nqma da raboti pravilno
isFixpoint f x = x == f x

derive :: (Double -> Double ) -> Double -> (Double -> Double)

derive f dx = \x -> (f (x + dx) - f x) / dx



square x = x * x 





type Point = (Double , Double)
type Vector = Point

-- + (1,2)+ (3,4)= (4,6)

(<+>) :: Vector -> Vector -> Vector
(x,y) <+> (z,t) = (x+z, y+t)

lenght :: [a] -> Int

lenght []= 0
lenght  (_: xs)= 1+ lenght xs


map :: (a->b)-> [a]-> [b]
map f []= []
map f (x:xs)= f x : map f xs


filter::(a->Bool) -> [a] -> [a]

filter p []=[]
filter p (x:xs) = if p x then x:filter p xs else filter p xs



lenght [] = 0
lenght (x:xs)= 1+ lenght xs

replicate' n x   
  | n <=0 = []
  | otherwise = x: replicate' (n-1) x



take' n [] = []
take' 0 (x:xs) = []
take' n (x:xs)= x : take' (n-1) xs  



reverse' [] = []
reverse' (x:xs)= reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a : repeat' a


quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 


flip':: (a -> b -> c) -> b ->a ->c
flip' f x y= f y x

map' :: (a ->b) ->[a]->[b]
map' f []= []
map' f (x:xs)= f x : map' f xs


filter':: (a -> Bool) -> [a] ->[a]
filter' p [] = []
filter' p (x:xs)
   | p x = x : filter' p xs
   |otherwise = filter' p xs 



quicksort' :: (Ord a) => [a] -> [a]    
quicksort' [] = []    
quicksort' (x:xs) =     
    let smallerSorted = quicksort' (filter (<=x) xs)  
        biggerSorted = quicksort' (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted


largestDivisible:: (Integral a ) => a
largestDivisible = head (filter' p [100000, 99999..])
      where p x = x `mod` 3829==0
























