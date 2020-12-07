


mymin::Int ->Int ->Int
mymin a b
    |a > b     =a
    |otherwise =a

isInside::Int->Int->Int->Bool
isInside x a b = x >=a && x <=b    

myFunc::Int -> Int ->Double
myFunc a b = fromIntegral(sqr a + sqr b) /2
  where sqr x = x * x

myFib n 
    | n == 0  = 0
    | n == 1   =1
    |otherwise =myFib(n-1)+myFib(n-2)


mygcd::Int->Int->Int
mygcd a b
  | a == b =a
  | a > b  =mygcd (a-b) b
  |otherwise = mygcd a (b-a)


maxDivisor::Int ->Int
maxDivisor x = helper (x -1)
  where  
     helper d
      |x `mod` d == 0   =d
      |otherwise        =helper (d-1) 


reverseNumber::Int->Int
reverseNumber n = helper n 0
  where
    helper current result
      |current==0  =result
      |otherwise   =helper (current `div`10) (current `mod ` 10  +(result*10) ) 


increasingDigits::Int ->Bool
increasingDigits n = helper n ((n `div` 10) `mod`10) (n `mod` 10)
  where
    helper num prev lastDigit
      |num==0               =True 
      |lastDigit < prev     =False
      | otherwise           =helper (num `div` 10) ((num `div` 10) `mod`10) (num `mod` 10)


divides::Int->Int ->Bool
divides m n = if (n `mod`m) == 0 then True else False

countDivisors::Int->Int
countDivisors n = helper  1 0
  where
    helper  divisor counter
      |divisor > n               =counter
      |(divides divisor n)       =helper  (divisor + 1) (counter+ 1)
      |otherwise                 =helper  (divisor + 1) counter


sumDivisors::Int->Int
sumDivisors n = helper 1 0
  where
    helper divisor sum
      |divisor>n            =sum
      |(divides divisor n)  =helper (divisor + 1) (sum + divisor)
      |otherwise            =helper (divisor + 1) sum


palyndrome::Int->Bool
palyndrome n = if (reverseNumber n) == n  then True else False


countPalyndromes::Int->Int->Int
countPalyndromes a b
  |a > b               = 0
  | (palyndrome a)     =1 + countPalyndromes (a + 1 ) b
  |otherwise           =countPalyndromes(a + 1) b


--lambda functions
next = \ x -> x + 1


inside::Int->Int->(Int ->Bool)
inside a b = \ x -> x>=a && x <=b

plusOne :: Int -> Int
plusOne x = x + 1
square :: Int -> Int
square x = x * x

xSquaredPlusOne :: Int  -> Int
xSquaredPlusOne x = (plusOne . square) x



plusOne' = (+1)

sumDigits:: Int->Int
sumDigits n = helper n 0
  where
    helper  n sum
      |n ==0          =sum
      |otherwise      =helper (n `div` 10) (sum + (n `mod` 10))
  


       
