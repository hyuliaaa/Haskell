dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates  f g []  =True
dominates f g (x:xs)
    | abs(f x) >= abs(g x)    =dominates f g xs 
    |otherwise    =False