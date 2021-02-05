findNb :: Integer -> Integer
findNb m = helper 1 m 0
    where
        helper n m sum
            | sum==m      = n - 1
            | sum<m       = helper (n+1) m (sum+n^3)
            | otherwise   = -1