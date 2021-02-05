type Point = (Double,Double)
splitPoints :: Point -> Double -> [Point] -> ([Point],[Point])
splitPoints p r [] = ([],[])
splitPoints p r xs = (filter( \x ->(fst x -(fst p))^2 +(snd x-(snd p))^2 <=r^2) xs, filter( \x ->(fst x -(fst p))^2 +(snd x-(snd p))^2 >r^2) xs )

