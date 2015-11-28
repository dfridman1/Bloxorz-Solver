module Utils
       (
         replaceWith,
         replace,
         atIndex,
         addIndices
       ) where




replaceWith :: Int -> (a -> a) -> [a] -> [a]
replaceWith n f = iter n
    where iter k (x: xs) | k == 0    = f x: iter (k-1) xs
                         | otherwise =   x: iter (k-1) xs
          iter _ _ = []



replace :: Int -> a -> [a] -> [a]
replace n x = replaceWith n (\_ -> x)



atIndex :: [a] -> Int -> Maybe a
atIndex xs = iter xs
    where iter (y: ys) k | k == 0    = Just y
                         | otherwise = iter ys (k-1)
          iter _ _ = Nothing



addIndices :: [a] -> [(Int, a)]
addIndices = zip [0..]
