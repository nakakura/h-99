import Control.Monad (replicateM)

--previous
split :: [a] -> Int -> ([a], [a])
split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split xs _             = ([], xs)

slice :: [a] -> Int -> Int -> [a]
slice x m n = drop (m-1) $ take n x

--prob21
insertAt :: a -> [a] -> Int -> [a]
insertAt m x n = (fst $ d) ++ [m] ++ (snd $ d)
	where d = split x (n-1)

--prob22
range :: Int -> Int -> [Int]
range m n = [x | x <- [m..n]]

--prob23
rnd_select xs n 
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)