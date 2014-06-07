--prob11

pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
	where func x [] = [[x]]
	      func x (y:ys) = if x == (head y) then ((x:y):ys) else [x]:y:ys

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\x -> (length x, head x)) . pack

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
	where encodeHelper (1,x) = Single x
	      encodeHelper (n,x) = Multiple n x

--prob12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
	where decodeHelper (Single x) = [x]
	      decodeHelper (Multiple n x) = [x | i <- [1..n]]

--prob13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = foldr encodeDirectHelper []
	where encodeDirectHelper x [] = [Single x]
	      encodeDirectHelper x (Single y:ys) = if x == y then (Multiple 2 x):ys else Single x : (Single y:ys)
	      encodeDirectHelper x (Multiple n y:ys) = if x == y then (Multiple (n+1) y):ys else Single x : (Multiple n y:ys)

--prob14
dupli :: Eq a => [a] -> [a]
dupli = concatMap (replicate 2)

--prob15
repli :: Eq a => [a] -> Int -> [a]
repli list num = concatMap (replicate num) list

--prob16
myDrop :: Eq a => [a] -> Int -> [a]
myDrop list count = dropHelper list count count
	where dropHelper [] _ _ = []
	      dropHelper (x:xs) count 1 = dropHelper xs count count
	      dropHelper (x:xs) count n = x : (dropHelper xs count (n-1))

--prob17
split :: [a] -> Int -> ([a], [a])
split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split xs _             = ([], xs)


--prob18
slice :: [a] -> Int -> Int -> [a]
slice x m n = drop (m-1) $ take n x

--prob19
rotate :: [a] -> Int -> [a]
rotate x n
	| n >= 0 = drop n x ++ take n x
	| otherwise = drop k x ++ take k x 
	where k = length x + n

--prob20
removeAt :: Int -> [a] -> (a, [a])
removeAt n x = (last . fst $ y, (take (n-1) . fst $ y) ++ (snd $ y))
	where y = split x n



