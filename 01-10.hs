--prob01

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

myLast2 = head . reverse

--prob02

myButLast :: [a] -> a
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs


--prob03

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Out of bounds"
elementAt (_:ks) k 
	| k < 1 = error "Out of bounds"
	| otherwise = elementAt ks (k-1)

--prob04

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength2 = foldl (\n _ -> n + 1) 0
myLength3 = sum . map (\_ -> 1)

--prob05

myReverse :: [a] -> [a]
myReverse x = mySubReverse x []
mySubReverse :: [a] -> [a] -> [a]
mySubReverse [] y = y
mySubReverse (x:xs) y = mySubReverse xs (x : y)

--prob06

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome x
	| head x == last x = isPalindrome (init $ tail x)
	| otherwise = False

--prob07

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--prob08

compress :: (Eq a) => [a] -> [a]
compress x = foldl (\n x -> if last n == x then n else n ++ [x]) [head x] x

--prob09

pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
	where func x [] = [[x]]
	      func x (y:ys) = if x == (head y) then ((x:y):ys) else [x]:y:ys

--prob10
encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\x -> (length x, head x)) . pack


