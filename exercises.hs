import Data.List
import Data.Char

--1
subtotal :: (Num a) => [a] -> [a]
subtotal xs = reverse (sumOnLists (previouseElements (reverse xs)))

sumOnLists :: (Num a) => [[a]] -> [a]
sumOnLists xss = [ sum x | x <- xss]

previouseElements :: [a] -> [[a]]
previouseElements [] = []
previouseElements xs = xs : previouseElements (tail xs)

--2
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

histogram :: Int -> [Int] -> [Int]
histogram n xs = map length ( groupBy ((==) `on` snd) (mySort (intervalPairing n xs)) )

mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (compare `on` snd)

intervalPairing :: Int -> [Int] -> [(Int, Int)]
intervalPairing n xs = [(x, k) | x<-xs, k<-[1..((maximum xs) `div` n)  + 1 ], (<=) ((k-1)*n)  x, (<=) x (k*n-1)]

--3
meetsOffer :: String -> Int -> Bool
meetsOffer xs n |(sum (map convertGrade xs))>=n = True
                |(sum (map convertGrade xs))<n = False

convertGrade :: Char -> Int
convertGrade x | x=='E' = 16
               | x=='D' = 24
               | x=='C' = 32
               | x=='B' = 40
               | x=='A' = 48
               | x=='*' = 8

--4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show

ascending :: (Ord a) => [a] -> TypeOfSort
ascending [] = Ascending
ascending [x] = Ascending
ascending (x:y:xs) | x < y = ascending (y:xs)
                   | x==y = nonDescending (y:xs)
                   | otherwise = NotSorted

nonDescending :: (Ord a) => [a] -> TypeOfSort
nonDescending [] = NonDescending
nonDescending [x] = NonDescending
nonDescending (x:y:xs) | x<=y = nonDescending (y:xs)
                       | otherwise = NotSorted

descending :: (Ord a) => [a] -> TypeOfSort
descending [] = Descending
descending [x] = Descending
descending (x:y:xs) | x>y = descending (y:xs)
                    | x==y = nonAscending (y:xs)
                    | otherwise = NotSorted

nonAscending :: (Ord a) => [a] -> TypeOfSort
nonAscending [] = NonAscending
nonAscending [x] = NonAscending
nonAscending (x:y:xs) | x>=y = nonAscending (y:xs)
                      | otherwise = NotSorted

constant :: (Ord a) => [a] -> TypeOfSort
constant [] = Constant
constant [x] = Constant
constant (x:y:xs) | x==y = constant (y:xs)
                  | x>y = descending (y:xs)
                  | otherwise = ascending (y:xs)

sortType :: (Ord a) => [a] -> TypeOfSort
sortType (x:y:xs) | x==y = constant (y:xs)
                  | x>y = descending (y:xs)
                  | otherwise = ascending (y:xs)

--5
characterString :: Char -> [Char]
characterString x = [x]

rpcalc :: String -> Double
rpcalc expression = head (foldl operation [] (map characterString expression))

rpcalcWords :: String -> Double
rpcalcWords expression = head (foldl operation [] (words expression))

operation :: (Floating a, Read a) => [a] -> [Char] -> [a]
operation (x:y:ys) "*" = (x * y):ys
operation (x:y:ys) "+" = (x + y):ys
operation (x:y:ys) "-" = (y - x):ys
operation (x:y:ys) "/" = (y / x):ys
operation (x:y:ys) "^" = (y ** x):ys
operation (x:xs) "ln" = log x:xs
operation xs numberString = read numberString:xs

--6
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k p xs = take k (map getFirstElem (mySort (map distance (pair p xs))))

toIntPair (x, y) = (round x, round y)

getFirstElem :: (a, b) -> a
getFirstElem (x,y) = x

pair :: (a, b1) -> [b2] -> [((a, b1), b2)]
pair (x1, y1) xs = [((x1, y1), a) | a <- xs];

distance :: Floating b => ((b, b), (b, b)) -> ((b, b), b)
distance ((x1,y1), (x2,y2)) = ((x2,y2), sqrt((x1-x2)^2 + (y1-y2)^2))

--7
data SearchTree = Node SearchTree Int SearchTree | Leaf (Int) deriving Show

size (Leaf n)    = 1
size (Node l i r) = (max1 (size l) (size r)) + 1

max1 x y | x<=y = y
         | x>y = x

balanced :: SearchTree -> Bool
balanced (Leaf _) = True
balanced (Node l v r) = let diff = abs (size l - size r) in diff <= 1 && balanced l && balanced r && (value l)<v && v<(value r)

value (Node a b c) = b
value (Leaf a) = a

--8
f1 :: (Ord a, Num a) => a -> [a] -> a
f1 e (x:y:xs) | (abs (x-y))<=e = y
              | otherwise = f1 e (y:xs)

newtonRoot :: Double -> Double -> Double
newtonRoot d e = f1 e (newtonRootSequence d)

newtonRootSequence :: Double -> [Double]
newtonRootSequence d = iterate (next d) 1.0

next :: Fractional a => a -> a -> a
next n x = (x + n/x)/2

--9
hyperOperator n a b | n==0 = b+1
                    | n==1 && b==0 = a
                    | n==2 && b==0 = 0
                    | n>=3 && b==0 = 1
                    | otherwise = hyperOperator (n-1) a (hyperOperator n a (b-1))

--10
encode :: String -> [Int]
encode [] = []
encode (x:xs) = (h x) ++ (encode xs)

h :: Char -> [Int]
h x = if sum k `mod` 2 == 1
        then k ++ [1]
      else k ++ [0]
      where k = map fromInteger (faLista (digs(read (convert (fromEnum x) 2))))

faLista xs = if length xs == 8
                then xs
             else
               faLista (0:xs)

-- h :: Char -> [Int]
-- h x = if sum k `mod` 2 == 1
--         then k ++ [1]
--       else k ++ [0]
--       where k = reverse [ boolInt (testBit (fromEnum x) n) | n<-[0..7]]

boolInt :: Bool -> Int
boolInt x | x==True = 1
          | x==False = 0

--11
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

decode :: [Int] -> [Char]
decode xs = if allEven (map sumd (stringToInt (splitEvery 9 (map intToDigit xs))))
                then plm (map fromIntegral (map toDec (map init (splitEvery 9 (map intToDigit xs)))))
            else []

sumd 0 = 0
sumd x = (x `mod` 10) + sumd (x `div` 10)

stringToInt [] = []
stringToInt (x:xs) = a:stringToInt xs
                        where a = read x::Integer

allEven [] = True
allEven (x:xs) = if even x
                    then allEven xs
                 else False

-- decode xs = plm (map fromIntegral (map toDec (map init (splitEvery 9 (map intToDigit xs)))))

plm [] = []
plm (x:xs) = a : plm xs
                where a = toEnum x::Char

toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
--work on the fringe cases (check and what not)

--12
-- change n xs = if sum (map prodP (zip xs (reverse (snd (foldl nextc (n, []) xs)) ))) == n
--                 then reverse (snd (foldl nextc (n, []) xs) )
--               else change n (tail (reverse (snd (foldl nextc (n, []) xs) )))

makeChange n xs = carpaceala xs (change n (reverse (sort xs)))

carpaceala xs solutiexs = [ snd a|n<-xs, a <- (zip (reverse (sort xs)) (solutiexs)),  n==fst a]

change n xs = if allTheSame k && head k == 0
                  then map toMinus1 k
              else k
              where k = changee n xs

changee n xs = if sum (map prodP (zip xs (reverse (snd (foldl nextc (n, []) xs)) ) ) ) == n
                 then reverse (snd (foldl nextc (n, []) xs) )
              else if xs == []
                 then []
              else 0 : (changee n (tail xs))

toMinus1 a = -1

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

prodP (a,b) = a*b

nextc (remaining, cs) coin | coin <= remaining = (r', cnt:cs)
                           | otherwise         = (remaining, 0:cs)
                                where r' = remaining `mod` coin
                                      cnt = remaining `div` coin

--13
--from base 10 to another one
convert x base | x<base = show x
               | otherwise = (convert (x `div` base) base) ++ show (x `mod` base)

--ceva xs = [(x,y) | x<-xs, y<-[0..]]
convertTo base xs = sum [base^(snd a) * (fst a) | a <- ( zip xs [0..]) ]

goodSteinSuccessor base xs = ((base+1), reverse (digs (read (convert ((convertTo (base+1) xs)-1) (base+1)) :: Int)) )

watev :: [(Int, [Int])] -> [(Int, [Int])]
watev xs = if snd a /= []
              then  watev (xs ++ k)
           else xs
              where k = return (goodSteinSuccessor (fst (a)) (snd (a)))
                    a = last xs

goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence a = watev (return a)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

--14
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

isSat :: Prop -> Subst
isSat a = solution [(eval s a ,s) | s <- substs a]
  where
    solution [] = []
    solution (x:xs) = if fst x then snd x else solution xs

-- isSat :: Prop -> Bool
-- isSat p = or [eval s p | s <- substs p ]

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
              where vs = rmdups (vars p)

rmdups :: Eq a => [a ] -> [a ]
rmdups [ ] = [ ]
rmdups (x : xs) = x : rmdups (filter ( /= x ) xs)

bools :: Int -> [[Bool ]]
bools 0 = [[ ]]
bools n = map (False:) bss ++ map (True:) bss
                  where bss = bools (n-1)

vars :: Prop -> [Char]
vars (Const b) = []
vars (Var x ) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

find1 :: Eq k => k -> Assoc k v -> v
find1 k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find1 x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- p1 :: Prop
-- p1 = And (Var 'A') (Not (Var 'A'))
-- p2 :: Prop
-- p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
-- p3 :: Prop
-- p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
-- p4 :: Prop
-- p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

--15
xy z = (x, y)
        where w = floor ((sqrt(fromIntegral(8*z+1))-1)/2)
              t = (w^2 + w) `div` 2
              y = z - t
              x = w - y

pair15 x y = y + (x + y) * (x + y + 1) `div` 2

isPair z = if z == pair15 (pair15 (fst a') (snd a')) ((fst a')+(snd a'))
              then  True
           else False
              where a = xy z
                    a' = xy (fst a)
