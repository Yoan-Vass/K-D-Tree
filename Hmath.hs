module Hmath where

accumulate :: Ord t => (b -> a -> a) -> a -> t -> t -> (t -> b) -> (t->t) -> a
accumulate op nv a b term next
 | a > b = nv
 | otherwise = op (term a) $ accumulate op nv (next a) b term next

quickSort :: Ord t => [t] -> [t]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort (filter (<x) xs) ++ [x] ++ quickSort (filter (>=x) xs)

--колко мерна е точката
dimension :: [a] -> Int
dimension = length

--в колко мерно е пространството са точките (при коректно зададени равни измерения на всичките)
dimensionality :: [[a]] -> Int
dimensionality [] = 0
dimensionality l = dimension $ head l

--намира медиана
median :: [Double] -> Double
median l = if odd $ length l then sorted !! (length l `div` 2) else avg
 where sorted = quickSort l
       ls = drop (div (length l) 2 - 1) sorted
       avg = (head ls + head (tail ls))/ 2

--по множество числа намира средата на числата (средата на най-голямата отсечка с краища точките)
middle :: [Double] -> Double
middle l = (maximum l + minimum l) / 2

--слива два интервала
merge :: (Double,Double) -> (Double,Double) -> (Double,Double)
merge (l1, r1) (l2, r2) = (min l1 l2, max r1 r2)

--добавя точка към правоъгълна обвивка на точки, актуализира последната
addPoint :: [Double] -> [(Double,Double)] -> [(Double,Double)]
addPoint = zipWith (\t (l, r) -> (min l t, max r t))

--по дадени точки намира хиперкуба, който ги обвива
hull :: [[Double]] -> [(Double,Double)]
hull [] = []
hull (x:xs) = foldr addPoint nv xs
 where nv= map (\y -> (y,y)) x

--проверява дали два хиперкуба имат обща точка
intersect :: [(Double, Double)] -> [(Double, Double)] -> Bool
intersect cube1 cube2= and $ zipWith (\(x1,x2) (y1,y2) ->  not (x2<y1 || y2<x1)) cube1 cube2

--евкилдово разстояние
euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance point1 point2 = sqrt . sum $ zipWith (\x y -> (x-y)^2) point1 point2

--манхатъново разстояние
manhattanDistance :: [Double] -> [Double] -> Double
manhattanDistance  = (sum .) . zipWith ((abs.) . (-))
--manhattanDistance point1 point2 = sum (zipWith (\x y -> abs $ x-y) point1 point2)

--минимално разстояние
minDistance :: [Double] -> [Double] -> Double
minDistance point1 point2 = minimum $ zipWith (\x y -> abs $ x-y) point1 point2

--максимално разстояние
maxDistance :: [Double] -> [Double] -> Double
maxDistance point1 point2 = maximum $ zipWith (\x y -> abs $ x-y) point1 point2

--разстояние на Минковски
minkowskiDistance :: Int -> [Double] -> [Double] -> Double
minkowskiDistance p point1 point2 = sum (zipWith (\x y -> (x-y)^p) point1 point2) ** (1 / fromIntegral p)

projector :: Double -> (Double, Double) -> Double
projector x (l, r)
 | x < l = l 
 | x > r = r
 | otherwise = x

--намира разстояние от точка то обвиката на други точки (тоест до "хиперпаралелепипед") по дадена метрика
distanceToHull :: ([Double] -> [Double] -> Double) -> [Double] -> [(Double, Double)] -> Double
distanceToHull metric point hull = metric point $ zipWith projector point hull

isLetter :: Char -> Bool
isLetter c = ('a'<=c && c<='z') || ('A'<=c && c<='Z')


----TESTING------------------------------------------------------------------------

-- >>> minkowskiDistance 1 [4,0] [0,3]
-- 1.0

-- >>> distanceToHull maxDistance [1,-3,2] [(-1,0), (-2,1), (-1,0)]
-- 2.0
