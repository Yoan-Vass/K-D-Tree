module Construction where

import Tree
import Hmath



--проверява, че в списъкът от точки е валиден пол отношение на техните размерности
validateDimensions :: [[Double]] -> Bool
validateDimensions [] = True
validateDimensions l = all ((dimension (head l) ==) . dimension) l

-------------------------------------


--избира максимален брой точки в листата според размерността
leafMax :: Int -> Int
leafMax d
 | d < 5 = 25--to do: Rand between 10 and 40
 | otherwise = 10--to do: Rand between 5 and 15


leafsize :: Int -> Int -> Int -> Int
leafsize seed d n
 | t == 0 = 1 --по една точка в листо
 | t == 1 = max 1 $ round $ sqrt (fromIntegral $ div n d) -- sqrt (n/d); rule of thumb? (някъде пишеше, че sqrt е добра горна граница)
 | otherwise = leafMax d
 where t = seed `rem` 3

---------------

--За конкретно измерение връща максималното разстояние между две точки по него 
amplitude :: Int -> [[Double]] -> Double
amplitude d points = maximum lst - minimum lst
 where lst = map (!! d) points

widestDimension :: [[Double]] -> Int
widestDimension points = accumulate (\cr ans -> if amplitude cr points > amplitude ans points then cr else ans) 0 1 (dimensionality points - 1) id (+1)

--------------------------

-- points :: [[Double]]
-- points = [[2.5,0, 1, 0], [1,3,5,2],[-1,-1,1,1],[0,4,-2,1]]


--алтерниращ избор на оста/разделящата равнина
nextSeed :: Integral a => a -> a -> a
nextSeed seed = if seed >= 0 then rem (seed + 1) else const seed
--nextSeed seed d = if seed >= 0 then rem (seed + 1) d else seed

--избира ос за текущата разделяща равнина, 3 стратегии - ос по "най-широкото направление", редуване на всички оси, избор на прозиволна ос
setAxis :: Int -> [[Double]] -> Int
setAxis seed points
 | seed >= 0 = seed
 | seed == -1 = widestDimension points
 | otherwise = 0 --to do : Rand () % (dimensionality points)


setVal :: Int -> Int -> [[Double]] -> Double
setVal seed axis points
 | seed >= 0 = middle projected --по средата на пространството
 | otherwise = median projected --медиана на точките
 where projected = map (!! axis) points
-- | --случайно val

-------------------------

-- конструита дървото по даден списък от точки, 
--точките точно на разделителя слага в лявата половина, избира стратегия на конструиране на дървото чрез seed
construct :: Int -> Int -> Int -> [[Double]] -> KDTree
construct seed1 seed2 seed3 l
 | length l <= leafsize seed3 (length l) (dimensionality l) = Leaf l $ hull l
 | otherwise = Tree axis value left right (zipWith merge (borders left) (borders right)) 
 where axis = setAxis seed1 l
       value = setVal seed2 axis l
       left = construct (nextSeed seed1 $ dimensionality l) seed2 seed3 $ filter (\point -> point !! axis <= value) l
       right = construct (nextSeed seed1 $ dimensionality l) seed2 seed3 $ filter (\point -> point !! axis > value) l


--wrapper функция за строене, хваща невалидни данни (тоест ако точки не са от еднаква размерност)

seed1 :: Int
seed1 = -1
seed2 :: Int
seed2 = -1
seed3 :: Int
seed3=0

build :: [[Double]] -> KDTree
build l = if validateDimensions l then construct seed1 seed2 seed3 l else error "Points are not of the same dimension!" -- construct seed1 seed2 points, -1 и 0 са примерни
