module Queries where
import Tree
import Construction
import Hmath

import System.IO

--INTERNAL-POINTS-OF-A-REGION---------------------------------------------------------

--по дадена точка и ограничения по всяка ос, определя дали точката ги удовлетворява
isInside :: [Double] -> [(Double, Double)] -> Bool
isInside [] [] = True
isInside (x:xs) ((beg, end):rest) = beg <= x && x <= end && isInside xs rest
isInside _ _ = error "constraints are not of the same dimension"

--същото като горното, само че не е safe (не предпазва от некоректни данни)
inside :: Ord a => [a] -> [(a, a)] -> Bool
inside point constraints = and $ zipWith (\axis (beg, end) -> beg <= axis && axis <= end ) point constraints

filterInside :: KDTree -> [(Double, Double)] -> [[Double]]
filterInside (Leaf l borders) constraints = if borders `intersect` constraints then filter (`isInside` constraints) l else []
filterInside (Tree ax val ltree rtree borders) constraints = if borders `intersect` constraints then filterInside ltree constraints ++ filterInside rtree constraints else []


--по дадени точки и ограничения връща кои точки са в ограниченията, като за целта ползва дървото
pointsInInterval :: [[Double]] -> [(Double, Double)] -> [[Double]]
pointsInInterval points = filterInside (build points)



--NEAREST-NEIGHBOUR---------------------------------------------------------

--при дадена точка, метрика на разстояние и две други точки, връща тази от последните, по-близо до първата
closer :: [Double] -> ([Double] -> [Double] -> Double) -> [Double] -> [Double] -> [Double]
closer point metric [] p2 = p2
closer point metric p1 [] = p1
closer point metric p1 p2 = if metric point p1 < metric point p2 then p1 else p2


nearestCustom :: [Double] -> ([Double] -> [Double] -> Double) -> [Double] -> KDTree -> [Double]
nearestCustom best metric point (Leaf l _) = foldr (closer point metric) best l
nearestCustom best metric point  (Tree axis val left right hull)
 | (best /= []) && (metric point best <= distanceToHull metric point hull) = best --всяка точка ще е по-далеч от best, няма смисъл да търси в поддървото
 | (point !! axis) <= val = nearestCustom (nearestCustom best metric point left) metric point right --намери най-близата отляво и виж дали има смисъл да продължаваш вдясно
 | otherwise = nearestCustom (nearestCustom best metric point right) metric point left --намери най-близата отдясно и виж дали има смисъл да продължаваш вляво

nnMinkwoski :: Int -> [Double] -> KDTree -> [Double]
nnMinkwoski p = nearestCustom [] (minkowskiDistance p)

nnEuclidean :: [Double] -> KDTree -> [Double]
nnEuclidean = nearestCustom [] euclideanDistance

nnManhattan :: [Double] -> KDTree -> [Double]
nnManhattan = nearestCustom [] manhattanDistance

nnMin :: [Double] -> KDTree -> [Double]
nnMin = nearestCustom [] minDistance

nnMax :: [Double] -> KDTree -> [Double]
nnMax = nearestCustom [] maxDistance

--ОБЩ-СЛУЧАЙ
--проблем: K-D дърветата не работят ефективно за произволна метрика за разсотяние (за общия случай явно трбяват други структури - VP-tree, BK-tree?)
--най-близка точка в общия случай, brute force решение:

nearestNeighbour :: [Double] -> KDTree -> ([Double] -> [Double] -> Double)-> [Double]
nearestNeighbour point tree metric = foldr (closer point metric) [] points
 where points = treeToList tree

-- друга реализация на същото
nearestNeighbour' :: [Double] -> KDTree -> ([Double] -> [Double] -> Double)-> [Double]
nearestNeighbour' point tree metric = head $ filter (\p -> metric point p == distance) points
 where points = treeToList tree
       distance = minimum $ map (metric point) points

-- To Do: трета алтернатива на горните, може би малко по-ефикасна, но не и в по-фунцкионален стил, би била да се сравняват двойки (позиция на най-близка точка досега, най-кратко разстояние)


--OTHER-FUNCTIONALITIES-------------------------------------------

--дърво към списък, подходяща за печат
treeToList :: KDTree  -> [[Double]]
treeToList (Leaf points _) = points
treeToList (Tree _ _ l r _) = treeToList l ++ treeToList r

--сортита списък от точки, според наредбата в дървото
kdSort :: [[Double]] -> [[Double]]
kdSort l = if validateDimensions l then treeToList $ build l else error "Points are not of the same dimension!"

--добавяне на точка към дървото
insert :: [Double] -> KDTree -> KDTree
insert point (Leaf l _) = if dimension point == dimensionality l then Leaf (point : l) (hull $ point:l) else error "Current point and tree points are not of the same dimension!"
insert point (Tree axis val l r hull) = if (point !! axis) <= val then Tree axis val (insert point l) r newHull else Tree axis val l (insert point r) newHull
 where newHull = addPoint point hull

-- --премахване на точка от дървото (ако я има)
remove :: [Double] -> KDTree -> KDTree
remove point (Leaf l _) = Leaf points (hull points)
 where points = filter (/= point) l
remove point (Tree axis val l r _) = Tree axis val left right (zipWith merge (borders left) (borders right))
 where left = if (point !! axis) <= val then remove point l else l
       right = if (point !! axis) <= val then r else remove point r


-------ПРИМЕР---------------------------------------------------------------------------
tree :: KDTree
tree = build [[2,-1],[-2,1],[1,-2],[-1, 0],[-2,-2],[3,0],[2,2]]

--TO JSON
-- >>> serialise "KDTree/tree-ctor1.json" tree

--TO LIST
-- >>> quickSort $ map (!! 0) $ treeToList tree
-- [-2.0,-2.0,-1.0,1.0,2.0,2.0,3.0]


--INSERT
-- >>> treeToList . insert [1, 2] $ tree
-- [[-2.0,-2.0],[1.0,-2.0],[-2.0,1.0],[1.0,2.0],[-1.0,0.0],[2.0,-1.0],[3.0,0.0],[2.0,2.0]]

--REMOVE, INSERT
-- >>> treeToList . remove [1, 2] . insert [1, 2] $ tree
-- [[-2.0,-2.0],[1.0,-2.0],[-2.0,1.0],[-1.0,0.0],[2.0,-1.0],[3.0,0.0],[2.0,2.0]]


--INSIDE AN INTERVAL
-- >>> filterInside tree [(-1,2),(-2.5,1.5)]
-- [[-1.0,0.0],[1.0,-2.0],[2.0,-1.0]]

--POINTS IN INTERVAL
-- >>> pointsInInterval [[-1,2,1], [0,2,2], [-3,0,0], [-1,1,0], [0,0,0]] [(-2,1),(0,2),(-1,1)]
-- [[-1.0,1.0,0.0],[0.0,0.0,0.0],[-1.0,2.0,1.0]]

--CLOSEST POINT
-- >>> nnManhattan [1,0.5] tree
-- [2.0,2.0]
-- >>> nnManhattan [1,0.499] tree
-- [2.0,-1.0]
-- >>> nnManhattan [-0.5,-2] tree
-- [1.0,-2.0]

-- >>> nnManhattan [-0.5001,-2] tree
-- [-2.0,-2.0]

-- >>> nnEuclidean [0.5,-1.5] tree
-- [1.0,-2.0]
-- >>> nnEuclidean [0.5,-1.5] $ remove [1,-2] tree
-- [2.0,-1.0]
-- >>> nnEuclidean [0.5,-1.5] $ remove [1,-2] $ remove [2,-1] tree
-- [-1.0,0.0]

-- >>> nnManhattan [0.5,-1.4999] $ remove [1,-2] $ remove [2,-1] tree
-- [-1.0,0.0]
-- >>> nnManhattan [0.5,-1.50001] $ left (remove [1,-2] $ remove [2,-1] tree)
-- [-2.0,-2.0]


-- t :: KDTree
-- t = Tree
--       0 0
--       (Tree
--         1 0
--         (Leaf [[-1.0, -2.0], [-2.0, -1.0]] [(-2,-1),(-2,-1)])
--         (Leaf [[-1, 0.5],[0, 1]] [(-1,0),(0.5,1)]) [(-2,0),(-2,1)])
--       (Tree
--         1 0
--         (Leaf [[1, -2], [2, -1]] [(1,2),(-2,-1)])
--         (Leaf [[1, 0.5],[1, 1]] [(1,1),(0.5,1)]) [(1,2),(-2,1)]) [(-2,2),(-2,2)]
