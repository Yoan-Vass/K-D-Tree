module Tree where

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors


import Hmath
import System.IO

class TupleToList a where
    fromTuple :: a -> [Double]
instance TupleToList ()  where
    fromTuple :: () -> [Double]
    fromTuple () = []
instance TupleToList (Double) where
    fromTuple :: Double -> [Double]
    fromTuple a = [a]
instance (TupleToList t ) => TupleToList (Double, t)  where
    fromTuple :: TupleToList t => (Double, t) -> [Double]
    fromTuple (x, xs) = x : fromTuple xs

--K-д дървото
data KDTree = Leaf {points :: [[Double]], borders  :: [(Double, Double)]} | Tree {axis :: Int, value :: Double, left :: KDTree, right :: KDTree, borders :: [(Double, Double)]} deriving (Show, Read)

--К-Д дърво, но без допълнителните данни във върховете за интервал на тотчките в поддървото
data CompressedTree = Leaf' {points' :: [[Double]]} | Tree' {axis' :: Int, value' :: Double, left' :: CompressedTree, right' :: CompressedTree} deriving (Show, Read)


class Show t => ToJSON t where
    toJSON :: t -> String

tabulation :: Int -> [Char]
tabulation n = replicate n '\t'

--дърво към JSON wrapper фунцкия
toJSON' :: Int -> KDTree -> String
toJSON' tabs (Leaf l hull) = "{\n" ++ tabulation (tabs+1) ++ "\"points\": " ++ show l ++ "\n" ++ tabulation tabs ++ "}"
toJSON' tabs (Tree axis val l r hull) = "{\n" ++ tabulation (tabs+1) ++ "\"axis\": " ++ show axis ++ ",\n" ++ tabulation (tabs+1) ++ "\"value\": " ++ show val ++ ",\n" ++ tabulation (tabs+1) ++ "\"left\" :" ++ toJSON' (tabs+1) l ++ ",\n" ++ tabulation (tabs+1) ++ "\"right\" :" ++ toJSON' (tabs+1) r ++ "\n" ++ tabulation tabs ++ "}"

instance ToJSON KDTree where
    toJSON :: KDTree -> String
    toJSON = toJSON' 0

--Сериализиране
serialise :: FilePath -> KDTree -> IO ()
serialise outFile tree =
    do  writeFile outFile $ toJSON tree

--Помощна фунцкия 1 за JSONtoReadable
clearJSON1 :: String -> String
clearJSON1 [] = []
clearJSON1 (c:cs)
 | c == '"' || c == '\t' || c == '\n' || c=='\'' = clearJSON1 cs
 | c == ':' = '=' : clearJSON1 cs
 | otherwise = c : clearJSON1 cs

--Помощна фунцкия 2 за JSONtoShow
clearJSON2 :: String -> String
clearJSON2 [] = []
clearJSON2 (c:cs)
  | isLetter c && not (isLetter $ head cs) = c : '\'' : clearJSON2 cs
  | c == '{' && head cs == 'p' = "Leaf'" ++ c: clearJSON2 cs
  | c == '{' && head cs == 'a' = "Tree'" ++ c: clearJSON2 cs
  | otherwise = c : clearJSON2 cs

--приема JSON и го превръща във формат, който може да бъде прочетен и парснат от CompressedTree
jsonToReadable :: String -> String
jsonToReadable = clearJSON2 . clearJSON1 

--приема дърво CompressedTree и му добавя допълнителната инфорамция за интервалите на точките
decompress :: CompressedTree -> KDTree
decompress (Leaf' l) = Leaf l $ hull l
decompress (Tree' axis val left' right') = Tree axis val left right (zipWith merge (borders left) (borders right))
 where left = decompress left'
       right = decompress right'


--десериализиране на дървото
deserialise :: FilePath -> IO KDTree
deserialise inFile =
    do contents <- readFile inFile
       let tree = decompress (read (jsonToReadable contents) :: CompressedTree)
       writeFile "KDTree/treeCopy.json" $ toJSON tree
       return tree


---------------------------------------------ТЕСТИНГ-------------------------
-- >>> deserialise "KDTree/tree.json" 
-- Tree {axis = 0, value = 1.0, left = Tree {axis = 0, value = -1.5, left = Tree {axis = 1, value = -0.5, left = Leaf {points = [[-2.0,-2.0]], borders = [(-2.0,-2.0),(-2.0,-2.0)]}, right = Leaf {points = [[-2.0,1.0]], borders = [(-2.0,-2.0),(1.0,1.0)]}, borders = [(-2.0,-2.0),(-2.0,1.0)]}, right = Tree {axis = 0, value = 0.0, left = Leaf {points = [[-1.0,0.0]], borders = [(-1.0,-1.0),(0.0,0.0)]}, right = Leaf {points = [[1.0,-2.0]], borders = [(1.0,1.0),(-2.0,-2.0)]}, borders = [(-1.0,1.0),(-2.0,0.0)]}, borders = [(-2.0,1.0),(-2.0,1.0)]}, right = Tree {axis = 1, value = 0.0, left = Tree {axis = 0, value = 2.5, left = Leaf {points = [[2.0,-1.0]], borders = [(2.0,2.0),(-1.0,-1.0)]}, right = Leaf {points = [[3.0,0.0]], borders = [(3.0,3.0),(0.0,0.0)]}, borders = [(2.0,3.0),(-1.0,0.0)]}, right = Leaf {points = [[2.0,2.0]], borders = [(2.0,2.0),(2.0,2.0)]}, borders = [(2.0,3.0),(-1.0,2.0)]}, borders = [(-2.0,3.0),(-2.0,2.0)]}

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

-- t' :: CompressedTree
-- t' = Tree' 0 0
--         (Tree'
--          1 0
--          (Leaf' [[-1.0, -2.0], [-2.0, -1.0]])
--          (Leaf' [[-1, 0.5],[0, 1]]))
--         (Tree'
--             1 0
--             (Leaf' [[1, -2], [2, -1]])
--             (Leaf' [[1, 0.5],[1, 1]]))
-- >>> serialise "KDtree.hs/out.json" (Leaf [[1, -2], [2, -1]] [(1,2),(-2,-1)])

-- >>> serialise "KDTree/out.json" t

-- >>> toJSON t
-- "{\n\t\"axis\": 0,\n\t\"val\": 0.0,\n\t\"left\" :{\n\t\t\"axis\": 1,\n\t\t\"val\": 0.0,\n\t\t\"left\" :{\n\t\t\t\"points\": [[-1.0,-2.0],[-2.0,-1.0]]\n\t\t},\n\t\t\"right\" :{\n\t\t\t\"points\": [[-1.0,0.5],[0.0,1.0]]\n\t\t}\n\t},\n\t\"right\" :{\n\t\t\"axis\": 1,\n\t\t\"val\": 0.0,\n\t\t\"left\" :{\n\t\t\t\"points\": [[1.0,-2.0],[2.0,-1.0]]\n\t\t},\n\t\t\"right\" :{\n\t\t\t\"points\": [[1.0,0.5],[1.0,1.0]]\n\t\t}\n\t}\n}"

-- >>> read (show t) :: KDTree

-- >>> jsonToShow . toJSON $ t
-- "Tree'{axis'= 0,value'= 0.0,left' =Tree'{axis'= 1,         value' = 0.0, left' = Leaf' {points' = [[-1.0,-2.0],[-2.0,-1.0]]}, right' =Leaf'{points'= [[-1.0,0.5],[0.0,1.0]]}},right' =Tree'{axis'= 1,value'= 0.0,left' =Leaf'{points'= [[1.0,-2.0],[2.0,-1.0]]},right' =Leaf'{points'= [[1.0,0.5],[1.0,1.0]]}}}"
-- >>> show t'
-- "Tree' {axis' = 0, value' = 0.0, left' = Tree' {axis' = 1, value' = 0.0, left' = Leaf' {points' = [[-1.0,-2.0],[-2.0,-1.0]]}, right' = Leaf' {points' = [[-1.0,0.5],[0.0,1.0]]}}, right' = Tree' {axis' = 1, value' = 0.0, left' = Leaf' {points' = [[1.0,-2.0],[2.0,-1.0]]}, right' = Leaf' {points' = [[1.0,0.5],[1.0,1.0]]}}}"

-- >>> read (jsonToShow . toJSON $ t) :: CompressedTree
-- Tree' {axis' = 0, value' = 0.0, left' = Tree' {axis' = 1, value' = 0.0, left' = Leaf' {points' = [[-1.0,-2.0],[-2.0,-1.0]]}, right' = Leaf' {points' = [[-1.0,0.5],[0.0,1.0]]}}, right' = Tree' {axis' = 1, value' = 0.0, left' = Leaf' {points' = [[1.0,-2.0],[2.0,-1.0]]}, right' = Leaf' {points' = [[1.0,0.5],[1.0,1.0]]}}}

-- >>> read "Tree{axis=0,value = 0.0, left = Tree {axis = 1, value = 0.0, left = Leaf {points = [[-1.0,-2.0],[-2.0,-1.0]], borders = [(-2.0,-1.0),(-2.0,-1.0)]}, right = Leaf {points = [[-1.0,0.5],[0.0,1.0]], borders = [(-1.0,0.0),(0.5,1.0)]}, borders = [(-2.0,0.0),(-2.0,1.0)]}, right = Tree {axis = 1, value = 0.0, left = Leaf {points = [[1.0,-2.0],[2.0,-1.0]], borders = [(1.0,2.0),(-2.0,-1.0)]}, right = Leaf {points = [[1.0,0.5],[1.0,1.0]], borders = [(1.0,1.0),(0.5,1.0)]}, borders = [(1.0,2.0),(-2.0,1.0)]}, borders = [(-2.0,2.0),(-2.0,2.0)]}" :: KDTree
-- Tree {axis = 0, value = 0.0, left = Tree {axis = 1, value = 0.0, left = Leaf {points = [[-1.0,-2.0],[-2.0,-1.0]], borders = [(-2.0,-1.0),(-2.0,-1.0)]}, right = Leaf {points = [[-1.0,0.5],[0.0,1.0]], borders = [(-1.0,0.0),(0.5,1.0)]}, borders = [(-2.0,0.0),(-2.0,1.0)]}, right = Tree {axis = 1, value = 0.0, left = Leaf {points = [[1.0,-2.0],[2.0,-1.0]], borders = [(1.0,2.0),(-2.0,-1.0)]}, right = Leaf {points = [[1.0,0.5],[1.0,1.0]], borders = [(1.0,1.0),(0.5,1.0)]}, borders = [(1.0,2.0),(-2.0,1.0)]}, borders = [(-2.0,2.0),(-2.0,2.0)]}
