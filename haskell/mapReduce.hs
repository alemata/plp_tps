module MapReduce where

import Data.Ord
import Data.List
import Data.Maybe

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs k d = not (null [x | x <- d, fst x == k])

(?) :: Eq k => Dict k v -> k -> Bool
d ? k = belongs k d
--Main> [("calle",[3]),("city",[2,1])] ? "city" => True

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get k d = head ([snd x | x <- d, fst x == k])

(!) :: Eq k => Dict k v -> k -> v
d ! k = get k d
--Main> [("calle",[3]),("city",[2,1])] ! "city" => [2,1]

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith fadd k v d
  | not (d ? k) = d ++ [(k,v)]
  | d ? k       = [if (fst x == k) then (fst x, fadd (snd x) v ) else x | x <- d]
--Main> insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] [])) => [(1,"lab"),(2,"p")]

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
groupByKey l = foldr (\x xs -> insertWith (++) (fst x) [(snd x)] xs) [] l
-- groupByKey [("calle","Jean Jaures"),("ciudad","Brujas"), ("ciudad","Kyoto"),("calle","7")] =>
--    [("calle",["Jean␣Jaures","7"]),("ciudad",["Brujas","Kyoto"])]

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f dict = foldr (\x rec_dict -> insertWith f (fst x) (snd x) rec_dict) dict
--Main> unionWith (++) [("calle",[3]),("city",[2,1])] [("calle", [4]), ("altura", [1,3,2])] =>
--  [("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])]
--
--Main> unionWith (+) [("rutas",3)] [("rutas", 4), ("ciclos", 1)] =>
--  [("rutas",7),("ciclos",1)]


-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]

-- Ejercicio 6
distributionProcess :: Int ->  [a] -> [[a]]
distributionProcess n = foldr (\x rec -> (tail rec) ++ [x : (head rec)]) (replicate n [])
-- distributionProcess 5 [1,2,3,4,5,6,7,8,9,10,11,12] =>
-- [[1 ,6 ,11] ,[2 ,7 ,12] ,[3 ,8] ,[4 ,9] ,[5 ,10]]

-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> [(k,[v])]
mapperProcess m la = groupByKey (concat [m e | e <- la])

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [[(k, [v])]] -> [(k,[v])]
combinerProcess l = sortBy (comparing $ fst) (foldr (\x rec -> unionWith (++)  x rec) [] l)

-- Ejercicio 9
reducerProcess :: Reducer k v b -> [(k, [v])] -> [b]
reducerProcess r l = concat [r e | e <- l]

-- Ejercicio 10
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce m r l = reducerProcess r (combinerProcess [mapperProcess m x | x <- dist])
  where dist = distributionProcess 100 l
--mapReduce (\(x,y) -> [(x, y)]) (\(x,y) -> [(x,sum y)]) [("pablo", 1), ("pablo",1)]

-- Ejercicio 11
visitasPorMonumento :: [String] -> Dict String Int
visitasPorMonumento m = mapReduce (\l -> [(l,1)]) (\(x,y) -> [(x,sum y)]) m
--visitasPorMonumento ["m1","m2","m3","m2"] =>
--  [("m1",1),("m2",2),("m3",1)]

-- Ejercicio 12
monumentosTop :: [String] -> [String]
monumentosTop m = mapReduce (\(x,y) -> [(y,x)]) (\(x,y) ->  y) (visitasPorMonumento m)

-- Ejercicio 13 
monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais s = mapReduce (\(s,d) -> [(d ! "country", initNumber s)]) (\(x,y) -> if sum y /= 0 then [(x,sum y)] else []) s

initNumber :: Structure -> Int
initNumber Monument = 1
initNumber Street = 0
initNumber City = 0

-- ------------------------ Ejemplo de datos del ejercicio 13 ----------------------
data Structure = Street | City | Monument deriving Show

items :: [(Structure, Dict String String)]
items = [
    (Monument, [
      ("name","Obelisco"),
      ("latlong","-36.6033,-57.3817"),
      ("country", "Argentina")]),
    (Street, [
      ("name","Int. Güiraldes"),
      ("latlong","-34.5454,-58.4386"),
      ("country", "Argentina")]),
    (Monument, [
      ("name", "San Martín"),
      ("country", "Argentina"),
      ("latlong", "-34.6033,-58.3817")]),
    (City, [
      ("name", "Paris"),
      ("country", "Francia"),
      ("latlong", "-24.6033,-18.3817")]),
    (Monument, [
      ("name", "Bagdad Bridge"),
      ("country", "Irak"),
      ("new_field", "new"),
      ("latlong", "-11.6033,-12.3817")])
    ]

------------------------------------------------
------------------------------------------------
