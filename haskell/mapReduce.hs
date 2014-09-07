module MapReduce where

import Data.Ord
import Data.List
import Data.Maybe

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- La función "belongs" define si cierto elemento pertenece a las claves del diccionario dado como argumento
belongs :: Eq k => k -> Dict k v -> Bool
belongs k d = not (null [x | x <- d, fst x == k])

(?) :: Eq k => Dict k v -> k -> Bool
d ? k = belongs k d

-- La función "get" retorna el significado correspondiente a la clave dada como argumento,
-- se asume que el mismo se encuentra definido.
get :: Eq k => k -> Dict k v -> v
get k d = head ([snd x | x <- d, fst x == k])

(!) :: Eq k => Dict k v -> k -> v 
d ! k = get k d

-- La función "insertWith" incorpora un nuevo significado a una clave dada, utilizando como función de
-- incorporación a aquella dada como argumento. Si no existe la clave en el diccionario, entonces se 
-- la misma será agregada.
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith fadd k v d
  | not (d ? k) = d ++ [(k,v)]
  | d ? k       = [if (fst x == k) then (fst x, fadd (snd x) v ) else x | x <- d]

-- En este caso, se ordenará los datos del array pasado como parámetro para que pase a tener
-- una estructura de tipo Diccionario. Se asociará cada clave con la lista de todos los valores que tenía 
-- en el array original.
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
groupByKey l = foldr (\x xs -> insertWith (++) (fst x) [(snd x)] xs) [] l

-- En unionWith se espera que a partir de dos diccionarios y una función de unificación, 
-- se combinen ambos para obtener un único diccionario resultante. 
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f dict = foldr (\x rec_dict -> insertWith f (fst x) (snd x) rec_dict) dict

--------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]

-- "distributionProcess" tendrá como objetivo que a partir de una lista divide la carga de la misma de manera
-- balanceada entre una determinada cantidad de sublistas. 
distributionProcess :: Int ->  [a] -> [[a]]
distributionProcess n = foldr (\x rec -> (tail rec) ++ [x : (head rec)]) (replicate n [])
-- distributionProcess 5 [1,2,3,4,5,6,7,8,9,10,11,12] =>
-- [[1 ,6 ,11] ,[2 ,7 ,12] ,[3 ,8] ,[4 ,9] ,[5 ,10]]

-- La función "mapperProcess" deberá aplicar el tipo Mapper a cada uno de los elementos pasados 
-- en el array por parámetro y luego agrupar los resultados por claves. 
mapperProcess :: Eq k => Mapper a k v -> [a] -> [(k,[v])]
mapperProcess m la = groupByKey (concat [m e | e <- la])

-- "combinerProcess" consiste en unificar los resultados de cada sublista pasada por parámetro
-- agrupándolos por clave y ordenandolos de manera creciente según la clave. 
combinerProcess :: (Eq k, Ord k) => [[(k, [v])]] -> [(k,[v])]
combinerProcess l = sortBy (comparing $ fst) (foldr (\x rec -> unionWith (++)  x rec) [] l)

-- La función "reducerProcess" aplica el tipo Reducer a cada para de elementos de la lista 
-- pasada como argumento y luego combina los resultados para obtener una única lista. 
reducerProcess :: Reducer k v b -> [(k, [v])] -> [b]
reducerProcess r l = concat [r e | e <- l]

-- La función "mapReduce" combina todas las instrucciones desarrolladas de la Sección 2, para poder
-- implementar una técnica de MapReduce, que permite en tiempo óptimo procesar grandes cantidades de datos.
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce m r l = reducerProcess r (combinerProcess [mapperProcess m x | x <- dist])
  where dist = distributionProcess 100 l

-- La función "visitasPorMonumento" deberá, a partir de un array de nombres de monumentos repetidos
-- generar un Diccionario que indique la cantidad de repeticiones que hay por cada uno. 
visitasPorMonumento :: [String] -> Dict String Int
visitasPorMonumento m = mapReduce (\l -> [(l,1)]) (\(x,y) -> [(x,sum y)]) m

-- En este caso, "monumentosTop" deberá, a partir de una lista de nombres de monumentos repetidos según 
-- la cantidad de visitas, ordenarlos por cantidad de visitas, de mayor a menor, sin repetidos. 
monumentosTop :: [String] -> [String]
monumentosTop m = mapReduce (\(x,y) -> [(-y,x)]) (\(x,y) ->  y) (visitasPorMonumento m)

-- "monumentosPorPais" indica la cantidad de monumentos existentes para cada país que tenga
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
