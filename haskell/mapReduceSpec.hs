--  Para correr los tests deben cargar en hugs el módulo Tests
--  y evaluar la expresión "main".
-- Algunas funciones que pueden utilizar para chequear resultados:
-- http://hackage.haskell.org/package/hspec-expectations-0.6.1/docs/Test-Hspec-Expectations.html#t:Expectation

import Test.Hspec
import MapReduce

main :: IO ()
main = hspec $ do
  describe "Utilizando Diccionarios" $ do
    it "puede determinarse si un elemento es una clave o no" $ do
      belongs 3 [(3, "A"), (0, "R"), (7, "G")]                                              `shouldBe` True
      belongs "k" []                                                                        `shouldBe` False
      belongs "Argentina" [("Chile", 5), ("Bolivia", 9), ("Uruguay", 3)]                    `shouldBe` False
      belongs "Argentina" [("Pedro", ["Berlin", "Paris"]), ("Juan", ["Peru", "Amsterdam"])] `shouldBe` False
      [("H", [1]), ("E", [2]), ("Y", [0])] ? "R"                                            `shouldBe` False
      [("V", [1]), ("O", [2]), ("S", [0])] ? "V"                                            `shouldBe` True
      [] ? "V"                                                                              `shouldBe` False
      [("Pedro", ["Berlin", "Paris"]), ("Juan", ["Peru", "Amsterdam"])] ? "Juana"           `shouldBe` False


    it "debe devolver el valor asociado a la clave dada, asumiendo que está definida" $ do
      get "Argentina" [("Chile", 5), ("Argentina", 9), ("Uruguay", 3)]                                `shouldBe` 9
      get "Juan" [("Pedro", [(5,2),(3,3)]), ("Luis", [(8,7)]), ("Juan", [(3,2),(99,100),(4,1)])]      `shouldBe` [(3,2),(99,100),(4,1)]
      [( " calle " ," San Blas " ) ,( " ciudad " ," Hurlingham " )] ! " ciudad "                      `shouldBe` " Hurlingham "
      [( " perro " ," caniche " ) ,( " gato " ," siamés " ), ( " mono " ," chimpancé " )] ! " mono "  `shouldBe` " chimpancé "
      [("Corrientes", [45, 30, 55]), ("Pacífico", [80, 33, 21, 11]), ("Alvear", [])] ! "Alvear"       `shouldBe` []


    it "agregar el nuevo valor con la función dada" $ do
      insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] []))  `shouldBe` [(1,"lab"),(2,"p")]
      insertWith (++) 1 [99,188] [(1,[1]),(2,[2])]                                        `shouldBe` [(1,[1,99,188]),(2,[2])]
      insertWith (++) 3 [99] [(1,[1]),(2,[2])]                                            `shouldBe` [(1,[1]),(2,[2]),(3,[99])]
      insertWith (+) 2 8 [(1,17),(2,22)]                                                  `shouldBe` [(1,17),(2,30)]
      insertWith (+) 3 7 [(1,17),(2,22)]                                                  `shouldBe` [(1,17),(2,22),(3,7)]
      insertWith (\n m -> if (n - m) < 0 then 0 else (n-m)) "pan" 80
        [("café", 30),("galletita", 20), ("pan", 15), ("chocolate", 3)]                   `shouldBe` [("café", 30),("galletita", 20), ("pan", 0), ("chocolate", 3)]
      insertWith (\n m -> if (n - m) < 0 then 0 else (n-m)) "agua" 10
        [("café", 30),("galletita", 20), ("pan", 15), ("chocolate", 3)]                   `shouldBe` [("café", 30),("galletita", 20), ("pan", 15), ("chocolate", 3), ("agua", 10)]


    it "agrupa por clave en algún orden" $ do
      groupByKey [("calle","Jean Jaures"),("ciudad","Brujas"),("ciudad","Kyoto"),
                  ("calle","7"),("calle","Lagrange"),("ciudad","Moscú"),("país","Venezuela")] `shouldMatchList` [("país",["Venezuela"]), ("ciudad",["Moscú","Kyoto","Brujas"]),
                                                                                                                 ("calle",["Lagrange","7","Jean Jaures"])]
      groupByKey [("Chevrolet","Corsa"),("Fiat","Punto"),("Fiat","Spazio"),
                  ("Ford","Ka"),("Chevrolet","Spark"),("Ford","Fiesta"),("Fiat","Siena"),
                  ("Ford","Focus"),("Fiat","Uno")]                                            `shouldMatchList` [("Fiat",["Uno","Siena","Spazio","Punto"]),
                                                                                                                 ("Ford",["Focus","Fiesta","Ka"]),
                                                                                                                 ("Chevrolet",["Spark","Corsa"])]
      groupByKey [("fruta", "Banana"), ("carne", "Picada"), ("verdura", "Espinaca")]          `shouldMatchList` [("fruta", ["Banana"]), ("carne", ["Picada"]),
                                                                                                                 ("verdura", ["Espinaca"])]
	it "une dos diccionarios utilizando la función que recibe" $ do
		unionWith (*) [("Algodon", 20), ("Azucar", 10)]  [("Miel", 5),("Algodon", 5)] `shouldMatchList` [("Algodon", 100), ("Azucar", 10),("Miel", 5)]

    it "divide la carga de manera balanceada entre una cantidad determinada de máquinas en algún orden" $ do
      distributionProcess 5 ["A", "B", "C", "D", "E", "F", "G", "H"]  `shouldMatchList` [["A", "F"], ["B", "G"], ["C", "H"], ["D"], ["E"]]
      distributionProcess 1 [1, 4, 100, 104]                          `shouldMatchList` [[1, 4, 100, 104]]
      distributionProcess 8 [1, 5, 60, 30]                            `shouldMatchList` [[1], [5], [60], [30], [], [], [], []]


    it "aplica el mapper a cada uno de los elementos y luego agrupa los resultados por clave" $ do
      mapperProcess (\x -> [(x,1)]) ["Pablo", "Pedro", "Pepe", "Guillermo", "Aldana"] `shouldMatchList` [("Pablo",[1]), ("Pedro",[1]), ("Pepe",[1]),
                                                                                                         ("Guillermo",[1]), ("Aldana",[1])]
      mapperProcess (\x -> [(x,1)]) ["Pablo", "Pedro", "Pepe", "Pedro", "Pedro",
                                     "Guillermo", "Aldana"]                           `shouldMatchList` [("Pablo",[1]), ("Pedro",[1,1,1]), ("Pepe",[1]),
                                                                                                         ("Guillermo",[1]), ("Aldana",[1])]
      mapperProcess (\(n,p) -> [(n,p),(n,p+p*5)]) [("Papa",32), ("Banana", 39),
                                                   ("Manzana", 19), ("Tomate", 24),
                                                   ("Banana", 58)]                    `shouldMatchList` [("Papa",[192,32]), ("Banana", [348,58,234,39]),
                                                                                                         ("Manzana", [114,19]), ("Tomate", [144,24])]


    it "combina los resultados de cada máquina y los combina agrupandolos por clave (ordenandolos en forma creciente)" $ do
      combinerProcess [[("Pablo",[1]), ("Pedro",[1,1,1])], [("Pepe",[1])],
                       [("Guillermo",[1]), ("Aldana",[1])]]                             `shouldBe` [("Aldana",[1]), ("Guillermo",[1]),
                                                                                                    ("Pablo",[1]), ("Pedro",[1,1,1]), ("Pepe",[1])]
      combinerProcess [[(56,["Amsterdam", "Florencia"]), (23, ["Berlin"])],
                       [(56,["Ulm", "Buenos Aires"]),(87,["Viena","Paris","Amsterdam"]),
                       (23,["Berlin"])], [(77,["Chile", "Nueva York", "Tokio"])],
                       [(1,["Paraguay"])]]                                              `shouldBe` [(1,["Paraguay"]), (23, ["Berlin", "Berlin"]),
                                                                                                    (56,["Amsterdam", "Florencia", "Ulm", "Buenos Aires"]),
                                                                                                    (77,["Chile", "Nueva York", "Tokio"]),
                                                                                                    (87,["Viena","Paris","Amsterdam"])]


    it "aplica el reducer sobre cada par de elementos y luego aplana el resultado para unificar las soluciones" $ do
      reducerProcess (\(x,y) -> [x])      [("Alvear",[3,5,1,32]),("Cabildo",[4,6,4,4,4]),("Independencia",[1,1])]  `shouldBe` ["Alvear", "Cabildo", "Independencia"]
      reducerProcess (\(x,y) -> [sum y])  [("Alvear",[3,5,1,32]),("Cabildo",[4,6,4,4,4]),("Independencia",[1,1])]  `shouldBe` [41, 22, 2]
      reducerProcess (\(n,l) -> [l])      [("Chocotorta", ["Chocolinas", "Dulce de Leche", "Crema"]),
                                           ("Galletitas de Limón", ["Limón", "Harina", "Azúcar"]),
                                           ("Budín de Banana", ["Banana", "Harina", "Manteca"])]                   `shouldBe` [["Chocolinas", "Dulce de Leche", "Crema"],
                                                                                                                               ["Limón", "Harina", "Azúcar"],
                                                                                                                               ["Banana", "Harina", "Manteca"]]


    it "aplica la técnica MapReduce para 100 máquinas" $ do
      mapReduce (\(nom,ciudad) -> [(ciudad,1)]) (\(ciudad,cant) -> [(ciudad, sum cant)])
                [("Pablo","Berlin"), ("Gabriela","Amsterdam"), ("Taihú","Cairo"), ("Pablo", "Cairo"),
                 ("Taihú","Amsterdam"), ("Juan","Amsterdam")] `shouldBe` [("Amsterdam", 3), ("Berlin", 1), ("Cairo", 2)]

  describe "Utilizando Map Reduce" $ do
    it "visitas por monumento funciona en algún orden" $ do
      visitasPorMonumento [ "m1" ,"m2" ,"m3" ,"m2","m1", "m3", "m3"]          `shouldMatchList` [("m3",3), ("m1",2), ("m2",2)]
      visitasPorMonumento [ "Obelisco", "Cid", "San Martín", "Cid",
                            "Bagdad Bridge", "Cid", "San Martín", "Obelisco"] `shouldMatchList` [("Obelisco",2), ("Cid",3), ("San Martín",2), ("Bagdad Bridge", 1)]

    it "monumentos top devuelve una lista ordenada según cuántas veces haya sido visitado un monumento" $ do
      monumentosTop [ "m1", "m0", "m0", "m0", "m2", "m2", "m3"]  `shouldSatisfy` (\res -> res == ["m0", "m2", "m3", "m1"] || res == ["m0", "m2", "m1", "m3"])
      monumentosTop [ "Obelisco", "Obelisco", "Cid", "San Martin", "San Martin", "San Martin", "Obelisco", "Obelisco"]  `shouldBe` ["Obelisco", "San Martin", "Cid"]


    it "monumentos por país determina la cantidad de monumentos existen por cada país" $ do
      monumentosPorPais [ (Monument, [("name","Obelisco"),("latlong","-36.6033,-57.3817"),("country", "Argentina")]),
                          (Street, [("name","Int. Güiraldes"),("latlong","-34.5454,-58.4386"),("country", "Argentina")]),
                          (Monument, [("name", "San Martín"),("country", "Argentina"),("latlong", "-34.6033,-58.3817")]),
                          (City, [("name", "Paris"),("country", "Francia"),("latlong", "-24.6033,-18.3817")]),
                          (Monument, [("name", "Bagdad Bridge"),("country", "Irak"),("new_field", "new"),("latlong", "-11.6033,-12.3817")])] 
                                                                                `shouldMatchList` [("Argentina",2),("Irak",1)]
      monumentosPorPais [ (Monument, [("Antiguedad","muchos años"),("name","Catedral de Leon"),("country", "España")]),
                          (Monument, [("name","Cristo-Rei de Almada"),("Renovacion", "ninguna"),("country", "Portugal")]),
                          (Monument, [("name", "Monasterio de Piedra"),("country", "España")]),
                          (Monument, [("name", "Iglesia de San Francisco"),("country", "Argentina")]),
                          (Street, [("name", "Corrientes"),("country", "Argentina")]),
                          (City, [("name", "Berlina"),("country", "Alemania"),("habitantes", "Dos millones")]),
                          (Street, [("name", "Armenia"),("country", "Argentina"),("Barrio", "Palermo")])] 

                                                                                `shouldMatchList` [("Argentina",1),("España",2),("Portugal",1)]
