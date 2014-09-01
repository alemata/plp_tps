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
      belongs 3 [(3, "A"), (0, "R"), (7, "G")]    `shouldBe` True
      belongs "k" []                              `shouldBe` False
      belongs "Argentina" [("Chile", 5), ("Bolivia", 9), ("Uruguay", 3)] `shouldBe` False
      [("H", [1]), ("E", [2]), ("Y", [0])] ? "R"  `shouldBe` False
      [("V", [1]), ("O", [2]), ("S", [0])] ? "V"  `shouldBe` True
      [] ? "V"  `shouldBe` False

    it "debe devolver el valor asociado a la clave dada, asumiendo que está definida" $ do  
      get "Argentina" [("Chile", 5), ("Argentina", 9), ("Uruguay", 3)] `shouldBe` 9
      get "Juan" [("Pedro", [(5,2),(3,3),(8,9)]), ("Luis", [(8,7),(10,3),(1,1)]), ("Juan", [(3,2),(99,100),(4,1)])] `shouldBe` [(3,2),(99,100),(4,1)]
      [( " calle " ," San Blas " ) ,( " ciudad " ," Hurlingham " )] ! " ciudad " `shouldBe` " Hurlingham "
      [( " perro " ," caniche " ) ,( " gato " ," siamés " ), ( " mono " ," chimpancé " )] ! " mono " `shouldBe` " chimpancé "

    it "agregar el nuevo valor con la función dada" $ do
      insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] [])) `shouldBe` [(1,"lab"),(2,"p")]
      insertWith (++) 1 [99,188] [(1,[1]),(2,[2])] `shouldBe` [(1,[1,99,188]),(2,[2])]
      insertWith (++) 3 [99] [(1,[1]),(2,[2])] `shouldBe` [(1,[1]),(2,[2]),(3,[99])]
      insertWith (+) 2 7 [(1,17),(2,22)] `shouldBe` [(1,17),(2,29)]
      insertWith (+) 3 7 [(1,17),(2,22)] `shouldBe` [(1,17),(2,22),(3,7)]

    it "agrupa por clave en algún orden" $ do
      groupByKey [("calle","Jean Jaures"),("ciudad","Brujas"),("ciudad","Kyoto"),("calle","7"),("calle","Lagrange"),("ciudad","Moscú"),("país","Venezuela")] `shouldMatchList` [("país",["Venezuela"]), ("ciudad",["Moscú","Kyoto","Brujas"]), ("calle",["Lagrange","7","Jean Jaures"])]
      groupByKey [("Chevrolet","Corsa"),("Fiat","Punto"),("Fiat","Spazio"),("Ford","Ka"),("Chevrolet","Spark"),("Ford","Fiesta"),("Fiat","Siena"),("Ford","Focus"),("Fiat","Uno")] `shouldMatchList` [("Fiat",["Uno","Siena","Spazio","Punto"]), ("Ford",["Focus","Fiesta","Ka"]), ("Chevrolet",["Spark","Corsa"])]

  describe "Utilizando Map Reduce" $ do
    it "visitas por monumento funciona en algún orden" $ do
      visitasPorMonumento [ "m1" ,"m2" ,"m3" ,"m2","m1", "m3", "m3"] `shouldMatchList` [("m3",3), ("m1",2), ("m2",2)] 
      visitasPorMonumento [ "Obelisco", "Cid", "San Martín", "Cid", "Bagdad Bridge", "Cid", "San Martín", "Obelisco"] `shouldMatchList` [("Obelisco",2), ("Cid",3), ("San Martín",2), ("Bagdad Bridge", 1)] 

    it "monumentosTop devuelve los más visitados en algún orden" $ do 
      monumentosTop [ "m1", "m0", "m0", "m0", "m2", "m2", "m3"] 
      `shouldSatisfy` (\res -> res == ["m0", "m2", "m3", "m1"] || res == ["m0", "m2", "m1", "m3"])