module Memoria where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Geometry

-- Esta es la memoria del Robot, para que guarde la información recopilando de la partida
data Value = 
    VInt Int 
    | VStr String 
    | VPoint Point
    | VBool Bool 
    | VFloat Float
    | VVector (Vector Float)
    | ListIntValue [Int]
    | ListPointValue [Point]
    | RefRobotId Int
    deriving (Eq, Show)

type Key = String

-- La memoria es un diccionario String -> Value
newtype Memoria = Memoria (Map.Map Key Value)
    deriving (Eq, Show)


-- Crear memoria vacía
memEmpty :: Memoria
memEmpty = Memoria Map.empty 


-- Guardar un valor en la memoria
memStore :: Key -> Value -> Memoria -> Memoria
memStore k v (Memoria m) = Memoria (Map.insert k v m)


-- Leer un valor de la memoria (de forma general)
memGet :: Key -> Memoria -> Maybe Value
memGet k (Memoria m) = Map.lookup k m

-- Obtener valores de la memoria (de forma concreta)
-- Gets por tipo
getInt :: Key -> Memoria -> Maybe Int
getInt k = fmap (\(VInt n) -> n) . memGet k

getFloat :: Key -> Memoria -> Maybe Float
getFloat k = fmap (\(VFloat f) -> f) . memGet k

getString :: Key -> Memoria -> Maybe String
getString k = fmap (\(VStr s) -> s) . memGet k

getBool :: Key -> Memoria -> Maybe Bool
getBool k = fmap (\(VBool b) -> b) . memGet k

getPoint :: Key -> Memoria -> Maybe Point
getPoint k = fmap (\(VPoint p) -> p) . memGet k

getVector :: Key -> Memoria -> Maybe (Vector Float)
getVector k = fmap (\(VVector v) -> v) . memGet k

getRobotRef :: Key -> Memoria -> Maybe Int
getRobotRef k = fmap (\(RefRobotId i) -> i) . memGet k

-- Obtener lista de enteros con valor por defecto
getListIntOr :: Key -> Memoria -> Maybe [Int]
getListIntOr k (Memoria m) =
  (fmap (\(ListIntValue xs) -> xs) (Map.lookup k m))

getListPointOr :: Key -> Memoria -> Maybe [Point]
getListPointOr k (Memoria m) =
  (fmap (\(ListPointValue ps) -> ps) (Map.lookup k m))


-- Elimina un valor de la memoria
memDelete :: Key -> Memoria -> Memoria
memDelete clave (Memoria m) = Memoria (Map.delete clave m) 


-- Limpiar toda la memoria
memClear :: Memoria -> Memoria
memClear _ = Memoria Map.empty


-- Devuelve todos los valores de la memoria Lista clave:valor
memList :: Memoria -> [(Key,Value)]
memList (Memoria m) = Map.toList m

-- Lista con todas las claves almacenadas en la memoria
memKeys :: Memoria -> [Key]
memKeys (Memoria m) = Map.keys m


-- Funciones de modificación e incremento
-- Sumar un entero
addToInt :: Key -> Int -> Memoria -> Memoria
addToInt k v memoria =
  let valorActual = fromMaybe 0 (getInt k memoria)
  in memStore k (VInt (valorActual + v)) memoria

addToFloat :: Key -> Float -> Memoria -> Memoria
addToFloat k v memoria =
  let valorActual = fromMaybe 0 (getFloat k memoria)
  in memStore k (VFloat (valorActual + v)) memoria

altBool :: Key -> Memoria -> Memoria 
altBool k memoria =
  let valorActual = fromMaybe False (getBool k memoria)
  in memStore k (VBool (not valorActual)) memoria

modifyInt :: Key -> (Int -> Int) -> Memoria -> Memoria
modifyInt k f memoria =
  maybe memoria (\n -> memStore k (VInt (f n)) memoria) (getInt k memoria)

modifyFloat :: Key -> (Float -> Float) -> Memoria -> Memoria
modifyFloat k f memoria =
  maybe memoria (\x -> memStore k (VFloat (f x)) memoria) (getFloat k memoria)

-- Añadir un entero a una lista de enteros
appendInt :: Key -> Int -> Memoria -> Memoria
appendInt k val memoria =
  let oldList = fromMaybe [] (getListIntOr k memoria)
  in memStore k (ListIntValue (oldList ++ [val])) memoria

appendPoint :: Key -> Point -> Memoria -> Memoria
appendPoint k p memoria =
  let oldList = fromMaybe [] (getListPointOr k memoria)
  in memStore k (ListPointValue (oldList ++ [p])) memoria