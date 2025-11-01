--Módulo cin la memoria que es lo que tienen los bots para tomar decisiones. La memoria representa aquelo que el bot sabe o cree de la partida
module Memoria where

import qualified Data.Map as Map
import Geometry
import Debug.Trace (trace)

-- Esta es la memoria del Robot, para que guarde la información recopilando de la partida
data Value = VInt Int 
           | VStr String 
           | VPoint Point
           | VBool Bool 
           | VFloat Float
           | VVector Vector
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
getInt key memory = case memGet key memory of
    Just (VInt n) -> Just n
    _             -> Nothing

-- Obtener flotante de memoria
getFloat :: Key -> Memoria -> Maybe Float
getFloat key memory = case memGet key memory of
    Just (VFloat f) -> Just f
    _               -> Nothing

-- Obtener cadena de memoria
getString :: Key -> Memoria -> Maybe String
getString key memory = case memGet key memory of
    Just (VStr s) -> Just s
    _             -> Nothing

-- Obtener booleano de memoria
getBool :: Key -> Memoria -> Maybe Bool
getBool key memory = case memGet key memory of
    Just (VBool b) -> Just b
    _              -> Nothing

-- Obtener punto de memoria
getPoint :: Key -> Memoria -> Maybe Point
getPoint key memory = case memGet key memory of
    Just (VPoint p) -> Just p
    _               -> Nothing

-- Obtener vector de memoria
getVector :: Key -> Memoria -> Maybe Vector
getVector key memory = case memGet key memory of
    Just (VVector v) -> Just v
    _                -> Nothing

-- Obtener referencia a robot 
getRobotRef :: Key -> Memoria -> Maybe Int
getRobotRef key memory = case memGet key memory of
    Just (RefRobotId id) -> Just id
    _                    -> Nothing

-- Obtener lista de enteros con valor por defecto
getListIntOr :: Key -> Memoria -> [Int]
getListIntOr key (Memoria m) =
    case Map.lookup key m of
        Just (ListIntValue lst) -> lst
        _                       -> []

-- Obtener lista de puntos con valor por defecto
getListPointOr :: Key -> Memoria -> [Point]
getListPointOr key (Memoria m) =
    case Map.lookup key m of
        Just (ListPointValue lst) -> lst
        _                         -> []


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
addToInt k v memoria = memStore k (VInt (valorActual + v)) memoria
  where
    valorActual = case getInt k memoria of
                    Just n  -> n
                    Nothing -> 0

-- Suma un float
addToFloat :: Key -> Float -> Memoria -> Memoria
addToFloat k v memoria = memStore k (VFloat (valorActual + v)) memoria
  where
    valorActual = case getFloat k memoria of
                    Just n  -> n
                    Nothing -> 0

-- Alterna un bool
altBool :: Key -> Memoria -> Memoria 
altBool k memoria = memStore k (VBool (not valorActual)) memoria
  where
    valorActual = case getBool k memoria of
                    Just b  -> b
                    Nothing -> False

-- Modifica entero
modifyInt :: Key -> (Int -> Int) -> Memoria -> Memoria
modifyInt key f memoria =
  case getInt key memoria of
    Just n  -> memStore key (VInt (f n)) memoria
    Nothing -> memoria

-- Modifica float
modifyFloat :: Key -> (Float -> Float) -> Memoria -> Memoria
modifyFloat key f memoria =
  case getFloat key memoria of
    Just x  -> memStore key (VFloat (f x)) memoria
    Nothing -> memoria

-- Agnadir un entero a una lista de enteros
appendInt :: Key -> Int -> Memoria -> Memoria
appendInt key val memoria =
  let oldList = case memGet key memoria of
                  Just (ListIntValue xs) -> xs
                  _ -> []
  in memStore key (ListIntValue (oldList ++ [val])) memoria

-- Agnadir un punto a una lista de puntos
appendPoint :: Key -> Point -> Memoria -> Memoria
appendPoint key p memoria =
  let oldList = case memGet key memoria of
                  Just (ListPointValue ps) -> ps
                  _ -> []
  in memStore key (ListPointValue (oldList ++ [p])) memoria