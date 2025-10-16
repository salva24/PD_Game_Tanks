module Geometry where


-- 1. TIPOS DEFINIDOS
-- Definimos un tipo de dato Vector que representa un vector genérico en 2D.
data Vector a = Vector a a
  deriving (Show, Eq)

type Point = Vector Float       -- Point. Un punto 2D en el espacio. Es un Vector de Float
type Angle = Float              -- Angulo en grados (con decimales)
type Distance = Float           -- Distance. Un valor de distancia con decimales.
type Position = Point           -- Position. Representa la posición de objeto en un mundo 2D.
-- Size = ((xmin,xmax) , (ymin, ymax))
type Size = ((Float,Float) , (Float,Float))

--  Vector es una instancia de Functor y Applicative
instance Functor Vector where
  fmap f (Vector x y) = Vector (f x) (f y)
instance Applicative Vector where
  pure a = Vector a a
  (Vector fx fy) <*> (Vector x y) = Vector (fx x) (fy y)

-- FUNCIONES
-- Distancia euclidiana entre 2 posiciones en el espacio.
distanceBetween :: Position -> Position -> Distance
distanceBetween (Vector x1 y1) (Vector x2 y2) = sqrt((dx)^2 + (dy)^2)
    where dx = x2 - x1
          dy = y2 - y1

-- Determina el ángulo desde una posición origen hacia una posición objetivo.
--  Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
angleToTarget :: Position -> Position -> Angle
angleToTarget (Vector x1 y1) (Vector x2 y2) = rad2deg (atan2 (y2-y1) (x2-x1))

-- Convertir un ángulo expresado en grados a su equivalente en radianes.
deg2rad :: Angle -> Angle
deg2rad gs = gs * pi / 180

--Convertir un ángulo expresado en radianes a su equivalente en grados.
rad2deg :: Angle -> Angle
rad2deg rs = rs * 180 / pi

-- Resta de dos vectores
subVec :: Vector Float -> Vector Float -> Vector Float
subVec v1 v2 = pure (-) <*> v1 <*> v2

-- Lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.
getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1,p2,p3,p4,ang) = map (rota rad) [p1,p2,p3,p4]
    where rad = deg2rad ang
          rota r (Vector x y) = Vector (x * cos r - y * sin r) (x * sin r + y * cos r)


-- Producto escalar entre dos puntos tratados como vectores
dot :: Point -> Point -> Float
dot (Vector x1 y1) (Vector x2 y2) = (x1 * x2) + (y1 * y2)

-- Resta dos puntos
-- Es igual a subVec pero mantenemos esta funcion por compatibilidad con versiones anteriores
sub :: Point -> Point -> Point
sub v1 v2 = subVec v1 v2

-- Vector perpendicular a un punto dado (tratado como vector).
perp :: Vector Float -> Vector Float
perp (Vector x y) = Vector (-y) x

-- Verifica si un punto está dentro de los límites definidos.
isInBounds :: Point -> Size -> Bool
isInBounds (Vector x y) ((xmin,xmax),(ymin,ymax)) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

-- Multiplicacion Escalar
multEscalar :: Float -> Vector Float -> Vector Float
multEscalar k v = pure (k *) <*> v

-- Suma 2 vectores
sumVec :: Vector Float -> Vector Float -> Vector Float
sumVec v1 v2 = pure (+) <*> v1 <*> v2

-- mul: tal que (w1,h1) `mul` (w2,h2) = (w1 * w2, h1 * h2)
mul :: Vector Float -> Vector Float -> Vector Float
mul p1 p2 = pure (*) <*> p1 <*> p2

--FUNCIONES RELACIONADAS CON COLISIONES PERO NO EXCLUSIVAMENTE
-- se usa en otro modulos 

--normaliza :: Vector Float -> Vector Float
--normaliza v@(Vector x y)
--   | n == 0    = Vector 0 0
--    | otherwise = (/n) <$> v
--    where n = sqrt (x^2 + y^2)

normaliza :: Vector Float -> Either String (Vector Float)
normaliza v@(Vector x y) 
    | n == 0    = Left "Se ha intentado normalizar el vector 0,0"
    | otherwise = Right ((/n) <$> v)
    where n = sqrt (x^2 + y^2)
    

vectorToAngle :: Vector Float -> Angle
vectorToAngle (Vector x y) = rad2deg (atan2 y x)