-- Módulo con funciones de geometría básica

module Geometry where
import Hyperparams

-- 1. TIPOS DEFINIDOS
type Point = (Float,Float)  -- Point. Un punto 2D en el espacio.
type Vector = (Float,Float) -- Vector siempre se considera que empieza en (0,0)
type Angle = Float          -- Angulo en grados (con decimales)
type Distance = Float       -- Distance. Un valor de distancia con decimales.
type Position = Point       -- Position. Representa la posición de objeto en un mundo 2D.

-- FUNCIONES
-- Distancia euclidiana entre 2 posiciones en el espacio.
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1,y1) (x2,y2) = sqrt(dx^2 + dy^2)
    where dx = x2 - x1
          dy = y2 - y1

-- Determina el ángulo desde una posición origen hacia una posición objetivo.
--  Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
angleToTarget :: Position -> Position -> Angle
angleToTarget (x1,y1) (x2,y2) = rad2deg (atan2 (y2-y1) (x2-x1))

-- Convertir un ángulo expresado en grados a su equivalente en radianes.
deg2rad :: Angle -> Angle
deg2rad gs = gs * pi / 180

--Convertir un ángulo expresado en radianes a su equivalente en grados.
rad2deg :: Angle -> Angle
rad2deg rs = rs * 180 / pi

-- Resta de dos vectores
subVec :: Vector -> Vector -> Vector
subVec (x1,y1) (x2,y2) = (x1 - x2 , y1 - y2)

-- Lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.
getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1,p2,p3,p4,ang) = map (rota rad) [p1,p2,p3,p4]
    where rad = deg2rad ang
          rota r (x,y) = (x * cos r - y * sin r, x * sin r + y * cos r)

-- Producto escalar entre dos puntos tratados como vectores
dot :: Point -> Point -> Float
dot (x1,y1) (x2,y2) = (x1 * x2) + (y1 * y2)

-- Resta dos puntos
sub :: Point -> Point -> Point
sub (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

-- Vector perpendicular a un punto dado (tratado como vector).
perp :: Vector -> Vector
perp (x,y) = (-y,x)

-- Verifica si un punto está dentro de los límites definidos.
isInBounds :: Point -> Size -> Bool
isInBounds (x,y) ((xmin,xmax),(ymin,ymax)) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

-- Multiplicacion Escalar
multEscalar :: Float -> Vector -> Vector
multEscalar k (x,y) = (k*x, k*y)

-- Suma 2 vectores
sumVec :: Vector -> Vector -> Vector
sumVec (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
mul :: (Float,Float) -> (Float,Float) -> (Float,Float)
(w,h) `mul` (sw,sh) = (w * sw, h * sh)

--FUNCIONES RELACIONADAS CON COLISIONES PERO NO EXCLUSIVAMENTE
-- Tipo rectángulo (figura robot y o misil)
data Rectangle = Rectangle {
    center :: Point,
    size :: (Float, Float),  -- (ancho, alto)
    angleRectang :: Angle -- angulo de direccion del rectangulo (con el se puede sacar el angulo de rotacion)
} deriving (Show, Eq)

-- se usa en otro modulos 
normaliza :: Vector -> Vector
normaliza (x, y)
    | n == 0  = (0, 0)
    | otherwise = (x / n, y / n)
    where n = sqrt (x^2 + y^2)

-- Devuelve el ángulo de un= vector entre 0 y 360 grados
vectorToAngle :: Vector -> Angle
vectorToAngle (x, y) = normalizedAngle
    where
        angle = rad2deg (atan2 y x) -- esto lo devuelve entre -180 y 180
        normalizedAngle = if angle < 0 then angle + 360 else angle

-- Convertir ángulo a vector unitario
angleToVector :: Angle -> Vector
angleToVector angle = (cos rad, sin rad)
    where rad = deg2rad angle

-- Función que convierte un rectángulo en sus 4 puntos
rectangleToVertices :: Rectangle -> [Point]
rectangleToVertices (Rectangle (cx, cy) (w, h) ang) = [(cx + x, cy + y) | (x, y) <- rotatedPoints]
    where halfW = w / 2
          halfH = h / 2
          -- Definimos los 4 vértices
          p1 = (-halfW, -halfH)  -- inferior izquierda
          p2 = (halfW, -halfH)   -- inferior derecha
          p3 = (halfW, halfH)    -- superior derecha
          p4 = (-halfW, halfH)   -- superior izquierda
          -- Usamos getVertices para rotar los puntos
          rotatedPoints = getVertices (p1, p2, p3, p4, ang)

-----------------------------------------------------------
-- comprueba si un rectangulo se sale de los limites dados
-- Sirve para comprobar si el cuerpp de un robot, el cagnon de un robot o un proyectil se sale de los limites del mapa
isRectangleOutOfBounds :: Rectangle -> Bool
isRectangleOutOfBounds rect = any (\(x, y) -> not (isInBounds (x, y) globalBounds)) vertices -- si algún vértice está fuera de los límites
    where vertices = rectangleToVertices rect -- obtenemos los vertices del rectangulo

-- Resta de angulos, devuelve el delta entre el primero menos el segundo en el sentido de giro más corto. Negativo para giro horario, positivo antihorario
calculaDeltaAngulo :: Angle -> Angle -> Angle
calculaDeltaAngulo ang1 ang2
    | delta > 180 = delta - 360 -- giro antihorario más corto
    | delta < -180 = delta + 360 -- giro horario más corto
    | otherwise = delta
    where delta = ang1 - ang2