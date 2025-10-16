module Colision where

import Entidades
import Geometry

-- Evento de colisión
data CollisionEvent = 
    RobotProjectileCollision Robot Proyectil |
    RobotRobotCollision Robot Robot
    deriving (Show, Eq)

-- Tipo rectángulo (figura robot y o misil)
data Rectangle = Rectangle {
    center :: Point,
    size :: (Float, Float),  -- (ancho, alto)
    angle :: Angle
} deriving (Show, Eq)

-- Convertir robot en rectángulo
robotToRectangle :: Robot -> Rectangle
robotToRectangle robot = Rectangle {
    center = posicion robot,
    size = (ancho robot, alto robot),
    angle = vectorToAngle (direccion robot)
}

-- Convertir proyectil en rectángulo
projectileToRectangle :: Proyectil -> Rectangle
projectileToRectangle proyectil = Rectangle {
    center = posicion proyectil,
    size = (ancho proyectil, alto proyectil),
    angle = vectorToAngle (direccion proyectil)
}

-- Función para comprobar que dos proyecciones se solapan
solapan :: (Float, Float) -> (Float, Float) -> Bool
solapan (min1, max1) (min2, max2) = max1 >= min2 && max2 >= min1

-- Proyección de un punto sobre un eje
projectPoint :: Point -> Vector -> Float
projectPoint p axis = dot p axis

-- Proyección de los vértices de un polígono sobre un eje.
--  recibe 4 puntos del rectangulo y el eje ==> devuelve el min y max coordenada respecto del
--  eje de forma que queda acotada la sombra del rectángulo en el mismo
projectVertices :: [Point] -> Vector -> (Float, Float)
projectVertices vertices axis = (minimum projections, maximum projections)
    where projections = [projectPoint v axis | v <- vertices]

-- Función para hallar los vectores perpendiculares de los lados de un rectángulo
-- Solo funciona para rectángulos porque se devuelve la mitad de ejes (2 en total)
-- ya que los otros 2 son paralelos a los primeros
getSeparatingAxes :: [Point] -> [Vector]
getSeparatingAxes vertices = take (length axes `div` 2) axes
    where edges = zipWith sub (tail vertices ++ [head vertices]) vertices
          axes  = [normaliza (perp edge) | edge <- edges]
-- Lados calculados
-- Lado superior = v2 - v1 , Lado derecho = v3 - v2 , Lado inferior = v4 - v3 , Lado izquierdo = v1 - v4 

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

-- Comprueba si dos rectángulos han colisionado usando SAT
checkCollision :: Rectangle -> Rectangle -> Bool
checkCollision rect1 rect2 = all checkAxis allejes
    where
        vertices1 = rectangleToVertices rect1
        vertices2 = rectangleToVertices rect2
        
        -- Obtenemos los ejes de ambos rectángulos
        ejes1 = getSeparatingAxes vertices1
        ejes2 = getSeparatingAxes vertices2
        allejes = ejes1 ++ ejes2
        
        -- Para cada eje, verificamos si las proyecciones se solapan
        checkAxis eje = solapan proj1 proj2 -- Miramos si las dos tuplas se cortan, i.e. si las sombras de los dos rectángulos en el eje se solapan
            where proj1 = projectVertices vertices1 eje -- Coordenada minima y máxima respecto al eje axis de forma que la sombra del rectángulo queda acotada en el eje
                  proj2 = projectVertices vertices2 eje


-- Funcion para detectar colisiones entre robots y proyectiles
detectRobotProjectileCollisions :: [Robot] -> [Proyectil] -> [CollisionEvent]
detectRobotProjectileCollisions robots proyectiles = 
    [RobotProjectileCollision r p |
        r <- robots, isRobotAlive r,-- Un robot que siga en la simulación
        p <- proyectiles, id_entidad r /= getIdLanzador p, -- Evitamos que cuando se dispara un misil, el robot que lo ha lanzado colisione con él
        checkCollision (robotToRectangle r) (projectileToRectangle p) -- Comprobamos si han colisionado
    ]


-- Función para detectar colisiones entre robots
detectRobotRobotCollisions :: [Robot] -> [CollisionEvent]
detectRobotRobotCollisions robots = 
    [RobotRobotCollision r1 r2 |
    r1 <- robots, isRobotAlive r1,-- Un robot que siga en la simulación
    r2 <- robots, isRobotAlive r2,
    id_entidad r1 < id_entidad r2, -- Evitamos que un robot colisione consigo mismo y que se repitan las colisiones (para que no salga la colision r1 r2 y luego r2 r1)
    checkCollision (robotToRectangle r1) (robotToRectangle r2) -- Comprobamos si han colisionado
    ]


-- Función principal que coordina todas las comprobaciones de colisión.
checkCollisions :: [Robot] -> [Proyectil] -> [CollisionEvent]
checkCollisions robots proyectiles = detectRobotProjectileCollisions robots proyectiles ++ detectRobotRobotCollisions robots