module Partida where


import Entidades
import Geometry


-- Tipos de datos

-- Tiempo transcurrido en la partida
type Tiempo = Double

-- Estado genérico del juego: contiene una lista de entidades (del tipo que sea)
-- y un valor de tiempo. El tipo 'a' puede ser Robot, Proyectil, Explosion, etc.
data Estado a = Estado {
    entidades :: [a],
    tiempo    :: Tiempo
} deriving (Show, Eq)


-- Instancias de Functor y Applicative

-- Permite aplicar funciones a todas las entidades dentro del estado
instance Functor Estado where
    fmap f (Estado xs t) = Estado (map f xs) t

-- Permite aplicar funciones dentro del contexto del estado
instance Applicative Estado where
    pure x = Estado [x] (0.0)
    (Estado fs t1) <*> (Estado xs t2) =
        Estado [f x | f <- fs, x <- xs] (max t1 t2)


-- Funciones auxiliares sobre el tiempo

-- Incrementa el tiempo del estado en dt segundos
actualizarTiempo :: Double -> Estado a -> Estado a
actualizarTiempo dt estado =
    estado { tiempo = dt + (tiempo estado) }


-- Definicion de

data Explosion = Explosion { posicion_explosion :: Position }
    deriving (Eq, Show)



-- Funciones sin hacer que en un futuro actualizarán las entidades que están dentro del estado despues del tiempo transcurrido dt
updateRobot :: Double -> Robot -> Robot
updateRobot dt r = r

updateProyectil :: Double -> Proyectil -> Proyectil
updateProyectil dt p = p

updateExplosion :: Double -> Explosion -> Explosion
updateExplosion dt exp = exp


-- Actualización general del estado del juego


-- Usamos <$> (fmap) y pure para construir transformaciones declarativas
updateGameState ::
    Double ->
    Estado Robot -> Estado Proyectil -> Estado Explosion ->
    (Estado Robot, Estado Proyectil, Estado Explosion)
updateGameState dt estadoConRobots estadoConProyectiles estadoConExplosiones =
    let
        -- Actualizamos entidades
        estadoRobots' = updateRobot dt <$> estadoConRobots
        estadoProyectiles' = updateProyectil dt <$> estadoConProyectiles
        estadoExplosiones' = updateExplosion dt <$> estadoConExplosiones
        
        -- Actualizamos tiempo
        estadoRobotsFinal = actualizarTiempo dt estadoRobots'
        estadoProyectilesFinal = actualizarTiempo dt estadoProyectiles'
        estadoExplosionlesFinal = actualizarTiempo dt estadoExplosiones'
    in
        (estadoRobotsFinal, estadoProyectilesFinal, estadoExplosionlesFinal)
