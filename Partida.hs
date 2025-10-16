module Partida where

-- Este script no está implementado completamente, la idea futura es añadir todo lo relacionado con la partida
-- para que el juego funcione correctamente, se hará a futuro

import Entidades
import Geometry

-- Explosion
data Explosion = Explosion {
    posicion_explosion :: Position
} deriving (Eq,Show)

-- Tiempo transcurrido en el juego
newtype Tiempo = Tiempo{ 
    tiempoTranscurrido :: Int  -- número de steps transcurridos
      } deriving (Show, Eq, Ord )

--Estado actual de la partida en el que se encuentra la lista de robots, proyectiles y explosiones
-- además del tiempo, se va actualizando constantemente para ir cmabiando sus datos.
data Estado = Estado{
    allExplosiones :: [Explosion],   -- Lista con todas las explosiones activas
    allRobots :: [Robot],            -- Lista con todos los tanques vivos
    allProyectiles :: [Proyectil],   -- Lista con todos los proyectiles activos  
    tiempo :: Tiempo
}


-- Elimina un robot sin vida de la lista de robots activos de la partida
eliminarRobotDelEstado :: Robot -> Estado -> Estado
eliminarRobotDelEstado robotABorrar estado = estado { allRobots = nuevosRobots (allRobots estado) }
    where nuevosRobots :: [Robot] -> [Robot]
          nuevosRobots [] = []
          nuevosRobots (r:resto)
            | r == robotABorrar = resto -- Se ha encontrado el robot a borrar
            | otherwise         = r : nuevosRobots resto -- Se sigue buscando en el resto de la lista


-- Añade un nuevo robot a la partida
añadirRobotAlEstado :: Robot -> Estado -> Estado
añadirRobotAlEstado robot estado = estado { allRobots = robot : allRobots estado }


-- Elimina un proyectil sin vida de la lista de proyectiles activos de la partida
eliminarProyectilDelEstado :: Proyectil -> Estado -> Estado
eliminarProyectilDelEstado proyectilABorrar estado = 
    estado { allProyectiles = nuevosProyectiles (allProyectiles estado) }
    where nuevosProyectiles :: [Proyectil] -> [Proyectil]
          nuevosProyectiles [] = []
          nuevosProyectiles (p:resto)
            | p == proyectilABorrar = resto           -- Se ha encontrado el proyectil a borrar
            | otherwise = p : nuevosProyectiles resto -- Se sigue buscando en el resto de la lista


-- Añade un nuevo proyectil a la partida
añadirProyectilAlEstado :: Proyectil -> Estado -> Estado
añadirProyectilAlEstado proyectil estado = estado { allProyectiles = proyectil : allProyectiles estado }

-- Se recuerda que este móduo esta incompleto pues se irá expandiendo con las nuesvas entregas y tareas que se pidan