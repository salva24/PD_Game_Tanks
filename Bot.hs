module Bot where

import Entidades
import Colision
import Memoria
import Geometry

-- Acciones que puede realizar un bot
data BotAction = Mover AccionMovimiento  -- moverse (acelerar, frenar, mantener)
               | RotarBody Angle         -- girar el cañón o el cuerpo
               | RotarCanon Angle        -- no hacer nada este turno
               | Disparar Vector         -- disparar en cierta dirección
               deriving (Eq, Show)

-- Juego simplificado para la toma de decisiones.
-- Contiene al robot actual y la lista de enemigos visibles.
-- Lo que el robot sabe/cree del juego
data GameState = GameState
    { self :: Robot
    , enemigos :: [Robot] -- enemigos que capta el robot no es igual a la partida (que tiene vision general)
    , proyectiles :: [Proyectil]
    } deriving (Eq, Show)


-- Decide que accion tomara el BOT
decidirBot :: GameState -> [BotAction]
decidirBot (GameState r _ _ )
    | getEnergia r > 70 = [Mover Acelera]
    | getEnergia r < 40 = [Mover Desacelera]
    | otherwise = [Mover Mantiene] 

