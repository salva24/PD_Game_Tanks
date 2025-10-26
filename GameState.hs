-- Módulo con el Estado de la Partida. Contiene la información de todos los elementos del juego en un momento dado.
module GameState where

import Graphics.Gloss
import Colision
import Entidades
import Bot
import Memoria
import Hyperparams


type Tiempo = Float  -- tiempo total hasta el momento actual en segundos

data Explosion = Explosion {
    posExp :: (Float,Float),
    tiempoInicio :: Tiempo,
    duracion :: Float
} deriving (Show,Eq)

-- MUESTRA EL ESTADO DE LA PARTIDA
data GameState = GameState {
    allRobots :: [Robot],           -- Lista con todos los tanques vivos
    allProyectiles :: [Proyectil],  -- Lista con todos los proyectiles activos
    allExplosiones :: [Explosion],  -- Lista con todas las explosiones activas
    tiempo :: Tiempo,               -- Tiempo Global del juego
    gameOver :: Bool,               -- Indica si el juego ha terminado
    gamePausado :: Bool,            -- Indica si el juego está pausado
    fondo :: Picture,               -- Imagen de fondo del juego
    latestAvaileableId :: Int,      -- Ultimo numero de Id libre para entidades
    pantalla :: PantallaJuego,      -- Pantalla de incio del juego
    startPressed :: Bool,           -- botón de incio de juego (start presionado)
    explosionSprites :: [Picture],  -- secuencia de la explosion (proyectil impacta robots)
    tankSprites :: TankSprites      -- Nuevo campo para las imágenes del tanque
}

-- Menú inicial del juego / juego en curso
data PantallaJuego = MenuInicio | Jugando deriving (Eq, Show)

-- Cambia la firma para recibir también los sprites de explosión
data TankSprites = TankSprites {
    lateralDer :: Picture,
    cannonSprite :: Picture,
    proyectil :: Picture
}
-- -------------------------------------------------------------
-- Función para incrementar el Id disponible en el estado
incrementarIdDisponible :: GameState -> GameState
incrementarIdDisponible estado = estado { latestAvaileableId = latestAvaileableId estado + 1 }
-- -------------------------------------------------------------

