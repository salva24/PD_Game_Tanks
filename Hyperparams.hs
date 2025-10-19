-- Archivo que tiene constantes que se usan en todo el proyecto como el dagno de un proyectil

module Hyperparams where

-- ------------------------------------------------------------
-- TAMAgnO DE LA VENTANA
anchoV, altoV :: Int
anchoV = 1000
altoV = 600

-- Size = ((xmin,xmax) , (ymin, ymax))
type Size = ((Float,Float) , (Float,Float))
globalBounds :: Size
globalBounds = ((xmin_global, xmax_global), (ymin_global, ymax_global))
    where xmin_global = (- fromIntegral anchoV / 2) -10
          xmax_global = (fromIntegral anchoV / 2) - 10
          ymin_global = - fromIntegral altoV / 2
          ymax_global = (fromIntegral altoV / 2) - 40 --restamos esto para evitar que la barra blanca salga por encima del limite del juego

-- Velocidad maxima permitida para un tanque
maxAllowedVelocity :: Float
maxAllowedVelocity = 500.0

-- Velocidad rotacion
velocidadRotacionBody,velocidadRotacionCanon :: Float
velocidadRotacionBody = 50.0
velocidadRotacionCanon = 50.0 

-- Cuanto cambia la velocidad al acelerar o desacelerar
incrementoVelocidad :: Float
incrementoVelocidad = 1.0

-- Datos proyectil
velocidadProyectil :: Float
velocidadProyectil = 200.0

proyectil_tamagno :: (Float,Float)
proyectil_tamagno = (20,10)

-- Dagno que hace un proyectil al impactar
dagnoProyectil :: Int
dagnoProyectil = 20

-- Vida inicial de un tanque
vidaMaxima :: Int
vidaMaxima = 100

--cool down entre disparos de un robot
coolDownDisparo :: Float
coolDownDisparo = 0.5 -- en segundos

-- Duracion de una explosion
duracionExplosion :: Float
duracionExplosion = 2.0

-- Radio de una explosion
radioExplosion :: Float
radioExplosion = 50.0

-- Tamano tanque
anchoRobot, altoRobot :: Float
anchoRobot = 100.0
altoRobot = 60.0