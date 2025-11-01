-- Archivo que tiene constantes que se usan en todo el proyecto como el dagno de un proyectil
module Hyperparams where

-- ------------------------------------------------------------
-- TAMAGNO DE LA VENTANA
anchoV, altoV :: Int
anchoV = 1000
altoV = 600

-- Size = ((xmin,xmax) , (ymin, ymax))
type Size = ((Float,Float) , (Float,Float))
globalBounds :: Size
globalBounds = ((xmin_global, xmax_global), (ymin_global, ymax_global))
    where xmin_global = (- fromIntegral anchoV / 2) -10
          xmax_global = (fromIntegral anchoV / 2)
          ymin_global = - fromIntegral altoV / 2
          ymax_global = (fromIntegral altoV / 2)  --restamos esto para evitar que la barra blanca salga por encima del limite del juego
-- ------------------------------------------------------------
-- DATOS TANQUE/ROBOT
anchoRobot, altoRobot :: Float
anchoRobot = 100.0
altoRobot = 60.0

-- velocidad max
maxAllowedVelocity :: Float
maxAllowedVelocity = 500.0

-- velocidad rotacion
velocidadRotacionBody,velocidadRotacionCanon :: Float
velocidadRotacionBody = 50.0
velocidadRotacionCanon = 100.0 

-- Cuanto cambia la velocidad al acelerar o desacelerar
incrementoVelocidad :: Float
incrementoVelocidad = 1.0

-- Vida inicial de un tanque
vidaMaxima :: Int
vidaMaxima = 100

-- Radio del radar del robot para detectar enemigos
rangoRadar :: Float
rangoRadar = 300.0
-- ------------------------------------------------------------
-- DATOS POYECTIL
velocidadProyectil :: Float
velocidadProyectil = 200.0

proyectil_tamagno :: (Float,Float)
proyectil_tamagno = (20,10)

-- Dagno que hace un proyectil al impactar
dagnoProyectil :: Int
dagnoProyectil = 15

--cool down entre disparos de un robot
coolDownDisparo :: Float
coolDownDisparo = 3 -- en segundos
-- ------------------------------------------------------------
-- DATOS EXPLOSION
-- Duracion de una explosion
duracionExplosion :: Float
duracionExplosion = 2.0

-- Radio de una explosion
radioExplosion :: Float
radioExplosion = 50.0

-- Impacto de proyectil (visual pequegno, distinto de la explosión por muerte)
duracionImpacto :: Float
duracionImpacto = 0.3

radioImpacto :: Float
radioImpacto = 20.0

-- Duración en la que el tanque "desaparece" tras recibir un impacto
duracionDesaparicion :: Float
duracionDesaparicion = 0.4

-- ------------------------------------------------------------
-- DATOS OBSTACULOS
anchoObstaculo, altoObstaculo :: Float
anchoObstaculo = 70.0
altoObstaculo = 40.0

numeroObstaculos, dagno_Obstaculo :: Int
numeroObstaculos = 15
dagno_Obstaculo = 10

-- ------------------------------------------------------------
-- BOTÓN START (tamagnos y posición relativos)
botonStartWidth, botonStartHeight :: Float
botonStartWidth  = 200.0
botonStartHeight = 80.0

-- BOTÓN DE REINICIO DEL JUEGO
botonWidth, botonHeight :: Float
botonWidth = 200.0
botonHeight = 50.0

-- Centro del botón en la pantalla de inicio (se dibuja en el origen en Render)
botonStartPos :: (Float, Float)
botonStartPos = (0, 0)

