-- Es llamado por play del main y dibuja el juego en la situación actual
module Render where

import Graphics.Gloss
import GameState
import Entidades
import Geometry
import Hyperparams

-- DIBUJA ESCENA COMPLETA (la imagen de fondo y tanques/explosiones/proyectiles)
dibujaRender :: GameState -> Picture
dibujaRender e
  | gameOver e = Pictures [fondo e, mensajeGameOver]
  | otherwise  = Pictures
    [ Scale 1 1 (fondo e),
      barraSuperior (length (allRobots e)) (length (allProyectiles e)) (length (allExplosiones e)) (floor(tiempo e)),
      Pictures $ map dibujaUnRobot (allRobots e),   -- dibuja todos los robots
      dibujaProyectiles e,                          -- dibuja todas los proyectiles
      dibujaExplosiones e,                          -- dibuja todas las explosiones
      if gamePausado e then mensajePausa else Blank
    ]
  where mensajePausa    = dibujaMensaje "PAUSADO" yellow (0.25,0.25)
        mensajeGameOver = dibujaMensaje "GAME OVER" red (0.3,0.3)

-- BARRA SUPERIOR DEL JUEGO (muestra la información del juego)
barraSuperior :: Int -> Int -> Int -> Int -> Picture
barraSuperior nRobots nProy nExp t = Pictures
  [ Translate 0 294 $ Color white $ rectangleSolid 1053 60,           -- fondo barra
    Translate (-480) 285 $ Scale 0.15 0.15 $ Color black $ Text ("Robots: " ++ show nRobots),  -- texto dentro de la barra
    Translate (-370) 285 $ Scale 0.15 0.15 $ Color black $ Text ("Projectiles: " ++ show nProy),  -- texto dentro de la barra
    Translate (-230) 285 $ Scale 0.15 0.15 $ Color black $ Text ("Explosiones: " ++ show nExp),  -- texto dentro de la barra
    Translate 400 285 $ Scale 0.15 0.15 $ Color black $ Text ("Tiempo: " ++ show t)  -- texto dentro de la barra
  ]

-- MENSAJE DEL JUEGO (pausa / gameover)
dibujaMensaje :: String -> Color -> (Float,Float) -> Picture
dibujaMensaje texto colorTexto (sx,sy) = Pictures
  [ Translate 0 0 $ Color black $ Scale 2 1 $ circleSolid 90, -- Círculo de fondo
    -- Texto con efecto de negrita y centrado aproximado
    Pictures
      [ Translate (dx - anchoAprox/2) (dy - altoAprox/2) $
        Scale sx sy $ Color colorTexto $ Text texto
      | dx <- [-1,0,1,-1,2], dy <- [-1,0,1,-1,2]  -- desplazamientos para efector de negrita en la letra
      ]
  ]
  where anchoAprox = fromIntegral (length texto) * 22  -- cada carácter ≈ 10 unidades (tamagno texto)
        altoAprox  = 20                                 -- altura aproximada

-- -----------------------------------------------------------------------------
-- DIBUJA ROBOT EN LA POSICION (x,y)
dibujaUnRobot r = Translate x y $ Pictures
  [ Translate 0 50 $ vidaRobot (getEnergia r),                -- barra de vida robot
    Rotate anguloBody $ Color green $ rectangleSolid w h,     -- cuerpo del robot, rota según la direccion
    Color black $ circleSolid 25,                             -- cabeza robot (centrada en el cuerpo)
    -- cagnon robot, rota según el angulo_disparo (DatosRobot)
     Rotate angCanon $ Translate (30) 0 $ Color black $ rectangleSolid 22 15
  ] where (x,y) = posicion r
          (w,h) = (anchoRobot, altoRobot)   -- tamagno rectangulo (ancho,alto del robot)
          anguloBody = vectorToAngle (direccion r) * (-1)       -- direccion en la que gira el robot
          angCanon = -angulo_disparo (datos_especificos r) -- apunta en una direccion distinta al cuerpo

-- Barra de vida del robot
vidaRobot :: Int -> Picture
vidaRobot energiaActual = Pictures 
  [ Color white $ rectangleSolid 100 9,  -- fondo de la barra de vida
    rectangleWire 101 9,                -- contorno barra vida
    Translate desplazamiento 0 $           -- mueve la barra activa a la izquierda
      Color colorVida $ rectangleSolid anchoVida 8  -- vida actual (irá bajando conforme pasen cositas)
  ] where anchoVida = max 0 (fromIntegral energiaActual * 99 / fromIntegral vidaMaxima)
          desplazamiento = (- (99 - anchoVida) / 2)  -- fija el lado izquierdo y recorta desde la derecha
          colorVida | energiaActual > (vidaMaxima `div` 2) = green
                    | energiaActual > (vidaMaxima `div` 4) = yellow
                    | otherwise                            = red

-- DIBUJA PROYECTILES que lanza el robot/tanque
dibujaProyectiles :: GameState -> Picture
dibujaProyectiles e = Pictures $ map dibujaProyectil (allProyectiles e)

dibujaProyectil :: Proyectil -> Picture
dibujaProyectil p = Translate x y $ Rotate angulo $ Color blue $ Pictures
  [ rectangleSolid w h,                          -- cuerpo del proyectil
    Translate (w/2) 0 $ Polygon [ (0,h/2), (w,0), (0,-h/2)]   -- es un triangulo, la punta del proyectil
  ] where (x,y) = posicion p
          (w,h) = (Entidades.ancho p, Entidades.alto p)       -- tamagno del proyectil
          angulo = (-vectorToAngle (direccion p))                -- direccion de movimiento

-- -----------------------------------------------------------------------------
-- ANIMACIÓN DE LA EXPLOSION
-- Imagen Explosion base (estrellas)
estrella n r innerRatio rotDeg = 
  Rotate rotDeg $
  Polygon [ (radio i * cos (angulo i), radio i * sin (angulo i)) | i <- [0 .. (2*n-1)] ]
  where angulo i = fromIntegral i * (pi / fromIntegral n)
        radio i  = if even i then r else r * innerRatio

-- Explosión base: dos estrellas superpuestas
explosionBase :: Picture
explosionBase = Pictures
  [ Color yellow $ estrella 10 radioExplosion 0.5 0,    -- capa exterior amarilla, grande
    Color orange $ estrella 7 (0.75*radioExplosion) 0.45 18,   -- capa media naranja, rotada
    Color red $ estrella 5 (0.5*radioExplosion) 0.4 36        -- capa interior roja, rotadas
  ]

-- Dibujar las explosiones activas según el tiempo global
dibujaExplosiones :: GameState -> Picture
dibujaExplosiones e = Pictures $ map (dibujaExp (tiempo e)) (allExplosiones e)

-- Dibuja 1 explosion según el tiempo global
dibujaExp :: Float -> Explosion -> Picture
dibujaExp tiempGlobal e
  | tiempActivo < 0 = Blank              -- la explosion aun no empieza
  | tiempActivo > (duracion e) = Blank   -- la explosion termino
  | otherwise = Translate x y $ Scale escala escala $ explosionBase
  where (x,y) = posExp e
        tiempActivo = tiempGlobal - tiempoInicio e
        escala = 1.0 + 1.0 * sin (tiempActivo * pi / (duracion e)) -- cambia el tamgnano según el tiempo activo
