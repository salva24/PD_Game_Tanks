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
  | pantalla e == MenuInicio = menuInicioJuego
  | gameOver e               = Pictures [fondo e, mensajeGameOver]
  | otherwise                = Pictures
    [ Scale 1 1 (fondo e),
      barraSuperior (length (allRobots e)) (length (allProyectiles e)) (length (allExplosiones e)) (floor(tiempo e)),
      dibujaProyectiles e,                          -- dibuja todas los proyectiles
      Pictures $ map (dibujaUnRobot e) (allRobots e),   -- Pasar GameState aquí
      dibujaExplosiones e,                          -- dibuja todas las explosiones
      if gamePausado e then mensajePausa else Blank
    ]
  where mensajePausa    = dibujaMensajeNegrita "PAUSADO" yellow 0 0 0.25 0.25
        mensajeGameOver = dibujaMensajeNegrita ("Ganador: " ++ ganadorText e) red 0 0 0.3 0.3

-- obtener id del ganador: si hay exactamente un robot vivo, usa su id_entidad
ganadorText :: GameState -> String
ganadorText gs
  | null robotsVivos = "N/A"
  | otherwise        = show (id_entidad (head robotsVivos))
  where robotsVivos = filter isRobotAlive (allRobots gs)
-- -----------------------------------------------------------------------------
-- PANTALLA DE INICIO DEL JUEGO
menuInicioJuego :: Picture
menuInicioJuego = Pictures [fondoMenu, textoInicio, botonStart]

-- Fondo para el menú de inicio
fondoMenu :: Picture
fondoMenu = Color white $ rectangleSolid (fromIntegral anchoV) (fromIntegral altoV)

-- Texto de bienvenida
textoInicio :: Picture
textoInicio = Pictures
  [ negrita "BIENVENIDO a Hotdog War - GRUPO 3" black (-450) 130 0.3 0.3,
    negrita "Presiona ENTER o Click Izquierdo para Iniciar" black (-450) 70 0.3 0.3
  ]

-- Func. auxiliar para crear texto con efecto de negrita
negrita :: String -> Color -> Float -> Float -> Float -> Float -> Picture
negrita texto colorTexto x y sx sy = Pictures
  [ Translate (x + dx) (y + dy) $ Scale sx sy $ Color colorTexto $ Text texto
  | dx <- [-1,0,1,-1,2], dy <- [-1,0,1,-1,2]
  ]

-- Botón Start visual (solo decorativo)
botonStart :: Picture
botonStart = Pictures
  [ Color black $ rectangleSolid botonStartWidth botonStartHeight,
    Translate (-buttonTextOffsetX) (-15) $ Scale 0.3 0.3 $ Color white $ Text "START"
  ]
  where buttonTextOffsetX = botonStartWidth / 4  -- pequegno ajuste para centrar el texto visualmente
-- -----------------------------------------------------------------------------
-- BARRA SUPERIOR DEL JUEGO (muestra la información del juego)
barraSuperior :: Int -> Int -> Int -> Int -> Picture
barraSuperior nRobots nProy nExp t = Pictures
  [ Translate 0 340 $ Color white $ rectangleSolid 1070 80,
    Translate (-480) 335 $ Scale 0.15 0.15 $ Color black $ Text ("Robots: " ++ show nRobots),
    Translate (-370) 335 $ Scale 0.15 0.15 $ Color black $ Text ("Projectiles: " ++ show nProy),
    Translate (-230) 335 $ Scale 0.15 0.15 $ Color black $ Text ("Explosiones: " ++ show nExp),
    Translate 380 335 $ Scale 0.15 0.15 $ Color black $ Text ("Tiempo: " ++ show t),
    Translate (-480) 315 $ Scale 0.1 0.1 $ Color (greyN 0.3) $ Text "Presiona ESPACIO para pausar el juego"
  ]

-- MENSAJE DEL JUEGO (pausa / gameover)
dibujaMensajeNegrita :: String -> Color -> Float -> Float -> Float -> Float -> Picture
dibujaMensajeNegrita texto colorTexto x y sx sy = Pictures
  [ Color black $ Scale 2 1 $ rectangleSolid 200 200,
    -- Texto con efecto de negrita y centrado aproximado
    Pictures
      [ Translate (dx - anchoAprox/2) (dy - altoAprox/2) $ Scale sx sy $ Color colorTexto $ Text texto
      | dx <- [-1,0,1,-1,2], dy <- [-1,0,1,-1,2]  -- desplazamientos para efector de negrita en la letra
      ]
  ]
  where anchoAprox = fromIntegral (length texto) * 22  -- cada carácter ≈ 10 unidades (tamagno texto)
        altoAprox  = 20                                -- altura aproximada

-- -----------------------------------------------------------------------------
-- DIBUJA ROBOT EN LA POSICION (x,y)
dibujaUnRobot :: GameState -> Robot -> Picture
dibujaUnRobot gs r = Translate x y $ Pictures
  [ Translate 0 50 $ vidaRobot (getEnergia r),                -- barra de vida robot
    Rotate anguloBody$ Scale 0.2 0.2 $ lateralDer (tankSprites gs ),     -- cuerpo del robot, rota según la direccion
    Color blue $ circleSolid 10,                             -- cabeza robot (centrada en el cuerpo)
    -- cagnon robot, rota según el angulo_disparo (DatosRobot)
     Rotate angCanon $ Scale 0.1 0.1 $ cannonSprite (tankSprites gs)
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
-- -----------------------------------------------------------------------------
-- DIBUJA PROYECTILES que lanza el robot/tanque
dibujaProyectiles :: GameState -> Picture
dibujaProyectiles e = Pictures $ map (\p -> dibujaProyectil p e) (allProyectiles e) 

dibujaProyectil :: Proyectil -> GameState -> Picture
dibujaProyectil p gs= Translate x y $ Rotate angulo $ Scale 0.05 0.05 $ proyectil (tankSprites gs)
  where (x,y) = posicion p
        angulo = (-vectorToAngle (direccion p))                -- direccion de movimiento

-- -----------------------------------------------------------------------------
-- ANIMACIÓN DE LA EXPLOSION
-- Imagen Explosion base (estrellas)
estrella :: Int -> Float -> Float -> Float -> Picture
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
dibujaExplosiones e = Pictures $ map (dibujaExplosion (tiempo e) (explosionSprites e)) (allExplosiones e)

-- Dibuja 1 explosion según su tipo (muerte o impacto)
dibujaExplosion :: Float -> [Picture] -> Explosion -> Picture
dibujaExplosion tiempGlobal sprites ex
  | tiempActivo < 0 = Blank
  | tiempActivo > duracion ex = Blank
  | duracion ex == duracionExplosion = dibujaExplosionMuerte ex tiempActivo  -- explosion de muerte (estrella)
  | otherwise = dibujaExplosionImpacto sprites ex tiempActivo                 -- explosion de impacto (sprites)
  where tiempActivo = tiempGlobal - tiempoInicio ex

-- Dibuja explosion de muerte (estrella expandiéndose)
dibujaExplosionMuerte :: Explosion -> Float -> Picture
dibujaExplosionMuerte ex tiempActivo = 
    Translate x y $ Scale escala escala $ explosionBase
    where (x,y) = posExp ex
          escala = 1.0 + 1.0 * sin (tiempActivo * pi / duracionExplosion)

-- Dibuja explosion de impacto (secuencia de sprites)
dibujaExplosionImpacto :: [Picture] -> Explosion -> Float -> Picture
dibujaExplosionImpacto sprites ex tiempActivo
    | null sprites = Blank  -- si no hay sprites, no dibujamos nada
    | otherwise = Translate x y $ Scale 0.6 0.6 $ sprites !! idxSafe
    where (x,y) = posExp ex
          n = length sprites
          idx = floor (tiempActivo / duracionImpacto * fromIntegral n)
          idxSafe = max 0 (min (n-1) idx)