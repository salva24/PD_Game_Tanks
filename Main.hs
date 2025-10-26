-- Main file que crea el numero de bots elegidos con los cerebros deseados y con el play que e sel main loop
module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Geometry
import GameState
import Hyperparams
import GameUpdates
import Render
import HandleInput
import Memoria
import Entidades
import Colision
import Test.QuickCheck

-- ------------------------------------------------------------
main :: IO ()
main = do
  imgFondo <- loadJuicyPNGSafe "imagenes/fondo_juego2.png"  
  imgPerrito <- loadJuicyPNGSafe "imagenes/perrito.png"
  -- Cargar imágenes del tanque
  -- imgFrontal <- loadJuicyPNGSafe "imagenes/frontal.png"
  -- imgTrasera <- loadJuicyPNGSafe "imagenes/trasera.png"
  -- imgIzquierda <- loadJuicyPNGSafe "imagenes/lateral_izq.png"
  imgDerecha <- loadJuicyPNGSafe "imagenes/lateral_der.png"
  imgCagnon <- loadJuicyPNGSafe "imagenes/canon.png"
  let tankSprites = TankSprites imgDerecha imgCagnon imgPerrito
  
  -- Cargar imágenes de explosión
  -- Agrega aquí los nombres de los archivos de las imágenes de explosión
  let explosionFiles = ["imagenes/explotion/a.png", "imagenes/explotion/aa.png", 
                         "imagenes/explotion/aaa.png", "imagenes/explotion/aaaa.png",
                         "imagenes/explotion/b.png", "imagenes/explotion/bb.png", 
                         "imagenes/explotion/bbb.png", "imagenes/explotion/bbbb.png",
                         "imagenes/explotion/c.png", "imagenes/explotion/cc.png", 
                         "imagenes/explotion/ccc.png", "imagenes/explotion/cccc.png", 
                         "imagenes/explotion/ccccc.png",
                         "imagenes/explotion/d.png", "imagenes/explotion/dd.png", 
                         "imagenes/explotion/ddd.png", "imagenes/explotion/dddd.png",
                         "imagenes/explotion/e.png", "imagenes/explotion/ee.png", 
                         "imagenes/explotion/eee.png", "imagenes/explotion/eeee.png",
                         "imagenes/explotion/fff.png", "imagenes/explotion/g.png", 
                         "imagenes/explotion/h.png", "imagenes/explotion/i.png", 
                         "imagenes/explotion/j.png", "imagenes/explotion/k.png"]
  explosionImgs <- mapM loadJuicyPNGSafe explosionFiles

  estado0 <- estadoInicialAleatorio imgFondo explosionImgs tankSprites

  play ventana white 60 estado0
    dibujaRender
    handleInput
    updateGame
  where ventana = InWindow "Juego PD Grupo 3" (anchoV,altoV) (10,10) -- Posición de la ventana desde la esquina superior izquierda de la pantalla

-- -------------------------------------------------------------
-- Generador personalizado para posiciones dentro de los límites del mapa
genPosicion :: Gen Position
genPosicion = do
  let ((xmin_global, xmax_global), (ymin_global, ymax_global)) = globalBounds
      margen = anchoRobot
      xmin = xmin_global + margen
      xmax = xmax_global - margen
      ymin = ymin_global + margen
      ymax = ymax_global - margen
  x <- choose (xmin, xmax)
  y <- choose (ymin, ymax)
  return (x, y)


-- Genera una posición aleatoria válida para un robot (sin colisiones)
generarPosicionValida :: [Robot] -> Gen Position
generarPosicionValida robotsExistentes = do
  pos <- genPosicion  -- Usa el generador personalizado
  
  -- Crear un robot temporal para verificar colisiones
  let robotTemporal = crearRobot 
        999999  -- ID temporal
        pos 
        0 
        (1, 0) 
        anchoRobot 
        altoRobot 
        vidaMaxima 
        0 
        150 
        memEmpty 
        "Temporal" 
        (-coolDownDisparo)
  
  -- Verificar si la posición colisiona con otros robots
  if hasRobotCollision robotTemporal robotsExistentes
    then generarPosicionValida robotsExistentes  -- Recursión: intenta otra posición
    else return pos


-- Información de cada robot (ID, Nombre, Ángulo inicial del cagnón)
type RobotInfo = (Int, String, Float)

-- Lista de robots a crear
listaRobotsInfo :: [RobotInfo]
listaRobotsInfo = [
  (1, "Robot Cazador", 35),
  (2, "Robot Cobarde", 0),
  (3, "Robot Buscador", 60),
  (4, "Robot Francotirador", 95)
  ]


-- Genera un robot con posición aleatoria válida
generarRobotAleatorio :: [Robot] -> RobotInfo -> Gen Robot
generarRobotAleatorio robotsExistentes (idR, nombre, anguloDisparo) = do
  pos <- generarPosicionValida robotsExistentes
  return $ crearRobot 
    idR 
    pos 
    0 
    (1, 0) 
    anchoRobot 
    altoRobot 
    vidaMaxima 
    anguloDisparo 
    rangoRadar 
    memEmpty 
    nombre 
    (-coolDownDisparo)


-- Genera todos los robots de forma recursiva
generarTodosRobots :: [Robot] -> [RobotInfo] -> Gen [Robot]
generarTodosRobots robotsAcumulados [] = return robotsAcumulados
generarTodosRobots robotsAcumulados (info:resto) = do
  nuevoRobot <- generarRobotAleatorio robotsAcumulados info
  generarTodosRobots (nuevoRobot : robotsAcumulados) resto


-- Estado inicial con posiciones aleatorias usando QuickCheck
estadoInicialAleatorio :: Picture -> [Picture] -> TankSprites -> IO GameState
estadoInicialAleatorio imgFondo explosionImgs tankImgs = do
  -- Generar los robots con posiciones aleatorias
  robotsAleatorios <- generate (generarTodosRobots [] listaRobotsInfo)
  
  return GameState {
    allRobots = robotsAleatorios,
    allProyectiles = [],
    allExplosiones = [],
    tiempo = 0,
    gameOver = False,
    gamePausado = False,
    fondo = imgFondo,
    latestAvaileableId = 20, -- Debe ser mas alto que el id mas alto de robots
    pantalla = MenuInicio,
    startPressed = False,
    explosionSprites = explosionImgs,
    tankSprites = tankImgs
  }


-- -- Función original para referencia (sin usar)
-- estadoInicial :: Picture -> GameState
-- estadoInicial imgFondo = GameState {
--     allRobots = [
--                  crearRobot 1 (0,0) 0 (1, 0) anchoRobot altoRobot vidaMaxima 35 150 memEmpty "Robot Acelera" (-coolDownDisparo),
--                  crearRobot 2 (250,170) 0 (1, 0) anchoRobot altoRobot vidaMaxima 0 150 memEmpty "Robot Agresivo" (-coolDownDisparo),
--                  crearRobot 3 (250,-150) 0 (1, 0) anchoRobot altoRobot vidaMaxima 0 150 memEmpty "Robot Gira" (-coolDownDisparo)],
--     allProyectiles = [],
--     allExplosiones = [],
--     tiempo = 0,
--     gameOver = False,
--     gamePausado = False,
--     fondo = imgFondo,
--     latestAvaileableId = 10
--     pantalla = MenuInicio,
--     startPressed = False
-- }

loadJuicyPNGSafe :: FilePath -> IO Picture
loadJuicyPNGSafe path = do
  result <- loadJuicyPNG path
  case result of
    Just pic -> return pic
    Nothing  -> do
      putStrLn $ "Error cargando: " ++ path
      return Blank  -- Retorna imagen vacía si falla