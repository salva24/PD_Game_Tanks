-- Módulo con el Estado de la Partida. Contiene la información de todos los elementos del juego en un momento dado.
module GameState where

import Graphics.Gloss
import Entidades
import Colision
import Geometry
import Memoria
import Bot
import Hyperparams
import Test.QuickCheck

type Tiempo = Float  -- tiempo total hasta el momento actual en segundos

data Explosion = Explosion {
    posExp :: (Float,Float),
    tiempoInicio :: Tiempo,
    duracion :: Float,
    tipoExp :: TipoExp -- "curacion", "Por proyectil"
} deriving (Show,Eq)

data TipoExp = ExpProyectil | ExpMuerte | ExpObstaculoExplosivo | ExpCuracion deriving (Show,Eq)

-- MUESTRA EL ESTADO DE LA PARTIDA
data GameState = GameState {
    idsQueParticiparon :: [Int],                  -- lista de ids de bots que participaron, no recibe modificaciones y es solo como ayuda
    allRobots :: [Robot],               -- Lista con todos los tanques vivos
    allObstaculos :: [Obstaculo],       -- Lista con todos los obstáculos en el mapa
    allProyectiles :: [Proyectil],      -- Lista con todos los proyectiles activos
    allExplosiones :: [Explosion],      -- Lista con todas las explosiones activas
    tiempo :: Tiempo,                   -- Tiempo Global del juego
    gameOver :: Bool,                   -- Indica si el juego ha terminado
    gamePausado :: Bool,                -- Indica si el juego está pausado
    fondo :: Picture,                   -- Imagen de fondo del juego
    latestAvaileableId :: Int,          -- Ultimo numero de Id libre para entidades
    pantalla :: PantallaJuego,          -- Pantalla de incio del juego
    startPressed :: Bool,               -- botón de incio de juego (start presionado)
    explosionSprites :: [Picture],      -- secuencia de la explosion (proyectil impacta robots)
    tankSprites :: TankSprites,
    obsSprites :: ObsSprites,            -- imgs para los obstaculos
    estadisticaPartida :: Estadistica
}
-----------------------------------------------------------------------------------------
----------ESTADISTICApARTIDA-------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


data Estadistica = Estadistica {
  proyectilesImpactadosPorBot :: [(Int, Int)],  -- (id bot, num impactos logrados)
  tiempoVivoPorBot :: [(Int, Float)],            -- (id bot, tiempo vivo en segundos)
  tiempoTotalPartida :: Float,                      -- duración total de la partida
  ganador :: Int                          -- nombre del bot ganador
} deriving (Show, Eq)

-- Estadística inicial vacía
estadisticaInicial :: Estadistica
estadisticaInicial = Estadistica {
  proyectilesImpactadosPorBot = [],
  tiempoVivoPorBot = [],
  tiempoTotalPartida = 0,
  ganador = -1
}

-- Registrar impacto de proyectil disparado por un robot. Se guarda el id con el numero de impactos logrados por ese id
registrarImpacto :: Int -> Estadistica -> Estadistica
registrarImpacto botId stats = stats {
  proyectilesImpactadosPorBot = actualizarImpactos botId (proyectilesImpactadosPorBot stats)
}
  where
    actualizarImpactos :: Int -> [(Int, Int)] -> [(Int, Int)]
    actualizarImpactos bid [] = [(bid, 1)]-- Si no existe, agregar con 1 impacto -- No deberia suceder porque deberia estar a cero en el inicial
    actualizarImpactos bid ((id', count):rest) -- Buscar y actualizar
      | id' == bid = (id', count + 1) : rest 
      | otherwise  = (id', count) : actualizarImpactos bid rest

-- Registrar muerte de un robot
registrarMuerte :: Int -> Tiempo -> Estadistica -> Estadistica
registrarMuerte botId tiempoMuerte stats = stats {
  tiempoVivoPorBot = (botId, tiempoMuerte) : (tiempoVivoPorBot stats)
}

-- Establecer ganador
establecerGanador :: Int -> Tiempo -> Estadistica -> Estadistica
establecerGanador botId tiempoTotal stats = stats {
  ganador = botId,
  tiempoTotalPartida = tiempoTotal,
  tiempoVivoPorBot = [(botId, tiempoTotal)] ++ (tiempoVivoPorBot stats) -- tiempo vivo es el total de la partida
}

-- Establecer ganadores en caso de empate (múltiples bots murieron simultáneamente siendo los últimos)
establecerGanadorCasoEmpate :: [Int] -> Tiempo -> Estadistica -> Estadistica
establecerGanadorCasoEmpate idsFaltantes tiempoTotal stats = stats {
  ganador = -1,  -- -1 indica empate (no hay un único ganador)
  tiempoTotalPartida = tiempoTotal,
  tiempoVivoPorBot = agregarTiemposVivos idsFaltantes tiempoTotal (tiempoVivoPorBot stats)
}
  where
    -- Agregar el tiempo de vida para todos los bots que sobrevivieron hasta el final
    agregarTiemposVivos :: [Int] -> Float -> [(Int, Float)] -> [(Int, Float)]
    agregarTiemposVivos [] _ tiempos = tiempos
    agregarTiemposVivos (botId:rest) tTotal tiempos = 
      agregarTiemposVivos rest tTotal ((botId, tTotal) : tiempos)


-- Registrar múltiples muertes de robots
registrarMuertes :: [Robot] -> Tiempo -> Estadistica -> Estadistica
registrarMuertes [] _ stats = stats
registrarMuertes (robot:resto) tiempoMuerte stats = 
  registrarMuertes resto tiempoMuerte (registrarMuerte (id_entidad robot) tiempoMuerte stats)

-- Registrar múltiples impactos desde una lista de colisiones
registrarImpactos :: [CollisionEvent] -> Estadistica -> Estadistica
registrarImpactos [] stats = stats
registrarImpactos (RobotProjectileCollision _ proyectil : resto) stats =
  registrarImpactos resto (registrarImpacto (getIdLanzador proyectil) stats)
registrarImpactos (_ : resto) stats = 
  registrarImpactos resto stats
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

-- Menú inicial del juego / juego en curso
data PantallaJuego = MenuInicio | Jugando deriving (Eq, Show)

-- Recibe: la imagen del robot (food truck), el canon (vendedor) y el proyectil (hotdog)
data TankSprites = TankSprites {
    lateralDer :: Picture,
    cannonSprite :: Picture,
    proyectil :: Picture
}

-- Data con el sprite para cada obstaculo
data ObsSprites = ObsSprites {
    obsSolido :: Picture,
    obsDoloroso :: Picture,
    obsExplosivo :: Picture,
    obsCurativo :: Picture
}
-- -------------------------------------------------------------
-- Func. para incrementar el Id disponible en el estado
incrementarIdDisponible :: GameState -> GameState
incrementarIdDisponible estado = estado { latestAvaileableId = latestAvaileableId estado + 1 }

-- Func. de reinicio del juego
-- reiniciarJuego :: GameState -> IO GameState
-- reiniciarJuego gs = estadoInicialAleatorio (fondo gs) (explosionSprites gs) (tankSprites gs) (obsSprites gs)
-- -------------------------------------------------------------
-- ESTADO INCIIAL (posiciones aleatorias usando QuickCheck)
estadoInicialAleatorio :: Picture -> [Picture] -> TankSprites -> ObsSprites -> [String] -> IO GameState
estadoInicialAleatorio imgFondo explosionImgs tankImgs obsImgs nombresFuncionesDecision = do
  -- Crear lista de RobotInfo a partir de los nombres de funciones
  let listaRobotsInfo = [(i, nombre, fromIntegral (i * 10)) | (i, nombre) <- zip [1..] nombresFuncionesDecision]
  robotsAleatorios <- generate (generarTodosRobots [] listaRobotsInfo)
  obstaculosAleatorios <- generate (generarNObstaculos numeroObstaculos [] robotsAleatorios)

  let todos_ids_robots = map id_entidad robotsAleatorios
      estadistica_con_robots_con_disparos_a_cero =
        estadisticaInicial { proyectilesImpactadosPorBot = [(id, 0) | id <- todos_ids_robots] }


  return GameState {
    idsQueParticiparon = todos_ids_robots,-- lista de ids de bots que participaron
    allRobots = robotsAleatorios,
    allProyectiles = [],
    allExplosiones = [],
    allObstaculos = obstaculosAleatorios,
    tiempo = 0,
    gameOver = False,
    gamePausado = False,
    fondo = imgFondo,
    latestAvaileableId = 200, -- Debe ser mas alto que el id mas alto de robots y obstaculos
    pantalla = MenuInicio,
    startPressed = False,
    explosionSprites = explosionImgs,
    tankSprites = tankImgs,
    obsSprites = obsImgs,
    estadisticaPartida = estadistica_con_robots_con_disparos_a_cero
  }

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

-- Genera una posición aleatoria válida para un obstáculo  sin colisionar con robots
generarPosicionValidaObs :: [Robot] -> [Obstaculo] -> Gen Position
generarPosicionValidaObs robotsExistentes obstaculosExistentes = do
  pos <- genPosicion  -- Genera una posición aleatoria

  -- Crear un obstáculo curativo temporal en esa posición
  let obstaculoTemporal = crearObstaculoCurativo
        999999              -- ID temporal
        pos
        anchoObstaculo
        altoObstaculo

  -- Verificar colisiones vs robots y vs otros obstáculos
  if  any (\r -> checkCollision (obstacleToRectangle obstaculoTemporal)
                                (robotToRectangle r)) robotsExistentes
   || any (\o -> checkCollision (obstacleToRectangle obstaculoTemporal)
                                (obstacleToRectangle o)) obstaculosExistentes
    then generarPosicionValidaObs robotsExistentes obstaculosExistentes
    else return pos

-- Información de cada robot (ID, Nombre, Ángulo inicial del cagnón)
type RobotInfo = (Int, String, Float)

-- Información de cada obstaculo (ID, Tipo)
type ObstacleInfo = (Int, String)

--Genera aleatoriamente el tipo de obstaculo que se va a generar
genTipoObstaculo :: Gen TipoObstaculo
genTipoObstaculo = elements allTipos


-- Lista de robots a crear
-- listaRobotsInfo :: [RobotInfo]
-- listaRobotsInfo = [
--   (1, "Robot Francotirador", 0),
--   (2, "Robot Cazador", 180),
--   (3, "Robot Cobarde", 0)
--   ]

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

-- Genera un obstaculo con posición aleatoria válida
generarObstaculoAleatorio :: [Obstaculo] -> [Robot] -> Int -> Gen Obstaculo
generarObstaculoAleatorio obstaculosExistentes robotsExistentes idO = do
  pos <- generarPosicionValidaObs robotsExistentes obstaculosExistentes
  tipo <- genTipoObstaculo
  return $ obstaculoSegunTipo tipo idO pos anchoObstaculo altoObstaculo

-- Genera todos los robots de forma recursiva
generarTodosRobots :: [Robot] -> [RobotInfo] -> Gen [Robot]
generarTodosRobots robotsAcumulados [] = return robotsAcumulados
generarTodosRobots robotsAcumulados (info:resto) = do
  nuevoRobot <- generarRobotAleatorio robotsAcumulados info
  generarTodosRobots (nuevoRobot : robotsAcumulados) resto

-- Genera n obstáculos iniciales con tipos aleatorios
generarNObstaculos :: Int -> [Obstaculo] -> [Robot] -> Gen [Obstaculo]
generarNObstaculos 0 obstaculos _ = return obstaculos
generarNObstaculos n obstaculos robots = do
  let idO = length robots + length obstaculos + 1
  tipo <- genTipoObstaculo                        -- tipo aleatorio
  pos <- generarPosicionValidaObs robots obstaculos
  let nuevo = obstaculoSegunTipo tipo idO pos anchoObstaculo altoObstaculo
  generarNObstaculos (n-1) (nuevo:obstaculos) robots
