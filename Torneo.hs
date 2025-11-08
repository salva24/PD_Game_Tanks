module Torneo where

import Graphics.Gloss.Juicy
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import Text.Printf
import GameState
import GameUpdates
import HandleInput
import Render
import Hyperparams
import Entidades
import Data.List
import Debug.Trace (trace)

data ConfigTorneo = ConfigTorneo {
  listaBots :: [String],
  tamanoMapa :: (Int, Int),
  duracionMaxima :: Float,
  numeroTorneos :: Int
  } deriving (Show)

data EstadoTorneos = EstadoTorneos {
  estadoJuegoActual :: GameState,
  configuracion :: ConfigTorneo,
  torneoActual :: Int,
  estadisticasAcumuladas :: [Estadistica],
  esperandoNuevoTorneo :: Bool,
  tiempoEspera :: Float,
  todosLosRecursos :: RecursosGraficos,
  haTerminadoTodo :: Bool
}

data RecursosGraficos = RecursosGraficos {
  imgFondoRecursos :: Picture,
  tankSpritesRecursos :: TankSprites,
  obsSpritesRecursos :: ObsSprites,
  explosionSpritesRecursos :: [Picture]
}

runTorneos :: IO ()
runTorneos = do
  cfg <- leerConfig "config.txt"
  writeFile "estadisticas.txt" encabezadoEstadisticas
  
  recursos <- cargarRecursosGraficos
  estadoInicial <- crearEstadoInicialTorneo cfg recursos 1
  
  let estadoTorneosInicial = EstadoTorneos {
    estadoJuegoActual = estadoInicial,
    configuracion = cfg,
    torneoActual = 1,
    estadisticasAcumuladas = [],
    esperandoNuevoTorneo = False,
    tiempoEspera = 0,
    todosLosRecursos = recursos,
    haTerminadoTodo = False
  }
  
  let (anchoMapa, altoMapa) = tamanoMapa cfg
      ventana = InWindow "Sistema de Torneos - Food Truck Wars" (anchoMapa, altoMapa) (10, 10)
  play ventana white 60 estadoTorneosInicial
    dibujarTorneos
    manejarInputTorneos
    actualizarTorneos

cargarRecursosGraficos :: IO RecursosGraficos
cargarRecursosGraficos = do
  -- Carga imagenes de fondo
  imgFondo <- loadJuicyPNGSafe "imagenes/fondo_juego2.png"

  -- Carga imagenes de tanques
  imgPerrito <- loadJuicyPNGSafe "imagenes/perrito.png"
  imgDerecha <- loadJuicyPNGSafe "imagenes/lateral_der.png"
  imgCagnon <- loadJuicyPNGSafe "imagenes/canon.png"
  let tankSprites = TankSprites imgDerecha imgCagnon imgPerrito
  
  -- Carga imagenes de obstaculos
  imgObsSolido <- loadJuicyPNGSafe "imagenes/obstaculos/cliente_enfadado.png"
  imgObsDoloroso <- loadJuicyPNGSafe "imagenes/obstaculos/ketchup.png"
  imgObsExplosivo <- loadJuicyPNGSafe "imagenes/obstaculos/mostaza.png"
  imgObsCurativo <- loadJuicyPNGSafe "imagenes/obstaculos/mayonesa.png"
  let obsSprites = ObsSprites imgObsSolido imgObsDoloroso imgObsExplosivo imgObsCurativo

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

  return RecursosGraficos {
      imgFondoRecursos = imgFondo,
      tankSpritesRecursos = tankSprites,
      obsSpritesRecursos = obsSprites,
      explosionSpritesRecursos = explosionImgs
}

loadJuicyPNGSafe :: FilePath -> IO Picture
loadJuicyPNGSafe path = do
  result <- loadJuicyPNG path
  case result of
    Just pic -> return pic
    Nothing  -> do
      putStrLn $ "Error cargando: " ++ path
      return Blank  -- Retorna imagen vacía si falla

-- -----------------------------------------------------------------------------
-- FUNCIONES DE RENDER
dibujarTorneos :: EstadoTorneos -> Picture
dibujarTorneos et 
  | haTerminadoTodo et = pantallaFin et
  | esperandoNuevoTorneo et = Pictures [
      dibujaRender (estadoJuegoActual et),
      pantallaTransicion et
    ]
  | otherwise = Pictures [
      dibujaRender (estadoJuegoActual et),
      infoTorneoEnBarra et
    ]

infoTorneoEnBarra :: EstadoTorneos -> Picture
infoTorneoEnBarra et = 
  let texto = "Torneo: " ++ show (torneoActual et) ++ "/" ++ show (numeroTorneos (configuracion et))
  in negrita texto blue 0 335 0.2 0.2

pantallaTransicion :: EstadoTorneos -> Picture
pantallaTransicion et = Pictures [
    Color (makeColorI 0 0 0 180) $ rectangleSolid (fromIntegral anchoV) (fromIntegral altoV),
    Translate (-320) 100 $ Scale 0.5 0.5 $ Color white $ Text "TORNEO FINALIZADO",
    Translate (-320) 0 $ Scale 0.3 0.3 $ Color yellow $ Text mensaje,
    Translate (-320) (-100) $ Scale 0.2 0.2 $ Color white $ Text "Preparando siguiente torneo..."
  ]
  where est = estadisticaPartida (estadoJuegoActual et)
        ganadorId = ganador est
        mensaje = if ganadorId == -1 then "Empate" else "Ganador: Robot ID " ++ show ganadorId

pantallaFin :: EstadoTorneos -> Picture
pantallaFin et = Pictures [
    Color (makeColorI 0 0 0 180) $ rectangleSolid (fromIntegral anchoV) (fromIntegral altoV),
    negrita "FIN DE TORNEOS" white (-320) 100 0.5 0.5,
    negrita "Mira el archivo estadisticas.txt" yellow (-320) (-30) 0.3 0.3
  ]
-- -----------------------------------------------------------------------------
-- FUNCIONES DE ACTUALIZACIÓN
actualizarTorneos :: Float -> EstadoTorneos -> EstadoTorneos
actualizarTorneos dt et
  | esperandoNuevoTorneo et = --pasano a una nueva partida
      if tiempoEspera et >= 3.0
        then iniciarSiguienteTorneo et
      else et { tiempoEspera = tiempoEspera et + dt }
  | haTerminadoTodo et = et--Todo el torneo termino y no hace nada
  | gameOver (estadoJuegoActual et) = finalizarTorneoActual et --la partida terminó
  | tiempo (estadoJuegoActual et) >= duracionMaxima (configuracion et) = finalizarTorneoPorTiempo et --tiempo máximo alcanzado
  | otherwise = et { estadoJuegoActual = updateGame dt (estadoJuegoActual et) }

manejarInputTorneos :: Event -> EstadoTorneos -> EstadoTorneos
manejarInputTorneos event et
  | esperandoNuevoTorneo et = et  -- No procesar input durante transición
  | otherwise = et { estadoJuegoActual = handleInput event (estadoJuegoActual et) }

finalizarTorneoActual :: EstadoTorneos -> EstadoTorneos
finalizarTorneoActual et = 
  let est = estadisticaPartida (estadoJuegoActual et)
      cfg = configuracion et
      numActual = torneoActual et
      !_ = unsafePerformIOGuardar numActual est `seq` ()-- la exclamacion es para forzar la evalucion y evitar el lazy evaluation
  in if numActual >= numeroTorneos cfg
     then finalizarTodosLosTorneos et
     else et {
       esperandoNuevoTorneo = True,
       tiempoEspera = 0,
       estadisticasAcumuladas = estadisticasAcumuladas et ++ [est]
     }

-- se ha pasadod e tiempo
finalizarTorneoPorTiempo :: EstadoTorneos -> EstadoTorneos
finalizarTorneoPorTiempo et = 
  let gs = estadoJuegoActual et
      est = estadisticaPartida gs
      robotsVivos = filter isRobotAlive (allRobots gs)
      idGanador = -1 -- ha habido empate
      tiempo_partida = tiempo gs
      estFinal = establecerGanadorCasoEmpate 
               (idsFaltantes (tiempoVivoPorBot est) (idsQueParticiparon gs)) 
               tiempo_partida 
               est
      final_gs= gs { estadisticaPartida = estFinal, gameOver = True }
  in finalizarTorneoActual (et { estadoJuegoActual = final_gs })

iniciarSiguienteTorneo :: EstadoTorneos -> EstadoTorneos
iniciarSiguienteTorneo et = 
  let cfg = configuracion et
      numSiguiente = torneoActual et + 1
      recursos = todosLosRecursos et
      nuevoEstado = unsafePerformIOCrearEstado cfg recursos numSiguiente
  in et {
    estadoJuegoActual = nuevoEstado,
    torneoActual = numSiguiente,
    esperandoNuevoTorneo = False,
    tiempoEspera = 0
  }

finalizarTodosLosTorneos :: EstadoTorneos -> EstadoTorneos
finalizarTodosLosTorneos et = 
  let todasEsts = estadisticasAcumuladas et ++ [estadisticaPartida (estadoJuegoActual et)]
      !_ = unsafePerformIOGuardarAgregadas todasEsts -- la exclamacion es para forzar la evalucion y evitar el lazy evaluation
  in et {haTerminadoTodo= True}
-- -----------------------------------------------------------------------------
-- CREACIÓN DE ESTADO INICIAL
unsafePerformIOCrearEstado :: ConfigTorneo -> RecursosGraficos -> Int -> GameState
unsafePerformIOCrearEstado cfg recursos numTorneo = unsafePerformIO $ 
  crearEstadoInicialTorneo cfg recursos numTorneo

crearEstadoInicialTorneo :: ConfigTorneo -> RecursosGraficos -> Int -> IO GameState
crearEstadoInicialTorneo cfg recursos _ = do
  estado0 <- estadoInicialAleatorio 
    (imgFondoRecursos recursos)
    (explosionSpritesRecursos recursos)
    (tankSpritesRecursos recursos)
    (obsSpritesRecursos recursos)
    (listaBots cfg)
  
  return estado0 { 
    GameState.pantalla = Jugando, 
    startPressed = True,
    estadisticaPartida = estadisticaInicialConBots (allRobots estado0)
  }

estadisticaInicialConBots :: [Robot] -> Estadistica
estadisticaInicialConBots robots = Estadistica {
  proyectilesImpactadosPorBot = [],
  tiempoVivoPorBot = [],
  tiempoTotalPartida = 0,
  ganador = -1
}

-- FUNCIONES AUXILIARES CON IO
unsafePerformIOGuardar :: Int -> Estadistica -> ()
unsafePerformIOGuardar indice est = unsafePerformIO $ do
  guardarEstadisticaTorneo indice est
  return ()

unsafePerformIOGuardarAgregadas :: [Estadistica] -> ()
unsafePerformIOGuardarAgregadas ests = unsafePerformIO $ do
  guardarEstadisticasAgregadas ests
  return ()

guardarEstadisticasAgregadas :: [Estadistica] -> IO ()
guardarEstadisticasAgregadas ests = do
  appendFile "estadisticas.txt" "\n================= RESUMEN AGREGADO =================\n"
  -- duración media
  let dur = map tiempoTotalPartida ests
      avg x = sum x / fromIntegral (length x)
  appendFile "estadisticas.txt" $ "Duracion media: " ++ printf "%.2f" (avg dur) ++ " s\n"
  -- ganador más frecuente (ignorando empates)
  let ganadores = filter (/= (-1)) (map ganador ests)
  if null ganadores
    then appendFile "estadisticas.txt" "No hubo ningun ganador claro.\n"
    else do
      let tabla = map (\xs -> (head xs, length xs)) $ group $ sort ganadores
          (top, veces) = maximumBy (\a b -> compare (snd a) (snd b)) tabla
      appendFile "estadisticas.txt" $ "Bot mas ganador: Bot#" ++ show top ++ " (" ++ show veces ++ " victorias)\n"
  appendFile "estadisticas.txt" "====================================================\n"

-- Encabezado del archivo de estadísticas
encabezadoEstadisticas :: String
encabezadoEstadisticas = unlines [
    "========================================",
    "    ESTADISTICAS DE TORNEOS DE BOTS",
    "========================================",
    "",
    "Numero Torneo | Ganador | Tiempo Total | Impactos por Bot | % Tiempo Vivo por Bot",
    "-------------------------------------------------------------------------------------",
    ""
  ]

-- Guardar estadística de un torneo en el archivo
guardarEstadisticaTorneo :: Int -> Estadistica -> IO ()
guardarEstadisticaTorneo indice est = do
  let lineaTorneo = formatearEstadisticaTorneo indice est
  appendFile "estadisticas.txt" lineaTorneo

formatearEstadisticaTorneo :: Int -> Estadistica -> String
formatearEstadisticaTorneo indice est = unlines [
  "TORNEO " ++ show indice,
  "Ganador: " ++ (if ganador est == -1 then "Empate" else "Bot #" ++ show (ganador est)),
  "Duracion: " ++ printf "%.2f" (tiempoTotalPartida est) ++ " segundos",
  "Proyectiles impactados: " ++ formatearImpactos (proyectilesImpactadosPorBot est),
  "Tiempo vivo: " ++ formatearTiemposVivos (tiempoVivoPorBot est),
  "Porcentaje tiempo vivo: " ++ formatearPorcentajes (calcularPorcentajeVida est),
  ""
  ]

-- Calcular porcentaje de tiempo vivo para cada bot
calcularPorcentajeVida :: Estadistica -> [(Int, Float)]
calcularPorcentajeVida stats
  | tiempoTotalPartida stats == 0 = []
  | otherwise = map calcularPorcentaje (tiempoVivoPorBot stats)
  where
    calcularPorcentaje (botId, tiempoVivo) = 
      (botId, (tiempoVivo / tiempoTotalPartida stats) * 100)

-- Formatear lista de impactos
formatearImpactos :: [(Int, Int)] -> String
formatearImpactos [] = "Sin impactos"
formatearImpactos impactos = intercalate ", " $ map formatear impactos
  where formatear (botId, count) = "Bot#" ++ show botId ++ ":" ++ show count

-- Formatear lista de tiempos vivos
formatearTiemposVivos :: [(Int, Float)] -> String
formatearTiemposVivos [] = "Sin datos"
formatearTiemposVivos tiempos = intercalate ", " $ map formatear tiempos
  where formatear (botId, t) = "Bot#" ++ show botId ++ ":" ++ printf "%.2f" t ++ "s"

-- Formatear lista de porcentajes
formatearPorcentajes :: [(Int, Float)] -> String
formatearPorcentajes [] = "Sin datos"
formatearPorcentajes porcentajes = intercalate ", " $ map formatear porcentajes
  where formatear (botId, p) = "Bot#" ++ show botId ++ ":" ++ printf "%.1f" p ++ "%"


-- Lectura y parseo del archivo de configuración
---------------------------------------------------------------------------------------------------
leerConfig :: FilePath -> IO ConfigTorneo
leerConfig path = do
  contenido <- readFile path
  let lineas = lines contenido
      lineasLimpias = filter (not . esComentarioOVacia) lineas
  parseConfig lineasLimpias

-- Verifica si una línea es comentario o está vacía
esComentarioOVacia :: String -> Bool
esComentarioOVacia s = null (dropWhile (== ' ') s) || "#" `isInfixOf` s

-- Parsea las líneas de configuración
parseConfig :: [String] -> IO ConfigTorneo
parseConfig lineas
  | length lineas < 4 = error "Archivo de configuración incompleto"
  | otherwise = do
      let botsLine = lineas !! 0
          areaLine = lineas !! 1
          durLine = lineas !! 2
          numLine = lineas !! 3
      
      botsList <- parseBots botsLine
      areaSize <- parseArea areaLine
      duracion <- parseDuracion durLine
      numTorns <- parseNumTorneos numLine
      
      return $ ConfigTorneo botsList areaSize duracion numTorns

-- Parsear lista de bots
parseBots :: String -> IO [String]
parseBots linea = do
  let partes = dropWhile (/= '[') linea
  if null partes
    then error "Formato de bots inválido"
    else do
      let contenido = takeWhile (/= ']') (tail partes)
      let botNames = map (filter (\c -> c /= '"' && c /= ' ')) (splitOn ',' contenido)
      return $ filter (not . null) botNames

-- Parsear tamaño del área: area_size = (800, 600)
parseArea :: String -> IO (Int, Int)
parseArea linea = do
  let partes = dropWhile (/= '(') linea
  if null partes
    then error "Formato de area_size inválido"
    else do
      let contenido = takeWhile (/= ')') (tail partes)
      let [ancho, alto] = map (read . filter (/= ' ')) (splitOn ',' contenido)
      return (ancho, alto)

-- Parsear duración máxima: max_duration = 300
parseDuracion :: String -> IO Float
parseDuracion linea = do
  let valor = dropWhile (/= '=') linea
  if null valor
    then error "Formato de max_duration inválido"
    else return $ read $ filter (/= ' ') $ tail valor

-- Parsear número de torneos: num_tournaments = 5
parseNumTorneos :: String -> IO Int
parseNumTorneos linea = do
  let valor = dropWhile (/= '=') linea
  if null valor
    then error "Formato de num_tournaments inválido"
    else return $ read $ filter (/= ' ') $ tail valor

-- Función auxiliar para dividir strings
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim xs = 
  let (before, remainder) = break (== delim) xs
  in before : case remainder of
                [] -> []
                _:rest -> splitOn delim rest
-- -----------------------------------------------------------------------------