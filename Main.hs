-- Main file que crea el numero de bots elegidos con los cerebros deseados y con el play que e sel main loop
module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Geometry
import GameState
import Hyperparams
import GameUpdates
import HandleInput
import Render

-- ------------------------------------------------------------
main :: IO ()
main = do
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

  estado0 <- estadoInicialAleatorio imgFondo explosionImgs tankSprites obsSprites

  play ventana white 60 estado0
    dibujaRender
    handleInput
    updateGame
  where ventana = InWindow "Juego PD Grupo 3" (anchoV,altoV) (10,10) -- Posición de la ventana desde la esquina superior izq. de la pantalla
-- -------------------------------------------------------------
loadJuicyPNGSafe :: FilePath -> IO Picture
loadJuicyPNGSafe path = do
  result <- loadJuicyPNG path
  case result of
    Just pic -> return pic
    Nothing  -> do
      putStrLn $ "Error cargando: " ++ path
      return Blank  -- Retorna imagen vacía si falla