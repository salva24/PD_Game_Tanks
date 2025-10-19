-- Main file que crea el numero de bots elegidos con los cerebros deseados y con el play que e sel main loop
module Main where

import Graphics.Gloss
import GameState
import Hyperparams
import GameUpdates
import Render
import HandleInput
import Memoria
import Entidades

-- ------------------------------------------------------------
main :: IO ()
main = do
  imgFondo <- loadBMP "fondo_juego.bmp"  -- Gloss no carga PNG por defecto
  let estado0 = estadoInicial imgFondo
  play ventana white 60 estado0
    dibujaRender
    handleInput
    updateGame
  where ventana = InWindow "Juego PD Grupo 3" (anchoV,altoV) (10,10) -- Posición de la ventana desde la esquina superior izquierda de la pantalla


-- -------------------------------------------------------------
estadoInicial :: Picture -> GameState
estadoInicial imgFondo = GameState {
    allRobots = [
                 crearRobot 1 (0,0) 0 (1, 0) anchoRobot altoRobot vidaMaxima 35 150 memEmpty "Robot Acelera" (-coolDownDisparo),-- el menos coolDownDisparo es para que pueda disparar inmediatamente ya que es el tiempo en que s eejecuto el ultimo disparo
                 crearRobot 2 (250,170) 0 (1, 0) anchoRobot altoRobot vidaMaxima 0 150 memEmpty "Robot Agresivo" (-coolDownDisparo),
                 crearRobot 3 (250,-150) 0 (1, 0) anchoRobot altoRobot vidaMaxima 0 150 memEmpty "Robot Gira" (-coolDownDisparo)],
    allProyectiles = [],
    allExplosiones = [],
    tiempo = 0,
    gameOver = False,
    gamePausado = False,
    fondo = imgFondo,
    latestAvaileableId = 10 -- Debe ser mas alto que el id mas alto de robots y se usa para generear nuevos ids para nuevas entidades, que serán nuevos proyectiles
}