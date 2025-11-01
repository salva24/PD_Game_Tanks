module HandleInput where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState
import Hyperparams

-- ACTUALIZA EL GAMESTATE EN FUNCIÓN DEL TECLADO
handleInput :: Event -> GameState -> GameState
-- Inicia el juego preseionando el botón Start (Enter)
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) e
  | pantalla e == MenuInicio && not (startPressed e) = e { pantalla = Jugando, startPressed = True }
  | otherwise                                        = e

-- Start (Clic izquierdo) inicia solo si el clic cae dentro del botón START
handleInput (EventKey (MouseButton LeftButton) Down _ mousePos) e
  | pantalla e == MenuInicio && not (startPressed e) && clickEnStart mousePos = e { pantalla = Jugando, startPressed = True }
  | otherwise                                                                 = e

-- Pausa/Despausa el juego con el espacio
handleInput (EventKey (SpecialKey KeySpace) Down _ _) e = e { gamePausado = not (gamePausado e) }

-- Reinciar el juego cuando este termina
-- handleInput (EventKey (MouseButton LeftButton) Up _ mousePos) e
--   | gameOver e && dentroBoton mousePos = reiniciarJuego e
--   | otherwise                          = e

-- handleInput (EventKey (SpecialKey KeyEnter) Up _ _) e
--   | gameOver e = reiniciarJuego e
--   | otherwise  = e

-- Cualquier otro evento no afecta
handleInput _ e = e

-- Comprueba si una posición del ratón cae dentro del rectángulo del botón START
clickEnStart :: (Float, Float) -> Bool
clickEnStart (mx, my) = mx >= cx - halfW && mx <= cx + halfW && my >= cy - halfH && my <= cy + halfH
  where (cx, cy) = botonStartPos
        halfW    = botonStartWidth / 2
        halfH    = botonStartHeight / 2

dentroBoton :: (Float, Float) -> Bool
dentroBoton (x, y) = x >= -100 && x <= 100 && y >= -125 && y <= -75