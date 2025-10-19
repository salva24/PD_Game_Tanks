module HandleInput where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState

-- Es llamado por play del main y actualiza el GameState en funcion de input de teclado.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) e = e { gamePausado = not (gamePausado e) } -- pausa/despausa con espacio
handleInput _ e = e  -- otros eventos no afectan el estado
-- acciones que hacemos con el teclado como pausar la partida