-- Bot que usa una memoria para tomar decisiones e una estrategia
module Bot where

import Entidades
import Colision
import Memoria
import Geometry

-- Acciones que puede realizar un bot
-- Puede ser true o false para cada accion
-- SI haces una accion contraria a otra (acelerar y frenar) se hacen ambas acciones y el resultado es que queda igual que antes de aplicar la accion
data AccionesRobot = AccionesRobot
    { acelera :: Bool,
      frena :: Bool,
      rota_cagnon_horario :: Bool,
      rota_cagnon_antihorario :: Bool,
      rota_body_horario :: Bool,
      rota_body_antihorario :: Bool,
      dispara :: Bool
    } deriving (Eq, Show)

-------------------------------------------------------------------
-------------------------------------------------------------------
-- Ejemplos de algunas funciones para IA
decidirBot :: Robot -> AccionesRobot
decidirBot robot
    | nombre == "Robot Tonto" = decisionRobotTonto robot
    | nombre == "Robot Acelera" = decisionRobotAcelera robot
    | nombre == "Robot Gira" = decisionRobotGira robot
    | nombre == "Robot Agresivo" = decisionRobotAgresivo robot
    | otherwise = accionesVacias  -- Patrón por defecto: no hace nada
    where
        nombre = getFuncionDecision robot

-- Acciones vacías (no hace nada)
accionesVacias :: AccionesRobot
accionesVacias = AccionesRobot False False False False False False False

-- Bot Tonto: no hace nada
decisionRobotTonto :: Robot -> AccionesRobot
decisionRobotTonto _ = accionesVacias

-- Bot Acelera: solo acelera
decisionRobotAcelera :: Robot -> AccionesRobot
decisionRobotAcelera _ = accionesVacias { acelera = True, rota_cagnon_antihorario = True, dispara = True}

-- Bot Acelera: solo acelera
decisionRobotGira :: Robot -> AccionesRobot
decisionRobotGira _ = accionesVacias { rota_body_horario = True, rota_cagnon_antihorario = True, dispara = True, acelera = True }

-- Bot Acelera: solo acelera
decisionRobotAgresivo :: Robot -> AccionesRobot
decisionRobotAgresivo _ = accionesVacias { acelera= True, dispara = True }




-- en un futuro habrá q hacer bots que usna la memoria y toman decisiones mas inteligentes
{-- 
decidirBot :: GameState -> Robot -> AccionesRobot
decidirBot state robot@(Entidad { posicion = pos
                                , direccion_body = dirBody
                                , datos_especificos = DatosRobot { funcion_decision = "Bot Cazador"} }) =
  accionesBase
      { acelera = not (null enemigos)
      , dispara = not (null enemigos)
      , rota_body_horario = rotarHorario
      , rota_body_antihorario = rotarAntihorario
      }
  where
    -- IDs de los enemigos visibles según la memoria
    idsVisibles = getListIntOr "enemigos_visibles_ids" mem

    -- Obtiene los robots enemigos desde el estado
    todosRobots = allRobots state
    enemigos = mapMaybe (\rid -> find (\r -> id_entidad r == rid) todosRobots) idsVisibles

    -- Selecciona el enemigo más cercano
    enemigoMasCercano =
      if null enemigos
         then Nothing
         else Just $ minimumBy (comparing (distanceBetween pos . posicion)) enemigos

    -- Calcula el ángulo hacia el enemigo usando angleToTarget
    anguloObjetivo = case enemigoMasCercano of
      Just e  -> angleToTarget pos (posicion e)
      Nothing -> dirBody

    -- Diferencia entre el ángulo actual y el objetivo
    deltaAngulo = normalizaAngulo (anguloObjetivo - dirBody)
    rotarHorario = deltaAngulo > 0.1
    rotarAntihorario = deltaAngulo < -0.1

    -- Normaliza el ángulo entre -π y π
    normalizaAngulo a = ((a + pi) `mod'` (2 * pi)) - pi
--}