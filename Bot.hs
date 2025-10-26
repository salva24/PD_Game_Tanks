-- Bot que usa una memoria para tomar decisiones e una estrategia
module Bot where

import Entidades
import Colision
import Memoria
import Geometry
import Hyperparams
import Data.Fixed (mod')

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
decidirBot :: [Robot] -> Robot -> AccionesRobot
decidirBot robots_visibles robot
    | nombre == "Robot Tonto" = decisionRobotTonto robots_visibles robot
    | nombre == "Robot Acelera" = decisionRobotAcelera robots_visibles robot
    | nombre == "Robot Gira" = decisionRobotGira robots_visibles robot
    | nombre == "Robot Cazador" = decidirBotCazador robots_visibles robot
    | nombre == "Robot Cobarde" = decidirBotCobarde robots_visibles robot
    | nombre == "Robot Buscador" = decidirBotBuscador robots_visibles robot
    | nombre == "Robot Francotirador" = decidirBotFrancotirador robots_visibles robot
    | otherwise = accionesVacias  -- Patrón por defecto: no hace nada
    where
        nombre = getFuncionDecision robot
-- Acciones vacías (no hace nada)
accionesVacias :: AccionesRobot
accionesVacias = AccionesRobot False False False False False False False

-- Predice la posicion que va a tener el enemigo para disparar correctamente
predecirPosicionEnemigo :: Point -> Robot -> Point
predecirPosicionEnemigo miPos enemigo = posFutura
  where
    dirEnemigo = direccion enemigo
    posEnemigo = posicion enemigo
    velEnemigo = modulo_velocidad enemigo
    -- Calcular tiempo de vuelo aproximado del proyectil
    distancia = distanceBetween miPos posEnemigo
    tiempoVuelo = distancia / velocidadProyectil  -- Toma la velocidad del proyectil de los hiperparametros
    -- Predecir posición futura
    posFutura = ( fst posEnemigo + fst dirEnemigo * velEnemigo * tiempoVuelo
                , snd posEnemigo + snd dirEnemigo * velEnemigo * tiempoVuelo )
                

-- Bot Tonto: no hace nada
decisionRobotTonto :: [Robot] -> Robot -> AccionesRobot
decisionRobotTonto _ _ = accionesVacias

-- Bot Acelera: solo acelera
decisionRobotAcelera :: [Robot] -> Robot -> AccionesRobot
decisionRobotAcelera _ _ = accionesVacias { acelera = True, rota_cagnon_antihorario = True, dispara = True}

-- Bot Acelera: solo acelera
decisionRobotGira :: [Robot] -> Robot -> AccionesRobot
decisionRobotGira _ _ = accionesVacias { rota_body_horario = True, rota_cagnon_antihorario = True, acelera = True }



-- Persigue a alos enemigos y los mata
decidirBotCazador :: [Robot] -> Robot -> AccionesRobot
decidirBotCazador [] robot = -- Si no hay robots visibles da vueltas disparando en circulos y acelerando para buscar un enemigo
  accionesVacias { acelera = True, rota_body_horario = True, rota_cagnon_horario = True, dispara = True }
decidirBotCazador robots_visibles robot =
  accionesVacias
      { acelera = True
      , dispara = True
      , rota_body_horario = rotarBodyHorario
      , rota_body_antihorario = rotarBodyAntihorario
      , rota_cagnon_horario = rotarCanonHorario
      , rota_cagnon_antihorario = rotarCanonAntihorario
      }
  where
    pos = posicion robot
    dirVector = direccion robot
    anguloCanon = getAnguloDisparo robot
    -- Selecciona el enemigo más cercano si la lista tiene un solo elemento  devuelve el primer elemento sin provocar errores
    enemigoMasCercano = foldl1 (\r1 r2 -> if distanceBetween pos (posicion r1) < distanceBetween pos (posicion r2) then r1 else r2) robots_visibles

    -- Calcula el ángulo hacia el enemigo (usando posición predicha)
    anguloObjetivo = angleToTarget pos (predecirPosicionEnemigo pos enemigoMasCercano)
      
    -- Convierte el vector de dirección del cuerpo a ángulo
    anguloActualBody = vectorToAngle dirVector
    
    -- Ángulo actual del cagnón (ya viene en grados)
    anguloActualCanon = anguloCanon

    -- Diferencias de ángulo para cuerpo y cagnón
    -- decidir la dirección de rotación más corta a su objetivo
    deltaAnguloBody = calculaDeltaAngulo anguloObjetivo anguloActualBody
    deltaAnguloCanon = calculaDeltaAngulo anguloObjetivo anguloActualCanon

    -- Lógica de rotación del CUERPO (hacia donde acelera)
    rotarBodyHorario = deltaAnguloBody < 0
    rotarBodyAntihorario = deltaAnguloBody > 0

    -- Lógica de rotación del CAgnÓN (hacia donde dispara)
    rotarCanonHorario = deltaAnguloCanon < 0
    rotarCanonAntihorario = deltaAnguloCanon > 0


--Huye mientras dispara
decidirBotCobarde :: [Robot] -> Robot -> AccionesRobot
decidirBotCobarde [] robot = -- Si no hay robots visibles se queda quieto disparando en cierculos
  accionesVacias { frena = True, rota_cagnon_antihorario = True, dispara = True }
decidirBotCobarde robots_visibles robot =
  accionesVacias
      { acelera = True
      , dispara = True
      , rota_body_horario = rotarBodyHorario
      , rota_body_antihorario = rotarBodyAntihorario
      , rota_cagnon_horario = rotarCanonHorario
      , rota_cagnon_antihorario = rotarCanonAntihorario
      }
  where
    pos = posicion robot
    dirVector = direccion robot
    anguloCanon = getAnguloDisparo robot
    -- Selecciona el enemigo más cercano si la lista tiene un solo elemento  devuelve el primer elemento sin provocar errores
    enemigoMasCercano = foldl1 (\r1 r2 -> if distanceBetween pos (posicion r1) < distanceBetween pos (posicion r2) then r1 else r2) robots_visibles

    -- Calcula el ángulo hacia el enemigo (usando posición predicha)
    anguloObjetivoDisparo = angleToTarget pos (predecirPosicionEnemigo pos enemigoMasCercano)
      
    -- Calcula el ángulo opuesto para huir
    anguloObjetivoBody = (anguloObjetivoDisparo + 180) `mod'` 360

    -- Convierte el vector de dirección del cuerpo a ángulo
    anguloActualBody = vectorToAngle dirVector
    
    -- Ángulo actual del cagnón (ya viene en grados)
    anguloActualCanon = anguloCanon

    -- Diferencias de ángulo para cuerpo y cagnón
    -- decidir la dirección de rotación más corta a su objetivo
    deltaAnguloBody = calculaDeltaAngulo anguloObjetivoBody anguloActualBody
    deltaAnguloCanon = calculaDeltaAngulo anguloObjetivoDisparo anguloActualCanon

    -- Lógica de rotación del CUERPO (hacia donde acelera)
    rotarBodyHorario = deltaAnguloBody < 0
    rotarBodyAntihorario = deltaAnguloBody > 0

    -- Lógica de rotación del CAgnÓN (hacia donde dispara)
    rotarCanonHorario = deltaAnguloCanon < 0
    rotarCanonAntihorario = deltaAnguloCanon > 0

--Se da vueltas hasta encontrar enemigos y luego se para a matarlo
decidirBotBuscador :: [Robot] -> Robot -> AccionesRobot
decidirBotBuscador [] robot = -- Si no hay robots visibles se queda quieto disparando en cierculos
  accionesVacias { acelera = True, rota_cagnon_antihorario = True, dispara = True, rota_body_antihorario = True }
decidirBotBuscador robots_visibles robot =
  accionesVacias
      { frena = True
      , dispara = True
      , rota_cagnon_horario = rotarCanonHorario
      , rota_cagnon_antihorario = rotarCanonAntihorario
      }
  where
    pos = posicion robot
    dirVector = direccion robot
    anguloCanon = getAnguloDisparo robot
    -- Selecciona el enemigo más cercano si la lista tiene un solo elemento  devuelve el primer elemento sin provocar errores
    enemigoMasCercano = foldl1 (\r1 r2 -> if distanceBetween pos (posicion r1) < distanceBetween pos (posicion r2) then r1 else r2) robots_visibles

    -- Calcula el ángulo hacia el enemigo (usando posición predicha)
    anguloObjetivoDisparo = angleToTarget pos (predecirPosicionEnemigo pos enemigoMasCercano)
      
    -- Ángulo actual del cagnón (ya viene en grados)
    anguloActualCanon = anguloCanon

    -- Diferencias de ángulo para cuerpo y cagnón
    -- decidir la dirección de rotación más corta a su objetivo
    deltaAnguloCanon = calculaDeltaAngulo anguloObjetivoDisparo anguloActualCanon

    -- Lógica de rotación del CAgnÓN (hacia donde dispara)
    rotarCanonHorario = deltaAnguloCanon < 0
    rotarCanonAntihorario = deltaAnguloCanon > 0




--Se va a una esquina del mapa y desde alli dispara a los enemigos
decidirBotFrancotirador :: [Robot] -> Robot -> AccionesRobot
decidirBotFrancotirador [] robot = -- Si no hay robots visibles se va a la esquina y dispara en circulos
  accionesVacias 
      { acelera = True, 
       dispara = True
      , rota_body_horario = rotarBodyHorario
      , rota_body_antihorario = rotarBodyAntihorario
      , rota_cagnon_horario = True
      }
  where
    pos = posicion robot
    dirVector = direccion robot
    -- Convierte el vector de dirección del cuerpo a ángulo
    anguloActualBody = vectorToAngle dirVector
    
    xmin_global = fst (fst globalBounds)
    ymin_global = fst (snd globalBounds)
    -- Calcula el ángulo hacia la esquina del mapa
    anguloObjetivo = angleToTarget pos (xmin_global+20,ymin_global+20)  -- Esquina del mapa
      
    -- Diferencias de ángulo para cuerpo
    deltaAnguloBody = calculaDeltaAngulo anguloObjetivo anguloActualBody

    -- Lógica de rotación del CUERPO (hacia donde acelera)
    rotarBodyHorario = deltaAnguloBody < 0
    rotarBodyAntihorario = deltaAnguloBody > 0

decidirBotFrancotirador robots_visibles robot = --hay enemigos a la vista
  accionesVacias
      { acelera = True
      , dispara = True
      , rota_body_horario = rotarBodyHorario
      , rota_body_antihorario = rotarBodyAntihorario
      , rota_cagnon_horario = rotarCanonHorario
      , rota_cagnon_antihorario = rotarCanonAntihorario
      }
  where
    pos = posicion robot
    dirVector = direccion robot
    anguloCanon = getAnguloDisparo robot
    -- Convierte el vector de dirección del cuerpo a ángulo
    anguloActualBody = vectorToAngle dirVector

    -- Selecciona el enemigo más cercano si la lista tiene un solo elemento  devuelve el primer elemento sin provocar errores
    enemigoMasCercano = foldl1 (\r1 r2 -> if distanceBetween pos (posicion r1) < distanceBetween pos (posicion r2) then r1 else r2) robots_visibles

    -- Calcula el ángulo hacia el enemigo (usando posición predicha)
    anguloObjetivoDisparo = angleToTarget pos (predecirPosicionEnemigo pos enemigoMasCercano)
      
    -- Diferencias de ángulo para cuerpo y cagnón
    -- decidir la dirección de rotación más corta a su objetivo
    deltaAnguloCanon = calculaDeltaAngulo anguloObjetivoDisparo anguloCanon

    -- Lógica de rotación del CAgnÓN (hacia donde dispara)
    rotarCanonHorario = deltaAnguloCanon < 0
    rotarCanonAntihorario = deltaAnguloCanon > 0

    xmin_global = fst (fst globalBounds)
    ymin_global = fst (snd globalBounds)
    -- Calcula el ángulo hacia la esquina del mapa
    anguloObjetivo = angleToTarget pos (xmin_global+30,ymin_global+40)  -- Esquina del mapa
      
    -- Diferencias de ángulo para cuerpo
    deltaAnguloBody = calculaDeltaAngulo anguloObjetivo anguloActualBody

    -- Lógica de rotación del CUERPO (hacia dónde acelera)
    rotarBodyHorario = deltaAnguloBody < 0
    rotarBodyAntihorario = deltaAnguloBody > 0