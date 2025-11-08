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
-- Ejemplos de algunas funciones para IA. Devuelve una memoria modificada y las acciones a realizar
decidirBot :: [Robot]-> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decidirBot robots_visibles obstaculos robot
    | nombre == "Tonto" = decisionRobotTonto robots_visibles obstaculos robot
    | nombre == "Acelera" = decisionRobotAcelera robots_visibles obstaculos robot
    | nombre == "Gira" = decisionRobotGira robots_visibles obstaculos robot
    | nombre == "Cazador" = decidirBotCazador robots_visibles obstaculos robot
    | nombre == "Cobarde" = decidirBotCobarde robots_visibles obstaculos robot
    | nombre == "Buscador" = decidirBotBuscador robots_visibles obstaculos robot
    | nombre == "Francotirador" = decidirBotFrancotirador robots_visibles obstaculos robot
    | otherwise = (memEmpty,accionesVacias)  -- Patrón por defecto: no hace nada
    where nombre = getFuncionDecision robot

-- Acciones vacías (no hace nada)
accionesVacias :: AccionesRobot
accionesVacias = AccionesRobot False False False False False False False

-- Predice la posicion que va a tener el enemigo para disparar correctamente
predecirPosicionEnemigo :: Point -> Robot -> Point
predecirPosicionEnemigo miPos enemigo = posFutura
  where dirEnemigo = direccion enemigo
        posEnemigo = posicion enemigo
        velEnemigo = modulo_velocidad enemigo
        -- Calcular tiempo de vuelo aproximado del proyectil
        distancia = distanceBetween miPos posEnemigo
        tiempoVuelo = distancia / velocidadProyectil  -- Toma la velocidad del proyectil de los hiperparametros
        -- Predecir posición futura
        posFutura = ( fst posEnemigo + fst dirEnemigo * velEnemigo * tiempoVuelo,
                      snd posEnemigo + snd dirEnemigo * velEnemigo * tiempoVuelo )
                
-- BOT TONTO: no hace nada
decisionRobotTonto :: [Robot] -> [Obstaculo] ->  Robot -> (Memoria,AccionesRobot)
decisionRobotTonto _ _ robot = (getMemoria robot, accionesVacias)

-- BOT ACELERA: solo acelera
decisionRobotAcelera :: [Robot] -> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decisionRobotAcelera _  _ robot = (getMemoria robot,accionesVacias {acelera = True, rota_cagnon_antihorario = True, dispara = True})

-- BOT GIRA: solo gira
decisionRobotGira :: [Robot] -> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decisionRobotGira _ _ robot = (getMemoria robot,accionesVacias {acelera=True, rota_body_horario = True, rota_cagnon_antihorario = True })

-- BOT CAZADOR: persigue a alos enemigos y los mata
decidirBotCazador :: [Robot] -> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decidirBotCazador [] _  robot = -- Si no hay robots visibles da vueltas disparando en circulos y acelerando para buscar un enemigo
  (getMemoria robot,accionesVacias { acelera = True, rota_body_horario = True, rota_cagnon_horario = True, dispara = True })
decidirBotCazador robots_visibles objetos_visibles robot =
  ( getMemoria robot,
    accionesVacias
      { acelera = True,
        dispara = True,
        rota_body_horario = rotarBodyHorario,
        rota_body_antihorario = rotarBodyAntihorario,
        rota_cagnon_horario = rotarCanonHorario,
        rota_cagnon_antihorario = rotarCanonAntihorario
      })
  where pos = posicion robot
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

-- BOT COBARDE: huye mientras dispara
decidirBotCobarde :: [Robot] -> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decidirBotCobarde [] obstaculos_visibles robot =
  (getMemoria robot, accionesVacias
    { acelera = acelera,
      frena = frena,
      rota_body_horario = rotarBodyHorario,
      rota_body_antihorario = rotarBodyAntihorario,
      rota_cagnon_antihorario = True,
      dispara = True
    })
  where
    pos = posicion robot
    dirVector = direccion robot
    anguloActualBody = vectorToAngle dirVector

    -- Filtra los obstáculos curativos
    obstaculos_curativos = filter (\o -> getTipoObstaculo o == Curativo) obstaculos_visibles

    -- Si hay curativos, acelera; si no, frena
    acelera = length obstaculos_curativos > 0
    frena = length obstaculos_curativos == 0

    -- Comprobamos si la lista está vacía antes del fold
    (rotarBodyHorario, rotarBodyAntihorario) =
      if null obstaculos_curativos
        then (False, False)  -- No rotamos si no hay objetivos
        else
          let obstaculoMasCercano = foldl1
                (\o1 o2 ->
                  if distanceBetween pos (posicion o1) < distanceBetween pos (posicion o2)
                    then o1 else o2)
                obstaculos_curativos

              anguloObjetivo = angleToTarget pos (posicion obstaculoMasCercano)
              deltaAnguloBody = calculaDeltaAngulo anguloObjetivo anguloActualBody
          in (deltaAnguloBody < 0, deltaAnguloBody > 0)


decidirBotCobarde robots_visibles obstaculos_visibles robot =
  ( getMemoria robot,
    accionesVacias
      { acelera = True,
        dispara = True,
        rota_body_horario = rotarBodyHorario,
        rota_body_antihorario = rotarBodyAntihorario,
        rota_cagnon_horario = rotarCanonHorario,
        rota_cagnon_antihorario = rotarCanonAntihorario
      })
  where pos = posicion robot
        dirVector = direccion robot
        anguloCanon = getAnguloDisparo robot
        -- Selecciona el enemigo más cercano si la lista tiene un solo elemento  devuelve el primer elemento sin provocar errores
        enemigoMasCercano = foldl1 (\r1 r2 -> if distanceBetween pos (posicion r1) < distanceBetween pos (posicion r2) then r1 else r2) robots_visibles

        -- Calcula el ángulo hacia el enemigo (usando posición predicha)
        anguloObjetivoDisparo = angleToTarget pos (predecirPosicionEnemigo pos enemigoMasCercano)
          
        -- Calcula el ángulo opuesto para huir
        -- Normaliza el ángulo de huída a rango
        anguloObjetivoBody =
          let raw = anguloObjetivoDisparo + 180
          in if raw >= 360 then raw - 360 else raw


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

-- BOT BUSCADOR: Se da vueltas hasta encontrar enemigos y luego se para a matarlo
decidirBotBuscador :: [Robot] -> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decidirBotBuscador [] _ robot = -- Si no hay robots visibles se queda quieto disparando en cierculos
  (getMemoria robot, accionesVacias { acelera = True, rota_cagnon_antihorario = True, dispara = True, rota_body_antihorario = True })
decidirBotBuscador robots_visibles obstaculos_visibles robot =
  ( getMemoria robot,
    accionesVacias
      { frena = True,
        dispara = True,
        rota_cagnon_horario = rotarCanonHorario,
        rota_cagnon_antihorario = rotarCanonAntihorario
      })
  where pos = posicion robot
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


-- BOT FRANCOTIRADOR: Se va a una esquina del mapa y desde alli dispara a los enemigos
decidirBotFrancotirador :: [Robot] -> [Obstaculo] -> Robot -> (Memoria,AccionesRobot)
decidirBotFrancotirador [] _ robot = -- Si no hay robots visibles se va a la esquina y dispara en circulos
  ( nueva_memoria_modificada,
    accionesVacias 
      { acelera = True, 
       dispara = True,
       rota_body_horario = rotarBodyHorario,
       rota_body_antihorario = rotarBodyAntihorario,
       rota_cagnon_horario = rotarCagnonHorario,
       rota_cagnon_antihorario = rotarCagnonAntihorario
      })
  where pos = posicion robot
        dirVector = direccion robot
        -- Convierte el vector de dirección del cuerpo a ángulo
        anguloActualBody = vectorToAngle dirVector
        
        xmin_global = fst (fst globalBounds)
        ymin_global = fst (snd globalBounds)
        -- Calcula el ángulo hacia la esquina del mapa
        anguloObjetivo = angleToTarget pos (xmin_global+40,ymin_global+40)  -- Esquina del mapa
          
        -- Diferencias de ángulo para cuerpo
        deltaAnguloBody = calculaDeltaAngulo anguloObjetivo anguloActualBody

        -- Lógica de rotación del CUERPO (hacia donde acelera)
        rotarBodyHorario = deltaAnguloBody < 0
        rotarBodyAntihorario = deltaAnguloBody > 0

        -- Lógica de rotación del CAgnÓN (gira constantemente barriendo el area de juego entre 0 y 90 grados para matar desde su esquina cuando llegue)
        -- Ángulo actual del cagnón entre 0 y 360
        anguloCanon = getAnguloDisparo robot
        -- ver con la memoria si estaba gitando antihorario o no
        memoria_actual = getMemoria robot
        flag_giro_antihorario=getBool "girando_antihorario" memoria_actual
        valor_actual = if flag_giro_antihorario == Just False then False else True -- si aún no hay un valor en la memoria (ó si existe y es true), empieza girando antihorario
        -- Ver si el cañón ha llegado a los límites de 0 o 90 grados
        nuevo_valor = if anguloCanon >= 90 then False
                      else if anguloCanon <= 0 then True
                        else valor_actual
        nueva_memoria_modificada = memStore "girando_antihorario" (VBool nuevo_valor) memoria_actual
        rotarCagnonAntihorario = nuevo_valor == True
        rotarCagnonHorario = nuevo_valor == False
    

decidirBotFrancotirador robots_visibles obstaculos_visibles robot = --hay enemigos a la vista
  ( getMemoria robot,
    accionesVacias
      { acelera = True,
        dispara = True,
        rota_body_horario = rotarBodyHorario,
        rota_body_antihorario = rotarBodyAntihorario,
        rota_cagnon_horario = rotarCanonHorario,
        rota_cagnon_antihorario = rotarCanonAntihorario
      })
  where pos = posicion robot
        dirVector = direccion robot
        anguloCanon = getAnguloDisparo robot
        -- Convierte el vector de dirección del cuerpo a ángulo
        anguloActualBody = vectorToAngle dirVector

        -- Selecciona el enemigo más cercano si la lista tiene un solo elemento  devuelve el primer elemento sin provocar errores
        enemigoMasCercano = foldl1 (\r1 r2 -> if distanceBetween pos (posicion r1) < distanceBetween pos (posicion r2) then r1 else r2) robots_visibles

        -- Calcula el ángulo hacia el enemigo (usando posición predicha)
        anguloObjetivoDisparo = angleToTarget pos (posicion enemigoMasCercano)
          
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