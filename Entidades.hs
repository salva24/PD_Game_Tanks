-- Módulo que tiene los tipos proyectil y robot
module Entidades where

import Geometry
import Memoria
import Hyperparams

-- TIPO GENÉRICO BASE CON PARÁMETRO DE TIPO
data Entidad a = Entidad {
    id_entidad :: Int,
    posicion :: Position,
    modulo_velocidad :: Float,
    direccion :: Vector,
    ancho :: Float,
    alto :: Float,
    datos_especificos :: a  -- parámetro de tipo para datos específicos (DatosRobot o DatosProyectil)
} deriving (Eq, Show)

-- DATOS ESPECÍFICOS DE CADA TIPO
data DatosRobot = DatosRobot {
    energia :: Int,
    angulo_disparo :: Angle,--valor entre 0 y 360 grados
    radar :: Distance,
    memoria :: Memoria,
    funcion_decision :: String, -- función de decisión del bot
    momento_ultimo_disparo :: Float -- tiempo en el que disparó por última vez
} deriving (Eq, Show)

data DatosProyectil = DatosProyectil {
    id_lanzador :: Int
} deriving (Eq, Show)

-- TYPE ALIASES PARA USAR NOMBRES LIMPIOS
type Robot = Entidad DatosRobot
type Proyectil = Entidad DatosProyectil

-- Movimientos que puede realizar un robot
data AccionMovimiento = Acelera | Desacelera | Mantiene deriving (Show, Eq, Enum)


-- FUNCIONES DE ACCESO PARA DATOS ESPECÍFICOS POR COMODIDAD
-- Obtener energía de un robot
getEnergia :: Robot -> Int
getEnergia robot = energia (datos_especificos robot)

-- Obtener radar de un robot
getRadar :: Robot -> Distance
getRadar robot = radar (datos_especificos robot)

-- Obtener ángulo de disparo de un robot
getAnguloDisparo :: Robot -> Angle
getAnguloDisparo robot = angulo_disparo (datos_especificos robot)

-- Obetener la memoria de un robot
getMemoria :: Robot -> Memoria
getMemoria robot = memoria (datos_especificos robot)


-- Obtener id del lanzador de un proyectil
getIdLanzador :: Proyectil -> Int
getIdLanzador proy = id_lanzador (datos_especificos proy)

-- Obtener función de decisión de un robot
getFuncionDecision :: Robot -> String
getFuncionDecision robot = funcion_decision (datos_especificos robot)

-- Obtener momento del último disparo de un robot
getMomentoUltimoDisparo :: Robot -> Float
getMomentoUltimoDisparo robot =  momento_ultimo_disparo (datos_especificos robot)

-- Modificar energía de un robot
setEnergia :: Robot -> Int -> Robot
setEnergia robot nueva_energia = 
    robot {datos_especificos = (datos_especificos robot) {energia = nueva_energia}}

-- Modificar ángulo de disparo de un robot
setAnguloDisparo :: Robot -> Angle -> Robot
setAnguloDisparo robot nuevo_angulo = 
    robot {datos_especificos = (datos_especificos robot) {angulo_disparo = nuevo_angulo}}

-- Modificar radar de un robot
setRadar :: Robot -> Distance -> Robot
setRadar robot nuevo_radar = 
    robot {datos_especificos = (datos_especificos robot) {radar = nuevo_radar}}

-- Set la memoria de un robot
setMemoria :: Robot -> Memoria -> Robot
setMemoria robot nueva_memoria = 
    robot {datos_especificos = (datos_especificos robot) {memoria = nueva_memoria}}

-- Set el momento del último disparo de un robot
setMomentoUltimoDisparo :: Robot -> Float -> Robot
setMomentoUltimoDisparo robot nuevo_momento = 
    robot {datos_especificos = (datos_especificos robot) {momento_ultimo_disparo = nuevo_momento}}


-- CONSTRUCTORES CONVENIENTES
crearRobot :: Int -> Position -> Float -> Vector -> Float -> Float -> Int -> Angle -> Distance -> Memoria -> String -> Float -> Robot
crearRobot id_r pos mod_v dir ancho_r alto_r energia_r angulo radar_r memoria_r f_dec ultima_vez_disparo = 
    Entidad id_r pos mod_v dir ancho_r alto_r (DatosRobot energia_r angulo radar_r memoria_r f_dec ultima_vez_disparo)

crearProyectil :: Int -> Int -> Position -> Float -> Vector -> Float -> Float -> Proyectil
crearProyectil id_p id_lanz pos mod_v dir ancho_p alto_p = 
    Entidad id_p pos mod_v dir ancho_p alto_p (DatosProyectil id_lanz)


-- FUNCIONES:
-- Determinar si un agente ha detectado a otro dentro del rango de su radar
--    la func. "distanceBetween" calcula la dist. resultante y vemos si está dentro del rango de acción del robot1
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distanceBetween (posicion r1) (posicion r2) <= getRadar r1

-- Comprueba que la energía del robot sea mayor que 0
isRobotAlive :: Robot -> Bool
isRobotAlive r1 = getEnergia r1 > 0

-- Contar los robots que están vivos
countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r] 

-- Actualiza la velocidad de un robot con una velocidad dada
updateRobotVelocity :: Robot -> Float -> Robot
updateRobotVelocity robot modulo_v = robot {modulo_velocidad = modulo_v}

-- Velocidad máxima permitida
maxVelocity :: Float
maxVelocity = 10.0

-- updateVelocity: Actualizar velocidad basada en la acción de movimiento
updateVelocity :: Robot -> AccionMovimiento -> Robot

-- Caso de acción de acelerar: acelera sin sobrepasar la velocidad máxima
updateVelocity robot Acelera
    | v + incrementoVelocidad < maxAllowedVelocity = updateRobotVelocity robot (v + incrementoVelocidad)
    | otherwise = updateRobotVelocity robot maxAllowedVelocity
    where v = modulo_velocidad robot

-- Caso de acción de desacelerar: desacelera sin bajar de 0
updateVelocity robot Desacelera
    | v - incrementoVelocidad > 0   = updateRobotVelocity robot (v - incrementoVelocidad)
    | otherwise   = updateRobotVelocity robot 0
  where v = modulo_velocidad robot

-- Caso de acción de mantener: no cambia la velocidad
updateVelocity robot Mantiene = robot -- Esto no se uy se trata de una version antigua d eotra entrega

-- Actualiza posición de proyectil o robot en función de la velocidad y el incremento de tiempo
updatePosition :: Entidad a -> Float -> Entidad a
updatePosition e delta_tiempo = e {posicion = (x + vx*delta_tiempo, y + vy*delta_tiempo)}
    where (x, y) = posicion e
          (vx, vy) = multEscalar (modulo_velocidad e) (direccion e)

-- No lo ha pedido pero creemos que necesitamos una funcion para mover el robot y el cagnon
-- Actualiza la dirección de un robot para que el bot pueda girarlo
updateAngleRobot :: Robot -> Vector -> Robot
updateAngleRobot robot v = robot {direccion = v}

-- Actualiza el angulo de disparo del robot para que el bot pueda girar el cagnón
-- Normaliza el ángulo para mantenerlo entre 0 y 360
updateAngleCanon :: Robot -> Angle -> Robot
updateAngleCanon robot a = setAnguloDisparo robot normalizedAngle
  where
    temp = a - 360 * fromIntegral (floor (a / 360) :: Int)
    normalizedAngle = if temp < 0 then temp + 360 else temp