module GameUpdates where

-- actualiza el GameState y es llamado por el play antes de dibujar cada frame
import Geometry
import GameState
import Memoria
import Entidades
import Colision
import Bot
import Hyperparams

-- Bucle principal de actualizacion del juego
updateGame:: Float -> GameState -> GameState
updateGame dt state
    | pantalla state == MenuInicio = state -- No actualizar mientras estemos en la pantalla de inicio
    |  gamePausado state = state -- Si el juego esta pausado no se hace nada
    |  gameOver state = state -- El final del juego: no s ehace nada
    | otherwise = nuevo_state
        where 
            -- incrementar tiempo
            state1 = incrementaTiempo dt state
            -- borrar explosiones viejas
            explosiones_actualizadas = borrarExplosiones state1
            state2 = state1 {allExplosiones = explosiones_actualizadas }
            -- actualizar posiciones de proyectiles y borrar los que se salen del mapa
            proyectiles_actualizados_pos = actualizaPosicionProy state2 dt --pasa los proyectiles ya filtrados (elimina las que salen de pantalla)
            state3 = state2 { allProyectiles = proyectiles_actualizados_pos }
            -- manejar colisiones de proyectiles con robots
            colisiones = verificarColisionesRobotProyectil state3 -- lista de Eventos de colisiones entre proyectiles y robots
            state4 = procesarTodasLasColisiones state3 colisiones
            -- agnadir explosiones por los robots que han muerto en este frame
            nuevas_explosiones = [Explosion (posicion r) (tiempo state4) duracionExplosion | r <- allRobots state4, not (isRobotAlive r) ]
            state4_con_explosiones = state4 { allExplosiones = allExplosiones state4 ++ nuevas_explosiones }
            -- eliminar robots muertos
            robots_vivos = filter isRobotAlive (allRobots state4_con_explosiones)
            state5 = state4_con_explosiones { allRobots = robots_vivos }
            -- actualizar memoria a cada robot
            state6 = actualizaMemoriaRobots state5
            -- aplicar las decisiones de los robots: giro body, giro cagnon, disparo, aceleracion. Agnade los proyectiles disparados a la lista y actualiza los robots asegurandose de que no se producen colisiones al girar el body. Si se produce colision al girar el body, no gira
            state7 = recursivaRobotsDecisiones (allRobots state6) state6 dt
            -- Desplazamiento de Robots
            robots_finales = map (\r -> intentaAplicarDesplazamiento r state7 dt) (allRobots state7)
            state8 = state7 { allRobots = robots_finales }
            -- Revisar si el juego ha terminado (si queda un solo robot)
            state9
                | length (allRobots state8) <= 1 = state8 { gameOver = True } -- Si queda un solo robot, el juego termina
                | otherwise = state8
            --- Estado final
            nuevo_state = state9
-- ----------------------------------------------------------------------------------------------------
-- State 1 ==> Incrementa el tiempo del GameState
incrementaTiempo :: Float -> GameState -> GameState
incrementaTiempo dt state = state { tiempo = tiempo state + dt }
------------------------------------------------------------------------------------------------------
-- State 2 ==> Borrar las explosiones que llevan mas de cierto tiempo vivas
borrarExplosiones :: GameState -> [Explosion]
borrarExplosiones state = filter (\e -> tiempoInicio e + duracion e > tiempo state) (allExplosiones state)
-- ----------------------------------------------------------------------------------------------------
-- State 3 ==> Actualiza la posicion de proyectiles y borra los que se salen del mapa
actualizaPosicionProy :: GameState -> Float -> [Proyectil]
actualizaPosicionProy state dt = filter dentroDeLimites nuevosProy
  where
    nuevosProy = map (\p -> updatePosition p dt) (allProyectiles state)
    dentroDeLimites p = isInBounds (posicion p) globalBounds
-- ----------------------------------------------------------------------------------------------------
-- State 4 ==>
-- Verificar y detectar todas las colisiones robot-proyectil
verificarColisionesRobotProyectil :: GameState -> [CollisionEvent]
verificarColisionesRobotProyectil state = 
    [RobotProjectileCollision r p |
        r <- allRobots state, 
        p <- allProyectiles state,
        id_entidad r /= getIdLanzador p,  -- No colisiona con quien lo disparó
        checkCollision (robotToRectangle r) (projectileToRectangle p) -- Verifica colisión SAT
    ]

-- Func. calcula colisiones entre PROYECTILES y ROBOTS y mata aquellos que han sido impactados (borrando robot de la lista y poniendo un explosion)
-- Procesa todas las colisiones (por recursión)
procesarTodasLasColisiones :: GameState -> [CollisionEvent] -> GameState
procesarTodasLasColisiones state [] = state  -- Caso base: sin colisiones
procesarTodasLasColisiones state (col:rest) = 
    let state2 = processCollision state col  -- Primera colisión
    in procesarTodasLasColisiones state2 rest -- Siguen las demás

-- Procesa una colisión
-- Devuelve el GameState actualizado con:
--   - La lista de robots con el o aplicado al robot objetivo
--   - La lista de proyectiles sin el proyectil que impactó
processCollision :: GameState -> CollisionEvent -> GameState
processCollision state (RobotProjectileCollision r proyectil) =
    state { allRobots = updatedRobots, allProyectiles = updatedProyectiles, allExplosiones = allExplosiones state ++ [impactExplosion]}
  where
     -- Mapea sobre todos los robots, aplicando dagno solo al que fue impactado
    updatedRobots = map (aplicaDamageRobot (id_entidad r) dagnoProyectil) (allRobots state)
     -- Filtra el proyectil que colisionó, eliminándolo del GameState
    updatedProyectiles = filter (\p -> id_entidad p /= id_entidad proyectil) (allProyectiles state)
    -- Explosion de impacto (breve) en la posicion del proyectil
    impactExplosion = Explosion (posicion proyectil) (tiempo state) duracionImpacto

-- Aplica dagno a un robot específico identificado por su ID objetivo
aplicaDamageRobot :: Int -> Int -> Robot -> Robot
aplicaDamageRobot targetId damage robot
    | id_entidad robot == targetId = setEnergia robot (getEnergia robot - damage)
    | otherwise = robot
-- ----------------------------------------------------------------------------------------------------
-- State 6 ==> Func. que actualiza la memoria para cada robot
actualizaMemoriaRobots :: GameState -> GameState
actualizaMemoriaRobots state = state {allRobots = robots_actualizados}
    where robots_actualizados = map (actualizaMemoria state) (allRobots state)

actualizaMemoria :: GameState -> Robot -> Robot
actualizaMemoria state r
    | not (isRobotAlive r) = r
    | otherwise =setMemoria r newMem
        where
            oldMem = getMemoria r
            rid = id_entidad r

            -- Número de enemigos vivos detectados 
            enemigos_visibles_ids = [id_entidad robot | robot <- allRobots state, rid /= id_entidad robot,
                                isRobotAlive robot, detectedAgent r robot]

            num_enemigos_visibles = length enemigos_visibles_ids

            -- Actualizar memoria
            newMem =memStore "current_energy" (VInt (getEnergia r))
                   $ memStore "last_position" (VPoint (posicion r))            
                   $ memStore "num_enemigos_visibles" (VInt num_enemigos_visibles)
                   $ memStore "enemigos_visibles_ids" (ListIntValue enemigos_visibles_ids) oldMem
-- ----------------------------------------------------------------------------------------------------
{-- State 7 ==> Func. recursiva que para cada robot modifica el GameState preguntando y aplicando
    las decisiones del robot y agnadiendo un proyectil a la lista si dispara
--}
recursivaRobotsDecisiones :: [Robot] -> GameState -> Float -> GameState
recursivaRobotsDecisiones [] state dt = state -- caso base, Si ya no quedan robots, devuelve el estado
recursivaRobotsDecisiones (r:rs) state dt = nuevo_state_final
    where
        (r_actualizado, ha_disparado) = preguntaEIntentaAplicarGiroYSiHayDisparo r state dt
        -- Si ha disparado, crear un nuevo proyectil y agnadirlo a la lista de proyectiles del estado
        nuevo_state_con_proyectil
            | ha_disparado = state { allProyectiles = nuevo_proyectil : allProyectiles state, latestAvaileableId = nuevo_id_proyectil + 1 } 
            | otherwise    = state
            where
                -- Crear nuevo proyectil y solo se ejecuta si ha_disparado es True debido a la evaluación perezosa de Haskell
                nuevo_id_proyectil = latestAvaileableId state
                nuevo_proyectil = creaUnProyectilDisparado nuevo_id_proyectil r_actualizado

        -- Actualizar la lista de robots en el estado con el robot actualizado
        -- Reemplaza el robot antiguo por el actualizado filtrando el antiguo y agnadiendo el actualizado
        nuevo_state_con_robot = nuevo_state_con_proyectil { allRobots = r_actualizado : filter (\r -> id_entidad r /= id_entidad r_actualizado) (allRobots nuevo_state_con_proyectil) }

        -- Llamada recursiva para los siguientes robots
        nuevo_state_final = recursivaRobotsDecisiones rs nuevo_state_con_robot dt

-- Func. que pregunta al bot sus decisiones y las aplica al robot, comprobando si hay errores de posicion al girar el cuerpo.
-- Duevuelve el robot actualizado y un Bool que indica si ha disparado o no
preguntaEIntentaAplicarGiroYSiHayDisparo :: Robot -> GameState -> Float -> (Robot,Bool)
preguntaEIntentaAplicarGiroYSiHayDisparo r0 state dt = (r_final,va_a_disparar)
    where
        id_robots_visibles = getListIntOr "enemigos_visibles_ids" (getMemoria r0) -- IDs de robots visibles
        allRobotsInGame = allRobots state -- Obtener todos los robots del estado del juego
        robots_visibles = [ r | r <- allRobotsInGame , id_entidad r `elem` id_robots_visibles ] -- Filtrar robots visibles por ID
        -- Preguntar al bot sus decisiones
        acciones = decidirBot robots_visibles r0
        --Comrpobams si ha disparado y puede segun el coolDown está permitido que dispare
        momento_ultimo_disparo = getMomentoUltimoDisparo r0
        va_a_disparar = (dispara acciones) && ((momento_ultimo_disparo + coolDownDisparo) <= tiempo state)
        -- Actualizar el momento del último disparo si ha disparado
        nuevo_momento_ultimo_disparo
            | va_a_disparar = tiempo state
            | otherwise     = momento_ultimo_disparo
        -- Actualizar el momento del último disparo en el robot
        r1 = setMomentoUltimoDisparo r0 nuevo_momento_ultimo_disparo
        -- Aplicar las decisiones que no provocan inconsistencias
        r2 = aplicarDecisionesQueNoProvocanInconsistenciasBot r1 acciones dt -- aplica todas menos giro cuerpo pq giro cuerpo puede provocar colisiones
        -- Intentar aplicar el giro del cuerpo solo si no provoca colisiones
        r3 = aplicaGiroCuerpo r2 acciones dt -- aplica giro cuerpo
        r_final
            | compruebaErrorPosicionGiro state r3 = updateRobotVelocity r2 0.0 -- si al girar el cuerpo provoca colision, no gira y se pone su velocidad a 0 porque se ha chocado
            | otherwise                       = r3 -- si al girar el cuerpo no provoca colision, gira

-- Func. que aplica el giro del cuerpo del robot   
aplicaGiroCuerpo :: Robot -> AccionesRobot -> Float-> Robot
aplicaGiroCuerpo r acciones dt = r_final
    where
        -- Obtener el ángulo actual del cuerpo del robot a partir de su vector de dirección
        anguloBody = vectorToAngle (direccion r)
        -- Cálculo el nuevo ángulo
        nuevo_anguloBody
            | rota_body_horario acciones && rota_body_antihorario acciones = anguloBody -- Si rota en ambas direcciones simultáneamente, se cancelan y no rota
            | rota_body_horario acciones     = (anguloBody - velocidadRotacionBody * dt)
            | rota_body_antihorario acciones = (anguloBody + velocidadRotacionBody * dt)
            | otherwise                      = anguloBody

        r_final = updateAngleRobot r (angleToVector nuevo_anguloBody)

-- Función que calcula l
aplicarDecisionesQueNoProvocanInconsistenciasBot :: Robot -> AccionesRobot -> Float -> Robot
aplicarDecisionesQueNoProvocanInconsistenciasBot r acciones dt = r_final
    where
        -- Aplicar movimiento
        r1 
            | acelera acciones && frena acciones = r         -- Si acelera y frena a la vez, no hace nada
            | acelera acciones                   = updateVelocity r Acelera
            | frena acciones                     = updateVelocity r Desacelera
            | otherwise                          = r         -- No hace nada

        -- Rotar cagnon
        anguloCanon = getAnguloDisparo r1
        nuevo_anguloCanon 
            | rota_cagnon_horario acciones && rota_cagnon_antihorario acciones = anguloCanon -- Si rota en ambas direcciones simultáneamente, se cancelan y no rota
            | rota_cagnon_horario acciones     = (anguloCanon - velocidadRotacionCanon * dt)
            | rota_cagnon_antihorario acciones = (anguloCanon + velocidadRotacionCanon * dt)
            | otherwise                       = anguloCanon

        r_final = setAnguloDisparo r1 nuevo_anguloCanon

-- Comprueba si el robot se ha salido del mapa o ha colisionado con otro robot
compruebaErrorPosicionDesplazamiento :: GameState -> Robot -> Bool
compruebaErrorPosicionDesplazamiento state r = (isRectangleOutOfBounds (robotToRectangle r)) || (hasRobotCollision r (allRobots state))

-- Comprueba si el robot ha colisionado con otro robot. SI se sale del mapa si puede girar
compruebaErrorPosicionGiro :: GameState -> Robot -> Bool
compruebaErrorPosicionGiro state r = hasRobotCollision r (allRobots state)

-- Func. que crea un nuevo proyectil y lo agnade a la lista del GameState     
creaUnProyectilDisparado :: Int -> Robot -> Proyectil
creaUnProyectilDisparado nuevoId r = proyectil
    where direccion_proy = angleToVector (getAnguloDisparo r)
          proyectil = crearProyectil
                nuevoId 
                (id_entidad r) 
                (posicion r) 
                (velocidadProyectil)
                (direccion_proy)
                (fst proyectil_tamagno) -- Definido en hyperparams
                (snd proyectil_tamagno)
                
-- ----------------------------------------------------------------------------------------------------
-- State 8 ==> Intenta aplicar el desplazamiento del robot y comprueba si provoca colisiones, si provoca colisones no lo aplica
intentaAplicarDesplazamiento :: Robot -> GameState -> Float -> Robot
intentaAplicarDesplazamiento r1 state dt = r_final
    where
        r2 = aplicaDesplazamientoEntidad r1 dt-- actualiza la posicion del robot segun su velocidad
        r_final
            | compruebaErrorPosicionDesplazamiento state r2 = updateRobotVelocity r1 0.0 -- si al avanzar causa colision, no avanza y se pone su velocidad a 0 porque s eha chocado
            | otherwise                       = r2 -- si al avanzar con su velocidad no provoca colision, avanza
      

-- Desplaza una entidad (proyectil o robot) según su velocidad y dirección, dado un delta time
aplicaDesplazamientoEntidad :: Entidad a -> Float -> Entidad a
aplicaDesplazamientoEntidad entidad dt = updatePosition entidad dt


