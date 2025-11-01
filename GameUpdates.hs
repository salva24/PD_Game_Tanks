module GameUpdates where

-- actualiza el GameState y es llamado por el play antes de dibujar cada frame
import Geometry
import GameState
import Memoria
import Entidades
import Colision
import Bot
import Hyperparams
import Debug.Trace (trace)

-- Bucle principal de actualizacion del juego
updateGame:: Float -> GameState -> GameState
updateGame dt state
    | pantalla state == MenuInicio = state -- No actualizar mientras estemos en la pantalla de inicio
    | gamePausado state            = state -- Si el juego esta pausado no se hace nada
    | gameOver state               = state  -- El final del juego: no s ehace nada
    | otherwise                    = nuevo_state
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
            state4 = procesarTodasLasColisionesProyectiles state3 colisiones

            --Obstaculos activados
            -- Actualizar tiempo restante de explosion a quellos obstaculos con getMomentoActivacion>=0
            state4_con_tiempo_obstaculos_actualizado = state4 { allObstaculos = map (\o->actualizarTiempoObstaculoConDelay o dt) (allObstaculos state4) }
            --Borrar explotando los obstaculos de explosion retardada que ya han sido activados hace más de su tiempo de delay y aplicar dagno a los robots en su rango
            state4_sin_obstaculos_explotados = manejarExplosionesObstaculos state4_con_tiempo_obstaculos_actualizado

            -- agnadir explosiones por los robots que han muerto en este frame
            nuevas_explosiones = [Explosion (posicion r) (tiempo state4_sin_obstaculos_explotados) duracionExplosion ExpMuerte | r <- allRobots state4_sin_obstaculos_explotados, not (isRobotAlive r)]
            state4_con_explosiones = state4_sin_obstaculos_explotados { allExplosiones = allExplosiones state4_sin_obstaculos_explotados ++ nuevas_explosiones }
            -- eliminar robots muertos
            robots_vivos = filter isRobotAlive (allRobots state4_con_explosiones)
            state5 = state4_con_explosiones { allRobots = robots_vivos }
            -- actualizar memoria a cada robot
            state6 = actualizaMemoriaRobots state5
            -- aplicar las decisiones de los robots: giro body, giro cagnon, disparo, aceleracion. Agnade los proyectiles disparados a la lista y actualiza los robots asegurandose de que no se producen colisiones al girar el body. Si se produce colision al girar el body, no gira
            state7 = recursivaRobotsDecisiones (allRobots state6) state6 dt
            -- Desplazamiento de Robots y choques con obstaculos presentes en el mapa
            state8=manejarDesplazamientosRobotsYColisionesObstaculos state7 dt
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
    where nuevosProy = map (\p -> updatePosition p dt) (allProyectiles state)
          dentroDeLimites p = isInBounds (posicion p) globalBounds
-- ----------------------------------------------------------------------------------------------------
-- State 4
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
procesarTodasLasColisionesProyectiles :: GameState -> [CollisionEvent] -> GameState
procesarTodasLasColisionesProyectiles state [] = state      -- Caso base: sin colisiones
procesarTodasLasColisionesProyectiles state (col:rest) = 
    let state2 = processCollision state col                 -- Primera colisión
    in procesarTodasLasColisionesProyectiles state2 rest    -- Siguen las demás

-- Obstaculos -------------------------------------------------
-- Actualiza el tiempo restante de explosion de los obstaculos con delay que han sido activados
actualizarTiempoObstaculoConDelay :: Obstaculo -> Float -> Obstaculo
actualizarTiempoObstaculoConDelay o delta_tiempo
    | getMomentoActivacion o < 0 = o -- No ha sido activado, no hacer nada
    | otherwise = setTiempoExplosion o (getTiempoExplosion o - delta_tiempo) -- Restar el tiempo transcurrido al tiempo restante

-- Mira las explosiones retardadas que han sido activadas y deben explotar  y aplica el daño a los robots de su rango de dagno
manejarExplosionesObstaculos :: GameState -> GameState
manejarExplosionesObstaculos state = manejarExplosionesRecursivoAux state (allObstaculos state) 

manejarExplosionesRecursivoAux :: GameState -> [Obstaculo] -> GameState
manejarExplosionesRecursivoAux state [] = state -- Caso base: sin obstaculos que procesar
manejarExplosionesRecursivoAux state (o:os)
    | getTipoObstaculo o == Explosivo && getTiempoExplosion o <= 0 && getMomentoActivacion o >= 0  = manejarExplosionesRecursivoAux state_nuevo os -- Ha sido explotado, es tipo explosion y le toca explotar
    | otherwise = manejarExplosionesRecursivoAux state os -- No ha explotado
        where
            -- Crear explosion
            nueva_explosion = Explosion (posicion o) (tiempo state) duracionExplosion ExpObstaculoExplosivo
            -- Aplicar daño a robots en rango
            robots_actualizados = map (aplicaDamageSiEnRango o) (allRobots state)
            -- Actualizar estado
            state_nuevo = state { allRobots = robots_actualizados, allExplosiones = allExplosiones state ++ [nueva_explosion], allObstaculos = filter (\ob -> id_entidad ob /= id_entidad o) (allObstaculos state) } -- Eliminar obstaculo explotado

aplicaDamageSiEnRango :: Obstaculo -> Robot -> Robot
aplicaDamageSiEnRango o r
    | distancia <= getRadioExplosion o = setEnergia r (getEnergia r - getDanoObstaculo o) -- Si está en rango, aplicar daño
    | otherwise = r -- Si no está en rango, no hacer nada
    where
        distancia = distanceBetween (posicion o) (posicion r)
-- ----------------------------------------------------------------------------------------------------
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
    impactExplosion = Explosion (posicion proyectil) (tiempo state) duracionImpacto ExpProyectil

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

            obstaculos_visibles_ids = [id_entidad o | o <- allObstaculos state, detectedObstacle r o]

            -- Actualizar memoria
            newMem =memStore "current_energy" (VInt (getEnergia r))
                   $ memStore "last_position" (VPoint (posicion r))            
                   $ memStore "num_enemigos_visibles" (VInt num_enemigos_visibles)
                   $ memStore "enemigos_visibles_ids" (ListIntValue enemigos_visibles_ids) 
                   $ memStore "obstaculos_visibles_ids"(ListIntValue obstaculos_visibles_ids)  oldMem
-- ----------------------------------------------------------------------------------------------------
{-- State 7 ==> Func. que para cada robot modifica el GameState preguntando y aplicando
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
        id_obstaculos_visibles = getListIntOr "obstaculos_visibles_ids" (getMemoria r0) -- IDs de obstaculos visibles
        obstaculos_visibles = [ o | o <- allObstaculos state , id_entidad o `elem` id_obstaculos_visibles ] -- Filtrar obstaculos visibles por ID
        -- Preguntar al bot sus decisiones
        (nueva_memoria, acciones) = decidirBot robots_visibles obstaculos_visibles r0
        -- Actualizar la memoria del robot
        r0_con_memoria = setMemoria r0 nueva_memoria
        --Comrpobams si ha disparado y puede segun el coolDown está permitido que dispare
        momento_ultimo_disparo = getMomentoUltimoDisparo r0_con_memoria
        va_a_disparar = (dispara acciones) && ((momento_ultimo_disparo + coolDownDisparo) <= tiempo state)
        -- Actualizar el momento del último disparo si ha disparado
        nuevo_momento_ultimo_disparo
            | va_a_disparar = tiempo state
            | otherwise     = momento_ultimo_disparo
        -- Actualizar el momento del último disparo en el robot
        r1 = setMomentoUltimoDisparo r0_con_memoria nuevo_momento_ultimo_disparo
        -- Aplicar las decisiones que no provocan inconsistencias
        r2 = aplicarDecisionesQueNoProvocanInconsistenciasBot r1 acciones dt -- aplica todas menos giro cuerpo pq giro cuerpo puede provocar colisiones
        -- Intentar aplicar el giro del cuerpo solo si no provoca colisiones
        r3 = aplicaGiroCuerpo r2 acciones dt -- aplica giro cuerpo
        r_final
            | compruebaErrorPosicionGiro state r3 = r2 --ahora al girar si hay una colision la velocidad no se va a cero --updateRobotVelocity r2 0.0 -- si al girar el cuerpo provoca colision, no gira y se pone su velocidad a 0 porque se ha chocado
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
            | rota_body_horario acciones     = anguloBody - velocidadRotacionBody * dt
            | rota_body_antihorario acciones = anguloBody + velocidadRotacionBody * dt
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
            | rota_cagnon_horario acciones     = anguloCanon - velocidadRotacionCanon * dt
            | rota_cagnon_antihorario acciones = anguloCanon + velocidadRotacionCanon * dt
            | otherwise                        = anguloCanon

        r_final = setAnguloDisparo r1 nuevo_anguloCanon

-- Comprueba si el robot se ha salido del mapa o ha colisionado con otro robot
compruebaErrorPosicionDesplazamientoConOtrosRobots :: GameState -> Robot -> Bool
compruebaErrorPosicionDesplazamientoConOtrosRobots state r = (isRectangleOutOfBounds (robotToRectangle r)) || (hasRobotCollision r otrosRobots)
    where
        -- No se incluye el propio robot en la comprobación
        otrosRobots = filter (\x -> id_entidad x /= id_entidad r) (allRobots state)

-- Comprueba si el robot ha colisionado con otro robot o un obstaculo. SI se sale del mapa si puede girar
compruebaErrorPosicionGiro :: GameState -> Robot -> Bool
compruebaErrorPosicionGiro state r = hasRobotCollision r otrosRobots || length (hasRobotObsCollision r (allObstaculos state)) > 0
    where
        -- No se incluye el propio robot en la comprobación
        otrosRobots = filter (\x -> id_entidad x /= id_entidad r) (allRobots state)

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

manejarDesplazamientosRobotsYColisionesObstaculos :: GameState -> Float -> GameState
manejarDesplazamientosRobotsYColisionesObstaculos state dt = recursivaManejarDesplazamientosRobotsYColisionesObstaculosAux state dt (allRobots state)

recursivaManejarDesplazamientosRobotsYColisionesObstaculosAux :: GameState -> Float -> [Robot] -> GameState
recursivaManejarDesplazamientosRobotsYColisionesObstaculosAux state _ [] = state -- caso base, si ya no quedan robots, devuelve el estado
recursivaManejarDesplazamientosRobotsYColisionesObstaculosAux state dt (r:rs) = nuevo_state_con_robot
    where
        -- Actualizar la posición del robot considerando otros robots y límites del mapa
        r_actualizado = intentaAplicarDesplazamientoTeniendoEnCuentaOtrosRobots r state dt

        -- Comprobar colisiones con obstáculos y aplicar efectos
        (se_ha_podido_desplazar, state_nuevo) = comprobarDesplazamientoObstaculos r_actualizado state

        energia_actualizada = getEnergia (extraerRobotPorId r (allRobots state_nuevo))

        r_final
            | se_ha_podido_desplazar = setEnergia r_actualizado energia_actualizada
            | otherwise              = updateRobotVelocity (setEnergia r energia_actualizada) 0.0

        nuevo_state_con_robot =
            state_nuevo { allRobots = r_final : filter (\rob -> id_entidad rob /= id_entidad r_final)
                                               (allRobots state_nuevo) }

extraerRobotPorId :: Robot -> [Robot] -> Robot
extraerRobotPorId r [] = r--no deberia pasar
extraerRobotPorId r (x:xs)
    | id_entidad x == id_entidad r = x--ha encontrado el del mismo id
    | otherwise = extraerRobotPorId r xs
-- ----------------------------------------------------------------------------------------------------
-- State 8 ==> Intenta aplicar el desplazamiento del robot y comprueba si provoca colisiones con otros robots, si provoca colisones no lo aplica
intentaAplicarDesplazamientoTeniendoEnCuentaOtrosRobots :: Robot -> GameState -> Float -> Robot
intentaAplicarDesplazamientoTeniendoEnCuentaOtrosRobots r1 state dt = r_final
    where r2 = aplicaDesplazamientoEntidad r1 dt-- actualiza la posicion del robot segun su velocidad
          r_final
            | compruebaErrorPosicionDesplazamientoConOtrosRobots state r2 = updateRobotVelocity r1 0.0 -- si al avanzar causa colision, no avanza y se pone su velocidad a 0 porque s eha chocado
            | otherwise                       = r2 -- si al avanzar con su velocidad no provoca colision, avanza

-- Desplaza una entidad (proyectil o robot) según su velocidad y dirección, dado un delta time
aplicaDesplazamientoEntidad :: Entidad a -> Float -> Entidad a
aplicaDesplazamientoEntidad entidad dt = updatePosition entidad dt

comprobarDesplazamientoObstaculos :: Robot -> GameState -> (Bool, GameState)
comprobarDesplazamientoObstaculos r state = (se_ha_podido_desplazar, state_nuevo)
    where
        obstaculos = allObstaculos state
        obstaculos_con_los_que_colisiona= hasRobotObsCollision r obstaculos
        se_ha_podido_desplazar= (length obstaculos_con_los_que_colisiona) == 0 -- no ha habido colision
        state_nuevo = foldl (\acc o -> aplicarEfectoObstaculo r acc o) state obstaculos_con_los_que_colisiona -- aplica efectos de los obstaculos con los que colisiona

-- Función que aplica el efecto de un obstáculo al GameState
aplicarEfectoObstaculo :: Robot -> GameState -> Obstaculo -> GameState
aplicarEfectoObstaculo r state o
    | tipo == Solido = state  -- Sólido: no pasa nada (ya se bloqueó el movimiento)
    | tipo == Doloroso = state { allRobots = robots_con_dano }
    | tipo == Explosivo && getMomentoActivacion o < 0 = state { allObstaculos = obstaculos_activados }
    | tipo == Explosivo && getMomentoActivacion o >= 0 = state  -- Ya está activado, no hacer nada más
    | tipo == Curativo = state { 
        allRobots = robots_curados,
        allObstaculos = obstaculos_sin_curativo,
        allExplosiones = allExplosiones state ++ [explosion_curacion]
      }
    
    | otherwise = state
    
    where
        tipo = getTipoObstaculo o
        
        -- Aplicar daño directo al robot (Doloroso)
        robots_con_dano = map aplicarDano (allRobots state)
          where aplicarDano robot
                  | id_entidad robot == id_entidad r = setEnergia robot (getEnergia robot - getDanoObstaculo o)
                  | otherwise = robot
        
        -- Activar obstáculo explosivo
        obstaculos_activados = map activar (allObstaculos state)
          where activar obs
                  | id_entidad obs == id_entidad o = setMomentoActivacion obs (tiempo state)
                  | otherwise = obs
        
        -- Curar robot y eliminar obstáculo curativo
        robots_curados = map curar (allRobots state)
          where curar robot
                  | id_entidad robot == id_entidad r = setEnergia robot (min vidaMaxima (getEnergia robot + abs (getDanoObstaculo o)))
                  | otherwise = robot
        
        obstaculos_sin_curativo = filter (\obs -> id_entidad obs /= id_entidad o) (allObstaculos state)
        
        explosion_curacion = Explosion (posicion o) (tiempo state) duracionExplosion ExpCuracion 