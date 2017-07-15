###############################################
###############################################
# Pruebas Binomiales
############ Comparan la proporcion de casos X
############ hallados en los datos vs el azar
################################################


########################
# GANANCIAS
########################

#Todas las Sesiones - Contando Empates
binom.test(43, 80, p = 1/3, 'g')  #El Jugador 1 ganó más en los 8 periodos  (Contando Empates)
binom.test(20,40,p= 1/3, 'g')     #El Jugador 1 ganó más en el segundo juego (Contando Empates)
binom.test(7,10, p=1/3, 'g')      #Sesiones donde el Jugador 1 ganó más que 4 y 5 en el Segundo Juego (Contando Empates)

#Todas las sesiones - SIN empates
binom.test(40, 80, p = 1/3, 'g')  #El Jugador 1 ganó más en los 8 periodos  (SIN Empates)
binom.test(19,40,p= 1/3, 'g')     #El Jugador 1 ganó más en el segundo juego (SIN Empates)
binom.test(6,10, p=1/3, 'g')      #Sesiones donde el Jugador 1 ganó más que 4 y 5 en el Segundo Juego (SIN Empates)

#Sin Sesion 3 - Contando Empates
binom.test(39, 72, p = 1/3, 'g')  #El Jugador 1 ganó más en los 8 periodos  (Contando Empates)
binom.test(18, 36, p= 1/3, 'g')     #El Jugador 1 ganó más en el segundo juego (Contando Empates)
binom.test(6, 9, p=1/3, 'g')       #Sesiones donde el Jugador 1 ganó más que 4 y 5 en el Segundo Juego (Contando Empates)

#Sin Sesión 3 - SIN empates
binom.test(36, 72, p = 1/3, 'g')  #El Jugador 1 ganó más en los 8 periodos  (SIN Empates)
binom.test(17,36,p= 1/3, 'g')     #El Jugador 1 ganó más en el segundo juego (SIN Empates)
binom.test(5,9, p=1/3, 'g')       #Sesiones donde el Jugador 1 ganó más que 4 y 5 en el Segundo Juego (SIN Empates)

########################
# Previous games
########################

binom.test(21, 36, p = .33, 'g') 
