rm(list=ls())
setwd("C:/Users/Alejandro/Desktop/Jaime_Lab25/Datos")
library("grDevices", lib.loc="C:/Program Files/R/R-3.1.1/library")
Data <- read.csv("Datac.csv")
Wins <- read.csv("Datos_Wins.csv")
####################################################
####################################################
# Preparando los datos
####################################################

################################ Data Matrix (Empty)
#Matrices con Puntos (0-1)
W1 <- matrix(data=NA, nrow=8, ncol=10)     #IsWinner by Player1 each Period(row) and Sesion(col)
W2 <- matrix(data=NA, nrow=8, ncol=10)     #IsWinner by Player2 each Period(row) and Sesion(col)
W3 <- matrix(data=NA, nrow=8, ncol=10)     #IsWinner by Player3 each Period(row) and Sesion(col)
#Sumatoria de Puntos por Jugador, por Sesion
TW_P1Ex <- matrix(data=NA, nrow=1, ncol=10)
TW_P1J2 <- matrix(data=NA, nrow=1, ncol=10)
TW_P1J3 <- matrix(data=NA, nrow=1, ncol=10)
TW_P2Ex <- matrix(data=NA, nrow=1, ncol=10)
TW_P2J4 <- matrix(data=NA, nrow=1, ncol=10)
TW_P2J5 <- matrix(data=NA, nrow=1, ncol=10)
#Sumatorias de Puntos... sin incluir al Participante 3
TW_P1Ex_x <- matrix(data=NA, nrow=1, ncol=9)
TW_P1J2_x <- matrix(data=NA, nrow=1, ncol=9)
TW_P1J3_x <- matrix(data=NA, nrow=1, ncol=9)
TW_P2Ex_x <- matrix(data=NA, nrow=1, ncol=9)
TW_P2J4_x <- matrix(data=NA, nrow=1, ncol=9)
TW_P2J5_x <- matrix(data=NA, nrow=1, ncol=9)
#Se eligió el número más pequeño
L1 <- matrix(data=NA, nrow=8, ncol=10)     #GaveLowestNumber by Player 1 each Period(row) and Sesion(col)
L2 <- matrix(data=NA, nrow=8, ncol=10)     #GaveLowestNumber by Player 2 each Period(row) and Sesion(col)
L3 <- matrix(data=NA, nrow=8, ncol=10)     #GaveLowestNumber by Player 3 each Period(row) and Sesion(col)
#Sumatoria de Veces en que cada Jugador dio el número más bajo por Sesion
TL_P1Ex <- matrix(data=NA, nrow=1, ncol=10)
TL_P1J2 <- matrix(data=NA, nrow=1, ncol=10)
TL_P1J3 <- matrix(data=NA, nrow=1, ncol=10)
TL_P2Ex <- matrix(data=NA, nrow=1, ncol=10)
TL_P2J4 <- matrix(data=NA, nrow=1, ncol=10)
TL_P2J5 <- matrix(data=NA, nrow=1, ncol=10)
#Sumatorias de Puntos... sin incluir al Participante 3
TL_P1Ex_x <- matrix(data=NA, nrow=1, ncol=10)
TL_P1J2_x <- matrix(data=NA, nrow=1, ncol=10)
TL_P1J3_x <- matrix(data=NA, nrow=1, ncol=10)
TL_P2Ex_x <- matrix(data=NA, nrow=1, ncol=10)
TL_P2J4_x <- matrix(data=NA, nrow=1, ncol=10)
TL_P2J5_x <- matrix(data=NA, nrow=1, ncol=10)
#El jugador 1 está jugando de manera óptima respecto del Subjuego anterior
W0 <- matrix(data=NA, nrow=8, ncol=10) #Would Player 1 have won on the previous game by choosing the number on this same trial


#### Matrix Values ####
S <- c(0,8,16,24,32,40,48,56,64,72)
c <- c(0,1,2,3,4,5,6,7,8,9)
d <- c(1,2,3,4,5,6,7,8,9,10)

for (a in 1:8){ for (b in 1:10){
  W1[a,b] <- Data$Puntos1[a+S[b]]
  W2[a,b] <- Data$Puntos2[a+S[b]]
  W3[a,b] <- Data$Puntos3[a+S[b]]
  L1[a,b] <- Wins$Low1[a+S[b]]
  L2[a,b] <- Wins$Low2[a+S[b]]
  L3[a,b] <- Wins$Low3[a+S[b]]
  W0[a,b] <- Wins$W1_0[a+S[b]]
  if (W1[a,b] > 2){ W1[a,b] <- 1} else {W1[a,b] <- 0}
  if (W2[a,b] > 2){ W2[a,b] <- 1} else {W2[a,b] <- 0}
  if (W3[a,b] > 2){ W3[a,b] <- 1} else {W3[a,b] <- 0}}}

P1_Ex <- W1[-c(5,6,7,8),]
P1_J2 <- W2[-c(5,6,7,8),] 
P1_J3 <- W3[-c(5,6,7,8),]
P2_Ex <- W1[-c(1,2,3,4),]
P2_J4 <- W2[-c(1,2,3,4),]
P2_J5 <- W3[-c(1,2,3,4),]
P1_LEx <- L1[-c(5,6,7,8),]
P1_LJ2 <- L2[-c(5,6,7,8),] 
P1_LJ3 <- L3[-c(5,6,7,8),]
P2_LEx <- L1[-c(1,2,3,4),]
P2_LJ4 <- L2[-c(1,2,3,4),]
P2_LJ5 <- L3[-c(1,2,3,4),]

for (a in 1:10){TW_P1Ex[,a] <- sum(P1_Ex[,a])}
for (a in 1:10){TW_P1J2[,a] <- sum(P1_J2[,a])}
for (a in 1:10){TW_P1J3[,a] <- sum(P1_J3[,a])}
for (a in 1:10){TW_P2Ex[,a] <- sum(P2_Ex[,a])}
for (a in 1:10){TW_P2J4[,a] <- sum(P2_J4[,a])}
for (a in 1:10){TW_P2J5[,a] <- sum(P2_J5[,a])}
for (a in 1:10){TL_P1Ex[,a] <- sum(P1_LEx[,a])}
for (a in 1:10){TL_P1J2[,a] <- sum(P1_LJ2[,a])}
for (a in 1:10){TL_P1J3[,a] <- sum(P1_LJ3[,a])}
for (a in 1:10){TL_P2Ex[,a] <- sum(P2_LEx[,a])}
for (a in 1:10){TL_P2J4[,a] <- sum(P2_LJ4[,a])}
for (a in 1:10){TL_P2J5[,a] <- sum(P2_LJ5[,a])}

TW_POthers <- matrix (data=NA, nrow=1, ncol=20)
TW_POthers_x <- matrix (data=NA, nrow=1, ncol=18)
TW_PNoExp <- matrix (data=NA, nrow=1, ncol=20)
TW_PNoExp_x <- matrix (data=NA, nrow=1, ncol=18)
TL_POthers <- matrix (data=NA, nrow=1, ncol=20)
TL_POthers_x <- matrix (data=NA, nrow=1, ncol=18)
TL_PNoExp <- matrix (data=NA, nrow=1, ncol=20)
TL_PNoExp_x <- matrix (data=NA, nrow=1, ncol=18)

for (a in 1:10){
  TW_PNoExp[1,a] <- TW_P2J4[1,a]
  TW_PNoExp[1,a+10] <- TW_P2J5[1,a]}
for (a in 1:10){
  TW_POthers[1,a] <- TW_P1J2[1,a]
  TW_POthers[1,a+10] <- TW_P1J3[1,a]}
for (a in 1:10){
  TL_PNoExp[1,a] <- TL_P2J4[1,a]
  TL_PNoExp[1,a+10] <- TL_P2J5[1,a]}
for (a in 1:10){
  TL_POthers[1,a] <- TL_P1J2[1,a]
  TL_POthers[1,a+10] <- TL_P1J3[1,a]}

############################
# Grupos Sin Participante 3
############################
TW_P1Ex_x <- TW_P1Ex[,-3]
TW_P1J2_x <- TW_P1J2[,-3]
TW_P1J3_x <- TW_P1J3[,-3]
TW_P2Ex_x <- TW_P2Ex[,-3]
TW_P2J4_x <- TW_P2J4[,-3]
TW_P2J5_x <- TW_P2J5[,-3]
TW_POthers_x <- TW_POthers[,-c(3,13)]
TW_PNoExp_x <- TW_PNoExp[,-c(3,13)]
TL_P1Ex_x <- TL_P1Ex[,-3]
TL_P1J2_x <- TL_P1J2[,-3]
TL_P1J3_x <- TL_P1J3[,-3]
TL_P2Ex_x <- TL_P2Ex[,-3]
TL_P2J4_x <- TL_P2J4[,-3]
TL_P2J5_x <- TL_P2J5[,-3]
TL_POthers_x <- TL_POthers[,-c(3,13)]
TL_PNoExp_x <- TL_PNoExp[,-c(3,13)]

#######################################
#######################################
#######################################
# ANOVA's   
#######################################
#######################################
####################
# Periodo 1
####################
P1_Wins<- data.frame(cbind(TW_P1Ex[1,],TW_P1J2[1,],TW_P1J3[1,]))
Wins_P1 <- stack(P1_Wins)
Wins_Juego1 <- aov(values~ind, data=Wins_P1)
P1_Low <- data.frame(cbind(TL_P1Ex[1,], TL_P1J2[1,], TL_P1J3[1,]))
Low_P1 <- stack(P1_Low)
Low_Juego1 <- aov(values~ind, data=Low_P1)
#Sin P1-3
P1_Wins_x<- data.frame(cbind(TW_P1Ex_x,TW_P1J2_x,TW_P1J3_x))
Wins_P1_x <- stack(P1_Wins_x)
Wins_Juego1_x <- aov(values~ind, data=Wins_P1_x)
P1_Low_x <- data.frame(cbind(TL_P1Ex_x, TL_P1J2_x, TL_P1J3_x))
Low_P1_x <- stack(P1_Low_x)
Low_Juego1_x <- aov(values~ind, data=Low_P1_x)
#####################
# Periodo 2
#####################
P2_Wins<- data.frame(cbind(TW_P2Ex[1,],TW_P2J4[1,],TW_P2J5[1,]))
Wins_P2 <- stack(P2_Wins)
Wins_Juego2 <- aov(values~ind, data=Wins_P2)
P2_Low <- data.frame(cbind(TL_P2Ex[1,], TL_P2J4[1,], TL_P2J5[1,]))
Low_P2 <- stack(P2_Low)
Low_Juego2 <- aov(values~ind, data=Low_P2)
#Sin P1-3
P2_Wins_x<- data.frame(cbind(TW_P2Ex_x,TW_P2J4_x,TW_P2J5_x))
Wins_P2_x <- stack(P2_Wins_x)
Wins_Juego2_x <- aov(values~ind, data=Wins_P2_x)
P2_Low_x <- data.frame(cbind(TL_P2Ex_x, TL_P2J4_x, TL_P2J5_x))
Low_P2_x <- stack(P2_Low_x)
Low_Juego2_x <- aov(values~ind, data=Low_P2_x)

#########################################
#########################################
#########################################
#Pruebas T' Complementarias
#########################################
#########################################
####################
# Periodo 1
####################
### 1: Experimentado VS J2
Ex_2<- data.frame(cbind(TW_P1Ex[1,], TW_P1J2[1,]))
Ex_2 <- stack(Ex_2)
Ex_v_J2 <- t.test(values~ind,data=Ex_2,alternative = c("two.sided", "less", "greater"))
#Sin 3
Ex_2_x<- data.frame(cbind(TW_P1Ex_x, TW_P1J2_x))
Ex_2_x <- stack(Ex_2_x)
Ex_v_J2_x <- t.test(values~ind,data=Ex_2_x,alternative = c("two.sided", "less", "greater"))
### 2: Experimentado VS J3
Ex_3<- data.frame(cbind(TW_P1Ex[1,], TW_P1J3[1,]))
Ex_3 <- stack(Ex_3)
Ex_v_J3 <- t.test(values~ind,data=Ex_3,alternative = c("two.sided", "less", "greater"))
#Sin 3
Ex_3_x<- data.frame(cbind(TW_P1Ex_x, TW_P1J3_x))
Ex_3_x <- stack(Ex_3_x)
Ex_v_J3_x <- t.test(values~ind,data=Ex_3_x,alternative = c("two.sided", "less", "greater"))
### 3: J2 vs J3 
J2_3<- data.frame(cbind(TW_P1J2[1,], TW_P1J3[1,]))
J2_3 <- stack(J2_3)
J2_v_J3 <- t.test(values~ind,data=J2_3,alternative = c("two.sided", "less", "greater"))
#Sin 3
J2_3_x<- data.frame(cbind(TW_P1J2_x, TW_P1J3_x))
J2_3_x <- stack(J2_3_x)
J2_v_J3_x <- t.test(values~ind,data=J2_3_x,alternative = c("two.sided", "less", "greater"))


### 1: Experimentado VS J2  LOW
LEx_2<- data.frame(cbind(TL_P1Ex[1,], TL_P1J2[1,]))
LEx_2 <- stack(LEx_2)
LEx_v_J2 <- t.test(values~ind,data=LEx_2, alternative = c("two.sided", "less", "greater"))
#Sin 3
LEx_2_x<- data.frame(cbind(TL_P1Ex_x, TL_P1J2_x))
LEx_2_x <- stack(LEx_2_x)
LEx_v_J2_x <- t.test(values~ind,data=LEx_2_x, alternative = c("two.sided", "less", "greater"))
### 2: Experimentado VS J3
LEx_3<- data.frame(cbind(TL_P1Ex[1,], TL_P1J3[1,]))
LEx_3 <- stack(LEx_3)
LEx_v_J3 <- t.test(values~ind,data=LEx_3, alternative = c("two.sided", "less", "greater"))
#Sin 3
LEx_3_x<- data.frame(cbind(TL_P1Ex_x, TL_P1J3_x))
LEx_3_x <- stack(LEx_3_x)
LEx_v_J3_x <- t.test(values~ind,data=LEx_3_x, alternative = c("two.sided", "less", "greater"))
### 3: J2 vs J3
LJ2_3<- data.frame(cbind(TL_P1J2[1,], TL_P1J3[1,]))
LJ2_3 <- stack(LJ2_3)
LJ2_v_J3 <- t.test(values~ind,data=LJ2_3,alternative = c("two.sided", "less", "greater"))
#Sin 3
LJ2_3_x<- data.frame(cbind(TL_P1J2_x, TL_P1J3_x))
LJ2_3_x <- stack(LJ2_3_x)
LJ2_v_J3_x <- t.test(values~ind,data=LJ2_3_x,alternative = c("two.sided", "less", "greater"))

####################
# Periodo 2
####################
### 1: Experimentado VS J4
Ex_4<- data.frame(cbind(TW_P2Ex[1,], TW_P2J4[1,]))
Ex_4 <- stack(Ex_4)
Ex_v_J4 <- t.test(values~ind,data=Ex_4,alternative = c("greater"))
#Sin 3
Ex_4_x<- data.frame(cbind(TW_P2Ex_x, TW_P2J4_x))
Ex_4_x <- stack(Ex_4_x)
Ex_v_J4_x <- t.test(values~ind,data=Ex_4_x,alternative = c("greater"))
### 2: Experimentado VS J5
Ex_5<- data.frame(cbind(TW_P2Ex[1,], TW_P2J5[1,]))
Ex_5 <- stack(Ex_5)
Ex_v_J5 <- t.test(values~ind,data=Ex_5,alternative = c("greater"))
#Sin 4
Ex_5_x<- data.frame(cbind(TW_P2Ex_x, TW_P2J5_x))
Ex_5_x <- stack(Ex_5_x)
Ex_v_J5_x <- t.test(values~ind,data=Ex_5_x,alternative = c("greater"))
### 3: J4 vs J5 (NO EXPERIENCIA)
J4_5<- data.frame(cbind(TW_P2J4[1,], TW_P2J5[1,]))
J4_5 <- stack(J4_5)
J4_v_J5 <- t.test(values~ind,data=J4_5,alternative = c("two.sided", "less", "greater"))
#Sin 3
J4_5_x<- data.frame(cbind(TW_P2J4_x, TW_P2J5_x))
J4_5_x <- stack(J4_5_x)
J4_v_J5_x <- t.test(values~ind,data=J4_5_x,alternative = c("two.sided", "less", "greater"))

### 1: Experimentado VS J4  LOW
LEx_4<- data.frame(cbind(TL_P2Ex[1,], TL_P2J4[1,]))
LEx_4 <- stack(LEx_4)
LEx_v_J4 <- t.test(values~ind,data=LEx_4, alternative = c("two.sided", "less", "greater"))
#Sin 3
LEx_4_x<- data.frame(cbind(TL_P2Ex_x, TL_P2J4_x))
LEx_4_x <- stack(LEx_4_x)
LEx_v_J4_x <- t.test(values~ind,data=LEx_4_x, alternative = c("two.sided", "less", "greater"))
### 2: Experimentado VS J5
LEx_5<- data.frame(cbind(TL_P2Ex[1,], TL_P2J5[1,]))
LEx_5 <- stack(LEx_5)
LEx_v_J5 <- t.test(values~ind,data=LEx_5, alternative = c("two.sided", "less", "greater"))
#Sin 3
LEx_5_x<- data.frame(cbind(TL_P2Ex_x, TL_P2J5_x))
LEx_5_x <- stack(LEx_5_x)
LEx_v_J5_x <- t.test(values~ind,data=LEx_5_x, alternative = c("two.sided", "less", "greater"))
### 3: J4 vs J5 (NO EXPERIENCIA)
LJ4_5<- data.frame(cbind(TL_P2J4[1,], TL_P2J5[1,]))
LJ4_5 <- stack(LJ4_5)
LJ4_v_J5 <- t.test(values~ind,data=LJ4_5,alternative = c("two.sided", "less", "greater"))
#Sin 3
LJ4_5_x<- data.frame(cbind(TL_P2J4_x, TL_P2J5_x))
LJ4_5_x <- stack(LJ4_5_x)
LJ4_v_J5_x <- t.test(values~ind,data=LJ4_5_x,alternative = c("two.sided", "less", "greater"))

##########################################
##########################################
# Jugador 1 vs Otros (Prueba T)
###################### J1 vs (J4,J5) #####
##########################################
Ex_NoEx<- data.frame(cbind(TW_P2Ex[1,], TW_POthers[1,]))
Ex_NoEx <- stack(Ex_NoEx)
Talento <- t.test(values~ind,data=Ex_NoEx, alternative = c("two.sided", "less", "greater"))
LEx_NoEx<-  data.frame(cbind(TL_P2Ex[1,], TL_POthers[1,]))
LEx_NoEx <- stack(LEx_NoEx)
TalentoLow <- t.test(values~ind,data=LEx_NoEx, alternative = c("two.sided", "less", "greater"))
#Sin 3
Ex_NoEx_x<- data.frame(cbind(TW_P2Ex_x, TW_POthers_x))
Ex_NoEx_x <- stack(Ex_NoEx_x)
Talento_x <- t.test(values~ind,data=Ex_NoEx_x, alternative = c("two.sided", "less", "greater"))
LEx_NoEx_x<-  data.frame(cbind(TL_P2Ex_x, TL_POthers_x))
LEx_NoEx_x <- stack(LEx_NoEx_x)
TalentoLow_x <- t.test(values~ind,data=LEx_NoEx_x, alternative = c("two.sided", "less", "greater"))


##########################################
##########################################
# Experiencia vs No Experiencia (Prueba T)
###################### J1 vs (J4,J5) #####
##########################################
Ex_NoEx<- data.frame(cbind(TW_P2Ex[1,], TW_PNoExp[1,]))
Ex_NoEx <- stack(Ex_NoEx)
Experiencia <- t.test(values~ind,data=Ex_NoEx, alternative = c("greater"))
LEx_NoEx<- data.frame(cbind(TL_P2Ex[1,], TL_PNoExp[1,]))
LEx_NoEx <- stack(LEx_NoEx)
ExperienciaLow <- t.test(values~ind,data=LEx_NoEx, alternative = c("two.sided", "less", "greater"))
#Sin 3
Ex_NoEx_x<- data.frame(cbind(TW_P2Ex_x, TW_PNoExp_x))
Ex_NoEx_x <- stack(Ex_NoEx_x)
Experiencia_x <- t.test(values~ind,data=Ex_NoEx_x, alternative = c("greater"))
LEx_NoEx_x<- data.frame(cbind(TL_P2Ex_x, TL_PNoExp_x))
LEx_NoEx_x <- stack(LEx_NoEx_x)
ExperienciaLow_x <- t.test(values~ind,data=LEx_NoEx_x, alternative = c("two.sided", "less", "greater"))

##########################################
##########################################
##########################################
# Resultados
##########################################
##########################################


###############################
####### JUEGO 1 ###############
###############################

########1 : Analisis con todos los sujetos
##J2 y J3 como Independientes
summary(Wins_Juego1) #ANOVA para las Victorias de cada jugador en el Juego 1
#T's complementarias
J2_v_J3$p.value
Ex_v_J2$p.value
Ex_v_J3$p.value
##Jugador 1 VS Otros (P2yP3)
Talento$p.value
##J2 y J3 como Independientes
summary(Low_Juego1) #ANOVA para las Victorias de cada jugador en el Juego 1
#T's complementarias
LJ2_v_J3$p.value
LEx_v_J2$p.value
LEx_v_J3$p.value
##Jugador 1 VS Otros (P2yP3)
TalentoLow$p.value

########2 : Analisis - Participante 3
##J2 y J3 como Independientes
summary(Wins_Juego1_x) #ANOVA para las Victorias de cada jugador en el Juego 1
J2_v_J3_x$p.value
Ex_v_J2_x$p.value
Ex_v_J3_x$p.value
#Jugador 1 VS Otros (P2yP3)
Talento_x$p.value
##J2 y J3 como Independientes
summary(Low_Juego1_x) #ANOVA para las Victorias de cada jugador en el Juego 1
#T's complementarias
LJ2_v_J3_x$p.value
LEx_v_J2_x$p.value
LEx_v_J3_x$p.value
##Jugador 1 VS Otros (P2yP3)
TalentoLow_x$p.value



###############################
####### JUEGO 2 ###############
###############################

########1 : Analisis con todos los sujetos
##J4 y J5 como Independientes
summary(Wins_Juego2) #ANOVA para las Victorias de cada jugador en el Juego 2
#T's complementarias
J4_v_J5$p.value
Ex_v_J4$p.value
Ex_v_J5$p.value
##Experiencia VS No Experiencia (P4yP5)
Experiencia$p.value
##J4 y J5 como Independientes
summary(Low_Juego2) #ANOVA para las Victorias de cada jugador en el Juego 2
#T's complementarias
LJ4_v_J5$p.value
LEx_v_J4$p.value
LEx_v_J5$p.value
##Experiencia VS No Experiencia (P4yP5)
ExperienciaLow$p.value

########2 : Analisis - Participante 3
##J4 y J5 como Independientes
summary(Wins_Juego2_x) #ANOVA para las Victorias de cada jugador en el Juego 2
J4_v_J5_x$p.value
Ex_v_J4_x$p.value
Ex_v_J5_x$p.value
#Experiencia vs No Experiencia
Experiencia_x$p.value
##J4 y J5 como Independientes
summary(Low_Juego2_x) #ANOVA para las Victorias de cada jugador en el Juego 2
#T's complementarias
LJ4_v_J5_x$p.value
LEx_v_J4_x$p.value
LEx_v_J5_x$p.value
##Experiencia VS No Experiencia (P4yP5)
ExperienciaLow_x$p.value