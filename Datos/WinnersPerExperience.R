rm(list=ls())
setwd("C:/Users/Alejandro/Desktop/Jaime_Lab25/Datos")
library("grDevices", lib.loc="C:/Program Files/R/R-3.1.1/library")
Data <- read.csv("Datac.csv")

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

#### Matrix Values ####
S <- c(0,8,16,24,32,40,48,56,64,72)
c <- c(0,1,2,3,4,5,6,7,8,9)
d <- c(1,2,3,4,5,6,7,8,9,10)

for (a in 1:8){ for (b in 1:10){
  W1[a,b] <- Data$Puntos1[a+S[b]]
  W2[a,b] <- Data$Puntos2[a+S[b]]
  W3[a,b] <- Data$Puntos3[a+S[b]]
  if (W1[a,b] > 2){ W1[a,b] <- 1} else {W1[a,b] <- 0}
  if (W2[a,b] > 2){ W2[a,b] <- 1} else {W2[a,b] <- 0}
  if (W3[a,b] > 2){ W3[a,b] <- 1} else {W3[a,b] <- 0}}}

P1_Ex <- W1[-c(5,6,7,8),]
P1_J2 <- W2[-c(5,6,7,8),] 
P1_J3 <- W3[-c(5,6,7,8),]
P2_Ex <- W1[-c(1,2,3,4),]
P2_J4 <- W2[-c(1,2,3,4),]
P2_J5 <- W3[-c(1,2,3,4),]

for (a in 1:10){TW_P1Ex[,a] <- sum(P1_Ex[,a])}
for (a in 1:10){TW_P1J2[,a] <- sum(P1_J2[,a])}
for (a in 1:10){TW_P1J3[,a] <- sum(P1_J3[,a])}
for (a in 1:10){TW_P2Ex[,a] <- sum(P2_Ex[,a])}
for (a in 1:10){TW_P2J4[,a] <- sum(P2_J4[,a])}
for (a in 1:10){TW_P2J5[,a] <- sum(P2_J5[,a])}

TW_POthers <- matrix (data=NA, nrow=1, ncol=20)
TW_POthers_x <- matrix (data=NA, nrow=1, ncol=18)

for (a in 1:10){
  TW_POthers[1,a] <- TW_P2J4[1,a]
  TW_POthers[1,a+10] <- TW_P2J5[1,a]}

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



#######################################
#######################################
# ANOVA's   Periodo 2 : Ex vs J4 vs J5
#######################################
P2_Wins<- data.frame(cbind(TW_P2Ex[1,],TW_P2J4[1,],TW_P2J5[1,]))
Wins_P2 <- stack(P2_Wins)
Wins_Juego2 <- aov(values~ind, data=Wins_P2)
#Sin P1-3
P2_Wins_x<- data.frame(cbind(TW_P2Ex_x,TW_P2J4_x,TW_P2J5_x))
Wins_P2_x <- stack(P2_Wins_x)
Wins_Juego2_x <- aov(values~ind, data=Wins_P2_x)
summary(Wins_Juego2_x) #SIN PARTICIPANTE 3

#########################################
#Pruebas T' Complementarias
#########################################

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



##########################################
##########################################
# Experiencia vs No Experiencia (Prueba T)
###################### J1 vs (J4,J5) #####
##########################################
Ex_NoEx<- data.frame(cbind(TW_P2Ex[1,], TW_POthers[1,]))
Ex_NoEx <- stack(Ex_NoEx)
Experiencia <- t.test(values~ind,data=Ex_NoEx, alternative = c("greater"))
#Sin 3
Ex_NoEx_x<- data.frame(cbind(TW_P2Ex_x, TW_POthers_x))
Ex_NoEx_x <- stack(Ex_NoEx_x)
Experiencia_x <- t.test(values~ind,data=Ex_NoEx_x, alternative = c("greater"))


##########################################
##########################################
# Resultados
##########################################

########1 : Analisis con todos los sujetos
##J4 y J5 como Independientes
summary(Wins_Juego2) #ANOVA para las Victorias de cada jugador en el Juego 2
#T's complementarias
J4_v_J5$p.value
Ex_v_J4$p.value
Ex_v_J5$p.value
##Experiencia VS No Experiencia (P4yP5)
Experiencia$p.value

########2 : Analisis - Participante 3
##J4 y J5 como Independientes
summary(Wins_Juego2_x) #ANOVA para las Victorias de cada jugador en el Juego 2
J4_v_J5_x$p.value
Ex_v_J4_x$p.value
Ex_v_J5_x$p.value
#Experiencia vs No Experiencia
Experiencia_x$p.value