rm(list=ls())
#setwd("D:/APLICACION/EXP2")
setwd("C:/Users/Adriana/Desktop/Jaime_Lab25/Datos")
dir()
library("grDevices", lib.loc="C:/Program Files/R/R-3.2.2/library")
Data <- read.csv("Datac.csv")


####################################################
####################################################
# Preparando los datos
####################################################

#### Empty Data Matrix ####
C1 <- matrix(data=NA, nrow=8, ncol=10)     #ChosenNumber by Player1 each Period(row) and Sesion(col)
B1 <- matrix(data=NA, nrow=8, ncol=20)    #RawBeliefs by Player1 each Period(row) and Sesion(col)
BO1 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of Others by Player1 each Period(row) and Sesion(col)
BOP1 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of Others*P by Player1 each Period(row) and Sesion(col)
BA1 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of All by Player1 each Period(row) and Sesion(col)
BAP1 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of All*P by Player1 each Period(row) and Sesion(col)
DO1 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in Others by Player1 each Period(row) and Sesion(col)
DOP1 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in Others*P by Player1 each Period(row) and Sesion(col)
DA1 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in All by Player1 each Period(row) and Sesion(col)
DAP1 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in All*P by Player1 each Period(row) and Sesion(col)

#### Filling our Matrix ####
S <- c(0,8,16,24,32,40,48,56,64,72)
c <- c(0,1,2,3,4,5,6,7,8,9)
d <- c(1,2,3,4,5,6,7,8,9,10)

for (a in 1:8){ for (b in 1:10){
  C1[a,b] <- Data$EA[a+S[b]]

  B1[a,b+c[b]] <- Data$CA1[a+S[b]]
  B1[a,b+d[b]] <- Data$CA2[a+S[b]]

  BO1[a,b] <- mean(c(B1[a,b+c[b]],B1[a,b+d[b]]))
  BOP1[a,b] <- (mean(c(B1[a,b+c[b]],B1[a,b+d[b]]))) * (2/3)

  BA1[a,b] <- mean(c(B1[a,b+c[b]],B1[a,b+d[b]],C1[a,b]))
  BAP1[a,b] <- (mean(c(B1[a,b+c[b]],B1[a,b+d[b]],C1[a,b]))) * (2/3)

  DO1[a,b] <- BO1[a,b] - C1[a,b]
  DOP1[a,b] <- BOP1[a,b] - C1[a,b]

  DA1[a,b] <- BA1[a,b] - C1[a,b]
  DAP1[a,b] <- BAP1[a,b] - C1[a,b]
  }}

#### Plot Matrix ####

BarO <- matrix(data=NA, nrow=8, ncol=30)
BarOP <- matrix(data=NA, nrow=8, ncol=30)
BarA <- matrix(data=NA, nrow=8, ncol=30)
BarAP <- matrix(data=NA, nrow=8, ncol=30)

for (a in 1:8){ for (b in 1:10) {
  e <- c(0,2,4,6,8,10,12,14,16,18)
  BarO[a,b+e[b]] <- DO1[a,b]
  BarOP[a,b+e[b]] <- DOP1[a,b]
  BarA[a,b+e[b]] <- DA1[a,b]
  BarAP[a,b+e[b]] <- DAP1[a,b]}}

##############################################
#Verificando Efecto Reset en Tiradas y Creencias
##############################################
##############################################

#Arreglos sin participante ruidoso
C1_x <- C1[,-3]        #Elecciones P1 excepto No.3
B1_x <- B1[,-c(5,6)]   #Creencias Crudas sobre P2 y P3; excepto para el P1-3
BO1_x <- BO1[,-3]      #Creencias Promedio sobre P2 y P3; excepto para el P1-3
BOP1_x <- BOP1[,-3]    #Creencias Promedio P2-P3 * P  (Sin P1-3)
BA1_x <- BA1[,-3]      #Creencias Promedio P2-P3 Y P1 (Excepto para P1-3)
BAP1_x <- BAP1[,-3]    #Creencias Promedio P2-P3 y P1 * P (Excepto para P1-3)

#C1 = Reset en Elecciones
Reset_ch<- data.frame(cbind(C1[4,], C1[5,]))
choices_r <- stack(Reset_ch)
Reset_choices <- t.test(values~ind,data=choices_r,alternative = c("less"))
#Reset en Elecciones SIN soquete
Reset_ch<- data.frame(cbind(C1_x[4,], C1_x[5,]))
choices_r <- stack(Reset_ch)
Reset_choices_x <- t.test(values~ind,data=choices_r,alternative = c("less"))

#B1 = Reset en las Creencias Crudas del PArticipante 1 
Reset_bel<- data.frame(cbind(B1[4,], B1[5,]))
belief_r <- stack(Reset_bel)
Raw_beliefs <- t.test(values~ind,data=belief_r,alternative = c("less"))
#Reset en las Creencias SIN soquete
Reset_bel<- data.frame(cbind(B1_x[4,], B1_x[5,]))
belief_r <- stack(Reset_bel)
Raw_beliefs_x <- t.test(values~ind,data=belief_r,alternative = c("less"))

#BO1 = Reset en el Promedio de las Creencias de P2 y P3 por el Participante 1
Reset_bel_2<- data.frame(cbind(BO1[4,], BO1[5,]))
belief_rmean <- stack(Reset_bel_2)
Mean_Belief <- t.test(values~ind,data=belief_rmean,alternative = c("less"))
#Reset en Promedio de Creencias P2 y P3 sin P1-3
Reset_bel_2<- data.frame(cbind(BO1_x[4,], BO1_x[5,]))
belief_rmean <- stack(Reset_bel_2)
Mean_Belief_x <- t.test(values~ind,data=belief_rmean,alternative = c("less"))

#BOP1 = Reset en Creencias PRomedio sobre P1 y P2 *P
Reset_belOthersP<- data.frame(cbind(BOP1[4,], BOP1[5,]))
belief_OthersP <- stack(Reset_belOthersP)
Others_BeliefP <- t.test(values~ind,data=belief_OthersP,alternative = c("less"))
#Sin P1-3
Reset_belOthersP<- data.frame(cbind(BOP1_x[4,], BOP1_x[5,]))
belief_OthersP <- stack(Reset_belOthersP)
Others_BeliefP_x <- t.test(values~ind,data=belief_OthersP,alternative = c("less"))

#BA1 = Reset en el Promedio TOTAL (P2, P3... y P1)
Reset_belAll<- data.frame(cbind(BA1[4,], BA1[5,]))
belief_All <- stack(Reset_belAll)
All_Beliefs <- t.test(values~ind,data=belief_All,alternative = c("less"))
#Sin P1-3
Reset_belAll<- data.frame(cbind(BA1_x[4,], BA1_x[5,]))
belief_All <- stack(Reset_belAll)
All_Beliefs_x <- t.test(values~ind,data=belief_All,alternative = c("less"))

#BAP1 = Reset en el Promedio TOTAL * P
Reset_belAllP<- data.frame(cbind(BAP1[4,], BAP1[5,]))
belief_AllP <- stack(Reset_belAllP)
AllP_Beliefs <- t.test(values~ind,data=belief_AllP,alternative = c("less"))
#Sin P1-3
Reset_belAllP<- data.frame(cbind(BAP1_x[4,], BAP1_x[5,]))
belief_AllP <- stack(Reset_belAllP)
AllP_Beliefs_x <- t.test(values~ind,data=belief_AllP,alternative = c("less"))


#########################################
##### Resumen de las T-Test
#########################################

#T-Test for Reset Effect (Total)
Reset_choices$p.value           #Reset en Elecciones P1
Raw_beliefs$p.value             #Reset en Creencias Crudas P2-P3    
Mean_Belief$p.value             #Reset en Promedio Creencias P2-P3
Others_BeliefP$p.value          #Promedio P2-P3 * P
All_Beliefs$p.value             #Promedio P2-P3 Y P1
AllP_Beliefs$p.value            #Promedio P2-P3 y P1 * P
DiffOthers_Reset$p.value        #Diferencia (P2-P3) - P1

#T-Test for the Reset Effect (Without Participant 3)
Reset_choices_x$p.value         #Reset en Elecciones P1
Raw_beliefs_x$p.value           #Reset en Creencias Crudas P2-P3
Mean_Belief_x$p.value           #Reset en Promedio Creencias P2-P3
Others_BeliefP_x$p.value        #Promedio P2-P3 * P
All_Beliefs_x$p.value           #Promedio P2-P3 y P1
AllP_Beliefs_x$p.value          #Promedio P2-P3 y p1 * P
DiffOthers_Reset_x$p.value      #Diferencia (P2-P3) - P1 


###############################################################
###############################################################
# # Evaluando cambios en Consistencia con los Nuevos Jugadores
# # ¿Reset en las Diferencias Belief - Choice ?
###############################################################
###############################################################

##### Arreglos sin participante Horrible
DO1_x <- DO1[,-3]
DOP1_x <- DOP1[,-3]
DA1_x <- DA1[,-3]
DAP1_x <- DAP1[,-3]


#DO1 - Reset en la Diferencia (P2-P3) - ChoiceP1
Reset_DifOthers<- data.frame(cbind(DO1[4,], DO1[5,]))
Diff_Others <- stack(Reset_DifOthers)
DiffOthers_Reset <- t.test(values~ind,data=Diff_Others)
DiffOthers_Reset_side <- t.test(values~ind,data=Diff_Others, alternative = c("greater"))
#Sin P1-3
Reset_DifOthers<- data.frame(cbind(DO1_x[4,], DO1_x[5,]))
Diff_Others_x <- stack(Reset_DifOthers)
DiffOthers_Reset_x <- t.test(values~ind,data=Diff_Others_x)  #,alternative = c("greater")
DiffOthers_Reset_side_x <- t.test(values~ind,data=Diff_Others_x, alternative = c("greater"))

#DOP1 = Reset en la Diferencia (P2-P3)*P - ChoiceP1
Reset_DifOthersP<- data.frame(cbind(DOP1[4,], DOP1[5,]))
Diff_OthersP <- stack(Reset_DifOthersP)
DiffOthersP_Reset <- t.test(values~ind,data=Diff_OthersP)
DiffOthersP_Reset_side <- t.test(values~ind,data=Diff_OthersP, alternative = c("greater"))
#Sin P1-3
Reset_DifOthersP<- data.frame(cbind(DOP1_x[4,], DOP1_x[5,]))
Diff_OthersP <- stack(Reset_DifOthersP)
DiffOthersP_Reset_x <- t.test(values~ind,data=Diff_OthersP)
DiffOthersP_Reset_side_x <- t.test(values~ind,data=Diff_OthersP, alternative = c("greater"))

#DA1 = Diferencias (P1,P2,P3) - ChoiceP1
Rst_DifAll<- data.frame(cbind(DA1[4,], DA1[5,]))
Diff_All <- stack(Rst_DifAll)
DiffAll_Rst <- t.test(values~ind,data=Diff_All)
DiffAll_Rst_side <- t.test(values~ind,data=Diff_All, alternative = c("greater"))
#Sin P1-3
Rst_DifAll<- data.frame(cbind(DA1_x[4,], DA1_x[5,]))
Diff_All <- stack(Rst_DifAll)
DiffAll_Rst_x <- t.test(values~ind,data=Diff_All)
DiffAll_Rst_side_x <- t.test(values~ind,data=Diff_All, alternative = c("greater"))


#DAP1 = Duferencias (P1,P2,P3)*P - Choice P1
Rst_DifAllP<- data.frame(cbind(DAP1[4,], DAP1[5,]))
Diff_AllP <- stack(Rst_DifAllP)
DiffAllP_Rst <- t.test(values~ind,data=Diff_AllP)
DiffAllP_Rst_side <- t.test(values~ind,data=Diff_AllP, alternative = c("greater"))
#Sin P1-3
Rst_DifAllP<- data.frame(cbind(DAP1_x[4,], DAP1_x[5,]))
Diff_AllP <- stack(Rst_DifAllP)
DiffAllP_Rst_x <- t.test(values~ind,data=Diff_AllP)
DiffAllP_Rst_side_x <- t.test(values~ind,data=Diff_AllP, alternative = c("greater"))



###############################################
# Resumen Diferencias en Consistencia (Reset?)
###############################################

#########Todos
c(DiffOthers_Reset$p.value, DiffOthers_Reset_side$p.value)
c(DiffOthersP_Reset$p.value, DiffOthersP_Reset_side$p.value)
c(DiffAll_Rst$p.value, DiffAll_Rst_side$p.value)
c(DiffAllP_Rst$p.value, DiffAllP_Rst_side$p.value)

#########Sin Participante 3
c(DiffOthers_Reset_x$p.value, DiffOthers_Reset_side_x$p.value)
c(DiffOthersP_Reset_x$p.value, DiffOthersP_Reset_side_x$p.value)
c(DiffAll_Rst_x$p.value, DiffAll_Rst_side_x$p.value)
c(DiffAllP_Rst_x$p.value, DiffAllP_Rst_side_x$p.value)