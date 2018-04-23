rm(list=ls())
#setwd("C:/Users/Laboratorio25_2/Desktop/Jaime_Lab25/Datos")
setwd("C:/Users/Alejandro/Desktop/Jaime_Lab25/Datos")
library("grDevices", lib.loc="C:/Program Files/R/R-3.1.1/library")
Data <- read.csv("Datac.csv")
Wins <- read.csv("Datos_Wins.csv")
#### Data Matrix ####

C1 <- matrix(data=NA, nrow=8, ncol=10)      #ChosenNumber by Player1 each Period(row) and Sesion(col)
C2 <- matrix(data=NA, nrow=8, ncol=10)      #ChosenNumber by Player2 each Period(row) and Sesion(col)
C3 <- matrix(data=NA, nrow=8, ncol=10)      #ChosenNumber by Player3 each Period(row) and Sesion(col)
MC1 <- matrix(data=NA, nrow=8, ncol=1)      #Average ChosenNumber by Player1 each Period(row) and Sesion(col)
MC23 <- matrix(data=NA, nrow=8, ncol=1)     #Average ChosenNumber by Player2&3 each Period(row) and Sesion(col)
MC123 <- matrix(data=NA, nrow=8, ncol=1)    #Average ChosenNumber by All players each Period(row) and Sesion(col)

B1 <- matrix(data=NA, nrow=8, ncol=20)    #RawBeliefs by Player1 each Period(row) and Sesion(col)
B2 <- matrix(data=NA, nrow=8, ncol=20)    #RawBeliefs by Player2 each Period(row) and Sesion(col)
B3 <- matrix(data=NA, nrow=8, ncol=20)    #RawBeliefs by Player3 each Period(row) and Sesion(col)

W1 <- matrix(data=NA, nrow=8, ncol=10)     #IsWinner by Player1 each Period(row) and Sesion(col)
W2 <- matrix(data=NA, nrow=8, ncol=10)     #IsWinner by Player2 each Period(row) and Sesion(col)
W3 <- matrix(data=NA, nrow=8, ncol=10)     #IsWinner by Player3 each Period(row) and Sesion(col)

L1 <- matrix(data=NA, nrow=8, ncol=10)     #GaveLowestNumber by Player 1 each Period(row) and Sesion(col)
L2 <- matrix(data=NA, nrow=8, ncol=10)     #GaveLowestNumber by Player 2 each Period(row) and Sesion(col)
L3 <- matrix(data=NA, nrow=8, ncol=10)     #GaveLowestNumber by Player 3 each Period(row) and Sesion(col)

TN <- matrix(data=NA, nrow=8, ncol=10)     #TargetNumber each Period(row) and Sesion(col)

W0 <- matrix(data=NA, nrow=8, ncol=10) #Would Player 1 have won on the previous game by choosing the number on this same trial

BO1 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of Others by Player1 each Period(row) and Sesion(col)
BO2 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of Others by Player2 each Period(row) and Sesion(col)
BO3 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of Others by Player3 each Period(row) and Sesion(col)
MBO1 <- matrix(data=NA, nrow=8, ncol=1)    #Mean Average MB of Others by Player1 each Period(row) and Sesion(col)
MBO23 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Average MB of Others by Player2&3 each Period(row) and Sesion(col)

BOP1 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of Others*P by Player1 each Period(row) and Sesion(col)
BOP2 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of Others*P by Player2 each Period(row) and Sesion(col)
BOP3 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of Others*P by Player3 each Period(row) and Sesion(col)
MBOP1 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Average MB of Others*P by Player1 each Period(row) and Sesion(col)
MBOP23 <- matrix(data=NA, nrow=8, ncol=1)  #Mean Average MB of Others*P by Player2&3 each Period(row) and Sesion(col)

BA1 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of All by Player1 each Period(row) and Sesion(col)
BA2 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of All by Player2 each Period(row) and Sesion(col)
BA3 <- matrix(data=NA, nrow=8, ncol=10)    #MeanBeliefs of All by Player3 each Period(row) and Sesion(col)
MBA1 <- matrix(data=NA, nrow=8, ncol=1)    #Mean Average MB of All by Player1 each Period(row) and Sesion(col)
MBA23 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Average MB of All by Player2&3 each Period(row) and Sesion(col)

BAP1 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of All*P by Player1 each Period(row) and Sesion(col)
BAP2 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of All*P by Player2 each Period(row) and Sesion(col)
BAP3 <- matrix(data=NA, nrow=8, ncol=10)   #MeanBeliefs of All*P by Player3 each Period(row) and Sesion(col)
MBAP1 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Average MB of All*P by Player1 each Period(row) and Sesion(col)
MBAP23 <- matrix(data=NA, nrow=8, ncol=1)  #Mean Average MB of All*P by Player2&3 each Period(row) and Sesion(col)

DO1 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in Others by Player1 each Period(row) and Sesion(col)
DO2 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in Others by Player2 each Period(row) and Sesion(col)
DO3 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in Others by Player3 each Period(row) and Sesion(col)
MDO1 <- matrix(data=NA, nrow=8, ncol=1)    #Mean Differences in Others by Player1 each Period(row) and Sesion(col)
MDO23 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Differences in Others by Player2&3 each Period(row) and Sesion(col)

DOP1 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in Others*P by Player1 each Period(row) and Sesion(col)
DOP2 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in Others*P by Player2 each Period(row) and Sesion(col)
DOP3 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in Others*P by Player3 each Period(row) and Sesion(col)
MDOP1 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Differences in Others*P by Player1 each Period(row) and Sesion(col)
MDOP23 <- matrix(data=NA, nrow=8, ncol=1)  #Mean Differences in Others*P by Player2&3 each Period(row) and Sesion(col)

DA1 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in All by Player1 each Period(row) and Sesion(col)
DA2 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in All by Player2 each Period(row) and Sesion(col)
DA3 <- matrix(data=NA, nrow=8, ncol=10)    #Differences in All by Player3 each Period(row) and Sesion(col)
MDA1 <- matrix(data=NA, nrow=8, ncol=1)    #Mean Differences in All by Player1 each Period(row) and Sesion(col)
MDA23 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Differences in All by Player2&3 each Period(row) and Sesion(col)

DAP1 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in All*P by Player1 each Period(row) and Sesion(col)
DAP2 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in All*P by Player2 each Period(row) and Sesion(col)
DAP3 <- matrix(data=NA, nrow=8, ncol=10)   #Differences in All*P by Player3 each Period(row) and Sesion(col)
MDAP1 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Differences in All*P by Player1 each Period(row) and Sesion(col)
MDAP23 <- matrix(data=NA, nrow=8, ncol=1)  #Mean Differences in All*P by Player2&3 each Period(row) and Sesion(col)

SD1 <- matrix(data=NA, nrow=8, ncol=10)    #Smaller Diff by Player1 each Period(row) and Sesion(col)
SD2 <- matrix(data=NA, nrow=8, ncol=10)    #Smaller Diff by Player2 each Period(row) and Sesion(col)
SD3 <- matrix(data=NA, nrow=8, ncol=10)    #Smaller Diff by Player3 each Period(row) and Sesion(col)
MSD1 <- matrix(data=NA, nrow=8, ncol=1)    #Mean Smaller Diff by Player1 each Period(row) and Sesion(col)
MSD23 <- matrix(data=NA, nrow=8, ncol=1)   #Mean Smaller Diff by Player2&3 each Period(row) and Sesion(col)

#CSD1 <- matrix(data=NA, nrow=8, ncol=1)    #Comparing Smaller Difference with each Mean Difference
#CSD23 <- matrix(data=NA, nrow=8, ncol=1)    #Comparing Smaller Difference with each Mean Difference

#### Matrix Values ####

S <- c(0,8,16,24,32,40,48,56,64,72)
c <- c(0,1,2,3,4,5,6,7,8,9)
d <- c(1,2,3,4,5,6,7,8,9,10)

for (a in 1:8){ for (b in 1:10){
C1[a,b] <- Data$EA[a+S[b]]
C2[a,b] <- Data$EB[a+S[b]]
C3[a,b] <- Data$EC[a+S[b]]

B1[a,b+c[b]] <- Data$CA1[a+S[b]]
B1[a,b+d[b]] <- Data$CA2[a+S[b]]
B2[a,b+c[b]] <- Data$CB1[a+S[b]]
B2[a,b+d[b]] <- Data$CB2[a+S[b]]
B3[a,b+c[b]] <- Data$CC1[a+S[b]]
B3[a,b+d[b]] <- Data$CC2[a+S[b]]

W1[a,b] <- Data$Puntos1[a+S[b]]
W2[a,b] <- Data$Puntos2[a+S[b]]
W3[a,b] <- Data$Puntos3[a+S[b]]

if (W1[a,b] > 2){ W1[a,b] <- 1} else {W1[a,b] <- 0}
if (W2[a,b] > 2){ W2[a,b] <- 1} else {W2[a,b] <- 0}
if (W3[a,b] > 2){ W3[a,b] <- 1} else {W3[a,b] <- 0}

L1[a,b] <- Wins$Low1[a+S[b]]
L2[a,b] <- Wins$Low2[a+S[b]]
L3[a,b] <- Wins$Low3[a+S[b]]

TN[a,b] <- Data$Objetivo[a+S[b]]

W0[a,b] <- Wins$W1_0[a+S[b]]

BO1[a,b] <- mean(c(B1[a,b+c[b]],B1[a,b+d[b]]))
BO2[a,b] <- mean(c(B2[a,b+c[b]],B2[a,b+d[b]]))
BO3[a,b] <- mean(c(B3[a,b+c[b]],B3[a,b+d[b]]))

BOP1[a,b] <- (mean(c(B1[a,b+c[b]],B1[a,b+d[b]]))) * (2/3)
BOP2[a,b] <- (mean(c(B2[a,b+c[b]],B2[a,b+d[b]]))) * (2/3)
BOP3[a,b] <- (mean(c(B3[a,b+c[b]],B3[a,b+d[b]]))) * (2/3)

BA1[a,b] <- mean(c(B1[a,b+c[b]],B1[a,b+d[b]],C1[a,b]))
BA2[a,b] <- mean(c(B2[a,b+c[b]],B2[a,b+d[b]],C2[a,b]))
BA3[a,b] <- mean(c(B3[a,b+c[b]],B3[a,b+d[b]],C3[a,b]))

BAP1[a,b] <- (mean(c(B1[a,b+c[b]],B1[a,b+d[b]],C1[a,b]))) * (2/3)
BAP2[a,b] <- (mean(c(B2[a,b+c[b]],B2[a,b+d[b]],C2[a,b]))) * (2/3)
BAP3[a,b] <- (mean(c(B3[a,b+c[b]],B3[a,b+d[b]],C3[a,b]))) * (2/3)

DO1[a,b] <- C1[a,b] - BO1[a,b]
DO2[a,b] <- C2[a,b] - BO2[a,b]
DO3[a,b] <- C3[a,b] - BO3[a,b]

DOP1[a,b] <- C1[a,b] - BOP1[a,b]
DOP2[a,b] <- C2[a,b] - BOP2[a,b]
DOP3[a,b] <- C3[a,b] - BOP3[a,b]

DA1[a,b] <- C1[a,b] - BA1[a,b]
DA2[a,b] <- C2[a,b] - BA2[a,b]
DA3[a,b] <- C3[a,b] - BA3[a,b]

DAP1[a,b] <- C1[a,b] - BAP1[a,b]
DAP2[a,b] <- C2[a,b] - BAP2[a,b]
DAP3[a,b] <- C3[a,b] - BAP3[a,b]

D1 <- sort(abs(c(DO1[a,b],DOP1[a,b],DA1[a,b],DAP1[a,b])))
D2 <- sort(abs(c(DO2[a,b],DOP2[a,b],DA2[a,b],DAP2[a,b])))
D3 <- sort(abs(c(DO3[a,b],DOP3[a,b],DA3[a,b],DAP3[a,b])))

SD1[a,b] <- D1[1]
SD2[a,b] <- D2[1]
SD3[a,b] <- D3[1]

MC1[a,1] <- mean(C1[a,])
MC23[a,1] <- mean(c(C2[a,],C3[a,]))
MC123[a,1] <- mean(c(C1[a,],C2[a,],C3[a,]))

MBO1[a,1] <- mean(BO1[a,])
MBO23[a,1] <- mean(c(BO2[a,],BO3[a,]))

MBOP1[a,1] <- mean(BOP1[a,])
MBOP23[a,1] <- mean(c(BOP2[a,],BOP3[a,]))

MBA1[a,1] <- mean(BA1[a,])
MBA23[a,1] <- mean(c(BA2[a,],BA3[a,]))

MBAP1[a,1] <- mean(BAP1[a,])
MBAP23[a,1] <- mean(c(BAP2[a,],BAP3[a,]))

MDO1[a,1] <- mean(DO1[a,])
MDO23[a,1] <- mean(c(DO2[a,],DO3[a,]))

MDOP1[a,1] <- mean(DOP1[a,])
MDOP23[a,1] <- mean(c(DOP2[a,],DOP3[a,]))

MDA1[a,1] <- mean(DA1[a,])
MDA23[a,1] <- mean(c(DA2[a,],DA3[a,]))

MDAP1[a,1] <- mean(DAP1[a,])
MDAP23[a,1] <- mean(c(DAP2[a,],DAP3[a,]))

MSD1[a,1] <- mean(SD1[a,])
MSD23[a,1] <- mean(c(SD2[a,],SD3[a,]))
}}

#for (a in 1:8){
#MD1 <- sort(abs(c(MDO1[a,1],MDOP1[a,1],MDA1[a,1],MDAP1[a,1])))
#MD23 <- sort(abs(c(MDO23[a,1],MDOP23[a,1],MDA23[a,1],MDAP23[a,1])))

#CSD1[a,1] <- MD1[1]
#CSD23[a,1] <- MD23[1]
#}

#### Plot Matrix ####

BarO <- matrix(data=NA, nrow=8, ncol=30)
BarOP <- matrix(data=NA, nrow=8, ncol=30)
BarA <- matrix(data=NA, nrow=8, ncol=30)
BarAP <- matrix(data=NA, nrow=8, ncol=30)

MBarO <- matrix(data=NA, nrow=8, ncol=2)
MBarOP <- matrix(data=NA, nrow=8, ncol=2)
MBarA <- matrix(data=NA, nrow=8, ncol=2)
MBarAP <- matrix(data=NA, nrow=8, ncol=2)

for (a in 1:8){ for (b in 1:10) {
  e <- c(0,2,4,6,8,10,12,14,16,18)
  BarO[a,b+e[b]] <- DO1[a,b]
  BarOP[a,b+e[b]] <- DOP1[a,b]
  BarA[a,b+e[b]] <- DA1[a,b]
  BarAP[a,b+e[b]] <- DAP1[a,b]
  e = e + 1
  BarO[a,b+e[b]] <- DO2[a,b]
  BarOP[a,b+e[b]] <- DOP2[a,b]
  BarA[a,b+e[b]] <- DA2[a,b]
  BarAP[a,b+e[b]] <- DAP2[a,b]
  e = e + 1
  BarO[a,b+e[b]] <- DO3[a,b]
  BarOP[a,b+e[b]] <- DOP3[a,b]
  BarA[a,b+e[b]] <- DA3[a,b]
  BarAP[a,b+e[b]] <- DAP3[a,b]
}}

MBarO[,1] <- MDO1
MBarO[,2] <- MDO23
MBarOP[,1] <- MDOP1
MBarOP[,2] <- MDOP23
MBarA[,1] <- MDA1
MBarA[,2] <- MDA23
MBarAP[,1] <- MDAP1
MBarAP[,2] <- MDAP23


#########################################
#########################################
############ MATRICES SIN JUGADOR 3

C1_x <- C1[,-3]
C2_x <- C2[,-3]
C3_x <- C3[,-3]

B1_x <- B1[,-c(5,6)]
B2_x <- B2[,-c(5,6)]
B3_x <- B3[,-c(5,6)]

W1_x <- W1[,-3]
W2_x <- W2[,-3]
W3_x <- W3[,-3]

L1_x <- L1[,-3]
L2_x <- L2[,-3]
L3_x <- L3[,-3]

TN_x <- TN[,-3]

W0_x <- W0[,-3]

BO1_x <- BO1[,-3]
BO2_x <- BO2[,-3]
BO3_x <- BO3[,-3]

BOP1_x <- BOP1[,-3]
BOP2_x <- BOP2[,-3]
BOP3_x <- BOP3[,-3]

BA1_x <- BA1[,-3]
BA2_x <- BA2[,-3]
BA3_x <- BA3[,-3]

BAP1_x <- BAP1[,-3]
BAP2_x <- BAP2[,-3]
BAP3_x <- BAP3[,-3]

DO1_x <- DO1[,-3]
DO2_x <- DO2[,-3]
DO3_x <- DO3[,-3]

DOP1_x <- DOP1[,-3]
DOP2_x <- DOP2[,-3]
DOP3_x <- DOP3[,-3]

DA1_x <- DA1[,-3]
DA2_x <- DA2[,-3]
DA3_x <- DA3[,-3]

DAP1_x <- DAP1[,-3]
DAP2_x <- DAP2[,-3]
DAP3_x <- DAP3[,-3]

SD1_x <- SD1[,-3]
SD2_x <- SD2[,-3]
SD3_x <- SD3[,-3]
