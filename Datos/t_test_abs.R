##### En esta versión todas las diferencias usan valores absolutos

DO1 <- abs(DO1)
DO2 <- abs(DO2)
DO3 <- abs(DO3)
DOP1 <- abs(DOP1)
DOP2 <- abs(DOP2)
DOP3 <- abs(DOP3)
DA1 <- abs(DA1)
DA2 <- abs(DA2)
DA3 <- abs(DA3)
DAP1 <- abs(DAP1)
DAP2 <- abs(DAP2)
DAP3 <- abs(DAP3)

##### T-test Choice against Belief, with all sessions #####

T_diferencias <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs in choices & beliefs in all periods in 4 conditions
dimnames(T_diferencias) = list(c(1:8),c("BO1 t","BO1 p","BOP1 t","BOP1 p","BA1 t","BA1 p","BAP1 t","BAP1 p"))

for (a in 1:8){
  dC1vsBO1 <- data.frame(cbind(C1[a,],BO1[a,]))
  dC1vsBO1 <- stack(dC1vsBO1)
  dC1vsBO1 <- t.test(values~ind,data=dC1vsBO1)
  T_diferencias[a,1] <- dC1vsBO1$statistic
  T_diferencias[a,2] <- dC1vsBO1$p.value
}
for (a in 1:8){
  dC1vsBOP1 <- data.frame(cbind(C1[a,],BOP1[a,]))
  dC1vsBOP1 <- stack(dC1vsBOP1)
  dC1vsBOP1 <- t.test(values~ind,data=dC1vsBOP1)
  T_diferencias[a,3] <- dC1vsBOP1$statistic
  T_diferencias[a,4] <- dC1vsBOP1$p.value
}
for (a in 1:8){
  dC1vsBA1 <- data.frame(cbind(C1[a,],BA1[a,]))
  dC1vsBA1 <- stack(dC1vsBA1)
  dC1vsBA1 <- t.test(values~ind,data=dC1vsBA1)
  T_diferencias[a,5] <- dC1vsBA1$statistic
  T_diferencias[a,6] <- dC1vsBA1$p.value
}
for (a in 1:8){
  dC1vsBAP1 <- data.frame(cbind(C1[a,],BAP1[a,]))
  dC1vsBAP1 <- stack(dC1vsBAP1)
  dC1vsBAP1 <- t.test(values~ind,data=dC1vsBAP1)
  T_diferencias[a,7] <- dC1vsBAP1$statistic
  T_diferencias[a,8] <- dC1vsBAP1$p.value
}

CvBAll <- T_diferencias

CvBAll


##### T-test Difference between Choice and Belief againts zero, not normalized, with all sessions #####

AC123 <- matrix(data=NA, nrow=8, ncol=10)  #Average of choices of all players each Period(row) and Sesion(col)
for (a in 1:8){
  for (b in 1:10){
    AC123[a,b] <- mean(c(C1[a,b],C2[a,b],C3[a,b]))
  }
}

T_diferencias0 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs againts 0 in all periods in 4 conditions
dimnames(T_diferencias0) = list(c(1:8),c("DO1 t","DO1 p","DOP1 t","DOP1 p","DA1 t","DA1 p","DAP1 t","DAP1 p"))

for (a in 1:8){
  DO1t <- t.test(DO1[a,])
  T_diferencias0[a,1] <- DO1t$statistic
  T_diferencias0[a,2] <- DO1t$p.value
}
for (a in 1:8){
  DOP1t <- t.test(DOP1[a,])
  T_diferencias0[a,3] <- DOP1t$statistic
  T_diferencias0[a,4] <- DOP1t$p.value
}
for (a in 1:8){
  DA1t <- t.test(DA1[a,])
  T_diferencias0[a,5] <- DA1t$statistic
  T_diferencias0[a,6] <- DA1t$p.value
}
for (a in 1:8){
  DAP1t <- t.test(DAP1[a,])
  T_diferencias0[a,7] <- DAP1t$statistic
  T_diferencias0[a,8] <- DAP1t$p.value
}

CminB0All <- T_diferencias0

CminB0All

##### T-test Difference between Choice and Belief againts zero, normalized, with all sessions #####

DO1n <- DO1 / AC123
DOP1n <- DOP1 / AC123
DA1n <- DA1 / AC123
DAP1n <- DAP1 / AC123

T_diferencias0 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs againts 0 in all periods in 4 conditions
dimnames(T_diferencias0) = list(c(1:8),c("DO1 t","DO1 p","DOP1 t","DOP1 p","DA1 t","DA1 p","DAP1 t","DAP1 p"))

for (a in 1:8){
  DO1t <- t.test(DO1n[a,])
  T_diferencias0[a,1] <- DO1t$statistic
  T_diferencias0[a,2] <- DO1t$p.value
}
for (a in 1:8){
  DOP1t <- t.test(DOP1n[a,])
  T_diferencias0[a,3] <- DOP1t$statistic
  T_diferencias0[a,4] <- DOP1t$p.value
}
for (a in 1:8){
  DA1t <- t.test(DA1n[a,])
  T_diferencias0[a,5] <- DA1t$statistic
  T_diferencias0[a,6] <- DA1t$p.value
}
for (a in 1:8){
  DAP1t <- t.test(DAP1n[a,])
  T_diferencias0[a,7] <- DAP1t$statistic
  T_diferencias0[a,8] <- DAP1t$p.value
}

CminB0CmAll <- T_diferencias0

CminB0CmAll


##### T-test Choice against Belief, with session 3 removed #####

C1r3 <- C1[,-3]
BO1r3 <- BO1[,-3]                    #Esta sección elimina al sujeto problemático 3
BOP1r3 <- BOP1[,-3]
BA1r3 <- BA1[,-3]
BAP1r3 <- BAP1[,-3]

T_diferencias <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs in choices & beliefs in all periods in 4 conditions
dimnames(T_diferencias) = list(c(1:8),c("BO1 t","BO1 p","BOP1 t","BOP1 p","BA1 t","BA1 p","BAP1 t","BAP1 p"))

for (a in 1:8){
  dC1vsBO1 <- data.frame(cbind(C1r3[a,],BO1r3[a,]))
  dC1vsBO1 <- stack(dC1vsBO1)
  dC1vsBO1 <- t.test(values~ind,data=dC1vsBO1)
  T_diferencias[a,1] <- dC1vsBO1$statistic
  T_diferencias[a,2] <- dC1vsBO1$p.value
}
for (a in 1:8){
  dC1vsBOP1 <- data.frame(cbind(C1r3[a,],BOP1r3[a,]))
  dC1vsBOP1 <- stack(dC1vsBOP1)
  dC1vsBOP1 <- t.test(values~ind,data=dC1vsBOP1)
  T_diferencias[a,3] <- dC1vsBOP1$statistic
  T_diferencias[a,4] <- dC1vsBOP1$p.value
}
for (a in 1:8){
  dC1vsBA1 <- data.frame(cbind(C1r3[a,],BA1r3[a,]))
  dC1vsBA1 <- stack(dC1vsBA1)
  dC1vsBA1 <- t.test(values~ind,data=dC1vsBA1)
  T_diferencias[a,5] <- dC1vsBA1$statistic
  T_diferencias[a,6] <- dC1vsBA1$p.value
}
for (a in 1:8){
  dC1vsBAP1 <- data.frame(cbind(C1r3[a,],BAP1r3[a,]))
  dC1vsBAP1 <- stack(dC1vsBAP1)
  dC1vsBAP1 <- t.test(values~ind,data=dC1vsBAP1)
  T_diferencias[a,7] <- dC1vsBAP1$statistic
  T_diferencias[a,8] <- dC1vsBAP1$p.value
}

CvBno3 <- T_diferencias

CvBno3


##### T-test Difference between Choice and Belief againts zero, not normalized, with session 3 removed #####

C2r3 <- C2[,-3]
C3r3 <- C3[,-3]
DO1r3 <- DO1[,-3]
DOP1r3 <- DOP1[,-3]
DA1r3 <- DA1[,-3]
DAP1r3 <- DAP1[,-3]

AC123r <- matrix(data=NA, nrow=8, ncol=9)  #Average of choices of all players each Period(row) and Sesion(col)
for (a in 1:8){
  for (b in 1:9){
    AC123r[a,b] <- mean(c(C1r3[a,b],C2r3[a,b],C3r3[a,b]))
  }
}

T_diferencias0 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs againts 0 in all periods in 4 conditions
dimnames(T_diferencias0) = list(c(1:8),c("DO1 t","DO1 p","DOP1 t","DOP1 p","DA1 t","DA1 p","DAP1 t","DAP1 p"))

for (a in 1:8){
  DO1t <- t.test(DO1r3[a,])
  T_diferencias0[a,1] <- DO1t$statistic
  T_diferencias0[a,2] <- DO1t$p.value
}
for (a in 1:8){
  DOP1t <- t.test(DOP1r3[a,])
  T_diferencias0[a,3] <- DOP1t$statistic
  T_diferencias0[a,4] <- DOP1t$p.value
}
for (a in 1:8){
  DA1t <- t.test(DA1r3[a,])
  T_diferencias0[a,5] <- DA1t$statistic
  T_diferencias0[a,6] <- DA1t$p.value
}
for (a in 1:8){
  DAP1t <- t.test(DAP1r3[a,])
  T_diferencias0[a,7] <- DAP1t$statistic
  T_diferencias0[a,8] <- DAP1t$p.value
}

CminB0no3 <- T_diferencias0

CminB0no3

##### T-test Difference between Choice and Belief againts zero, normalized, with session 3 removed #####

DO1nr3 <- DO1r3 / AC123r
DOP1nr3 <- DOP1r3 / AC123r
DA1nr3 <- DA1r3 / AC123r
DAP1nr3 <- DAP1r3 / AC123r

T_diferencias0 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs againts 0 in all periods in 4 conditions
dimnames(T_diferencias0) = list(c(1:8),c("DO1 t","DO1 p","DOP1 t","DOP1 p","DA1 t","DA1 p","DAP1 t","DAP1 p"))

for (a in 1:8){
  DO1t <- t.test(DO1nr3[a,])
  T_diferencias0[a,1] <- DO1t$statistic
  T_diferencias0[a,2] <- DO1t$p.value
}
for (a in 1:8){
  DOP1t <- t.test(DOP1nr3[a,])
  T_diferencias0[a,3] <- DOP1t$statistic
  T_diferencias0[a,4] <- DOP1t$p.value
}
for (a in 1:8){
  DA1t <- t.test(DA1nr3[a,])
  T_diferencias0[a,5] <- DA1t$statistic
  T_diferencias0[a,6] <- DA1t$p.value
}
for (a in 1:8){
  DAP1t <- t.test(DAP1nr3[a,])
  T_diferencias0[a,7] <- DAP1t$statistic
  T_diferencias0[a,8] <- DAP1t$p.value
}

CminB0Cmno3 <- T_diferencias0

CminB0Cmno3

##### Resultados #####

CvBAll        #t-test choice vs belief, all sessions
CvBno3        #t-test choice vs belief, minus session 3
CminB0All     #t-test diff choice and belief vs 0, not normalized, all sessions
CminB0CmAll   #t-test diff choice and belief vs 0, normalized, all sessions
CminB0no3     #t-test diff choice and belief vs 0, not normalized, minus session 3
CminB0Cmno3   #t-test diff choice and belief vs 0, normalized, minus session 3


##### Comparación de diferencias normalizadas juego1 vs juego 2, todas las sesiones ####

#media de diferencias entre elección y creencia en periodos 1-4 de j1/ media de elecciones de todos en periodos 1-4 vs
#media de diferencias entre elección y creencia en periodos 5-8 de j2/ media de elecciones de todos en periodos 5-8

MDOJ1 <- matrix(data=NA, nrow=10, ncol=1)
MDOJ2 <- matrix(data=NA, nrow=10, ncol=1)
MDOPJ1 <- matrix(data=NA, nrow=10, ncol=1)
MDOPJ2 <- matrix(data=NA, nrow=10, ncol=1)
MDAJ1 <- matrix(data=NA, nrow=10, ncol=1)
MDAJ2 <- matrix(data=NA, nrow=10, ncol=1)
MDAPJ1 <- matrix(data=NA, nrow=10, ncol=1)
MDAPJ2 <- matrix(data=NA, nrow=10, ncol=1)

for (a in 1:10){
MDOJ1[a] <- mean(DO1n[1:4,a])
MDOJ2[a] <- mean(DO1n[5:8,a])
MDOPJ1[a] <- mean(DOP1n[1:4,a])
MDOPJ2[a] <- mean(DOP1n[5:8,a])
MDAJ1[a] <- mean(DA1n[1:4,a])
MDAJ2[a] <- mean(DA1n[5:8,a])
MDAPJ1[a] <- mean(DAP1n[1:4,a])
MDAPJ2[a] <- mean(DAP1n[5:8,a])
}

T_diferencias1v2 <- matrix(data=NA, nrow=4, ncol=2) #t-test of diffs in game1 vs game2, normalized
dimnames(T_diferencias1v2) = list(c("DO1","DOP1","DA1","DAP1"),c("t","p"))

MDO1v2 <- data.frame(cbind(MDOJ1[,1],MDOJ2[,1]))
MDO1v2 <- stack(MDO1v2)
MDO1v2 <- t.test(values~ind,data=MDO1v2)
T_diferencias1v2[1,1] <- MDO1v2$statistic
T_diferencias1v2[1,2] <- MDO1v2$p.value

MDOP1v2 <- data.frame(cbind(MDOPJ1[,1],MDOPJ2[,1]))
MDOP1v2 <- stack(MDOP1v2)
MDOP1v2 <- t.test(values~ind,data=MDOP1v2)
T_diferencias1v2[2,1] <- MDOP1v2$statistic
T_diferencias1v2[2,2] <- MDOP1v2$p.value

MDA1v2 <- data.frame(cbind(MDAJ1[,1],MDAJ2[,1]))
MDA1v2 <- stack(MDA1v2)
MDA1v2 <- t.test(values~ind,data=MDA1v2)
T_diferencias1v2[3,1] <- MDA1v2$statistic
T_diferencias1v2[3,2] <- MDA1v2$p.value

MDAP1v2 <- data.frame(cbind(MDAPJ1[,1],MDAPJ2[,1]))
MDAP1v2 <- stack(MDAP1v2)
MDAP1v2 <- t.test(values~ind,data=MDAP1v2)
T_diferencias1v2[4,1] <- MDAP1v2$statistic
T_diferencias1v2[4,2] <- MDAP1v2$p.value

Dif1v2n <- T_diferencias1v2


##### Comparación de diferencias normalizadas juego 1 vs juego2, sin la sesión 3 ####

#media de diferencias entre elección y creencia en periodos 1-4 de j1/ media de elecciones de todos en periodos 1-4 vs
#media de diferencias entre elección y creencia en periodos 5-8 de j2/ media de elecciones de todos en periodos 5-8

MDOJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDOJ2 <- matrix(data=NA, nrow=9, ncol=1)
MDOPJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDOPJ2 <- matrix(data=NA, nrow=9, ncol=1)
MDAJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDAJ2 <- matrix(data=NA, nrow=9, ncol=1)
MDAPJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDAPJ2 <- matrix(data=NA, nrow=9, ncol=1)

for (a in 1:9){
  MDOJ1[a] <- mean(DO1nr3[1:4,a])
  MDOJ2[a] <- mean(DO1nr3[5:8,a])
  MDOPJ1[a] <- mean(DOP1nr3[1:4,a])
  MDOPJ2[a] <- mean(DOP1nr3[5:8,a])
  MDAJ1[a] <- mean(DA1nr3[1:4,a])
  MDAJ2[a] <- mean(DA1nr3[5:8,a])
  MDAPJ1[a] <- mean(DAP1nr3[1:4,a])
  MDAPJ2[a] <- mean(DAP1nr3[5:8,a])
}

T_diferencias1v2 <- matrix(data=NA, nrow=4, ncol=2) #t-test of diffs in game1 vs game2, normalized
dimnames(T_diferencias1v2) = list(c("DO1","DOP1","DA1","DAP1"),c("t","p"))

MDO1v2 <- data.frame(cbind(MDOJ1[,1],MDOJ2[,1]))
MDO1v2 <- stack(MDO1v2)
MDO1v2 <- t.test(values~ind,data=MDO1v2)
T_diferencias1v2[1,1] <- MDO1v2$statistic
T_diferencias1v2[1,2] <- MDO1v2$p.value

MDOP1v2 <- data.frame(cbind(MDOPJ1[,1],MDOPJ2[,1]))
MDOP1v2 <- stack(MDOP1v2)
MDOP1v2 <- t.test(values~ind,data=MDOP1v2)
T_diferencias1v2[2,1] <- MDOP1v2$statistic
T_diferencias1v2[2,2] <- MDOP1v2$p.value

MDA1v2 <- data.frame(cbind(MDAJ1[,1],MDAJ2[,1]))
MDA1v2 <- stack(MDA1v2)
MDA1v2 <- t.test(values~ind,data=MDA1v2)
T_diferencias1v2[3,1] <- MDA1v2$statistic
T_diferencias1v2[3,2] <- MDA1v2$p.value

MDAP1v2 <- data.frame(cbind(MDAPJ1[,1],MDAPJ2[,1]))
MDAP1v2 <- stack(MDAP1v2)
MDAP1v2 <- t.test(values~ind,data=MDAP1v2)
T_diferencias1v2[4,1] <- MDAP1v2$statistic
T_diferencias1v2[4,2] <- MDAP1v2$p.value

Dif1v2nr3 <- T_diferencias1v2


##### Comparación de diferencias juego1 vs juego2, sin la sesión 3 ####

#media de diferencias entre elección y creencia en periodos 1-4 de j1/ media de elecciones de todos en periodos 1-4 vs
#media de diferencias entre elección y creencia en periodos 5-8 de j2/ media de elecciones de todos en periodos 5-8

MDOJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDOJ2 <- matrix(data=NA, nrow=9, ncol=1)
MDOPJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDOPJ2 <- matrix(data=NA, nrow=9, ncol=1)
MDAJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDAJ2 <- matrix(data=NA, nrow=9, ncol=1)
MDAPJ1 <- matrix(data=NA, nrow=9, ncol=1)
MDAPJ2 <- matrix(data=NA, nrow=9, ncol=1)

for (a in 1:9){
  MDOJ1[a] <- mean(DO1r3[1:4,a])
  MDOJ2[a] <- mean(DO1r3[5:8,a])
  MDOPJ1[a] <- mean(DOP1r3[1:4,a])
  MDOPJ2[a] <- mean(DOP1r3[5:8,a])
  MDAJ1[a] <- mean(DA1r3[1:4,a])
  MDAJ2[a] <- mean(DA1r3[5:8,a])
  MDAPJ1[a] <- mean(DAP1r3[1:4,a])
  MDAPJ2[a] <- mean(DAP1r3[5:8,a])
}

T_diferencias1v2 <- matrix(data=NA, nrow=4, ncol=2) #t-test of diffs in game1 vs game2, normalized
dimnames(T_diferencias1v2) = list(c("DO1","DOP1","DA1","DAP1"),c("t","p"))

MDO1v2 <- data.frame(cbind(MDOJ1[,1],MDOJ2[,1]))
MDO1v2 <- stack(MDO1v2)
MDO1v2 <- t.test(values~ind,data=MDO1v2)
T_diferencias1v2[1,1] <- MDO1v2$statistic
T_diferencias1v2[1,2] <- MDO1v2$p.value

MDOP1v2 <- data.frame(cbind(MDOPJ1[,1],MDOPJ2[,1]))
MDOP1v2 <- stack(MDOP1v2)
MDOP1v2 <- t.test(values~ind,data=MDOP1v2)
T_diferencias1v2[2,1] <- MDOP1v2$statistic
T_diferencias1v2[2,2] <- MDOP1v2$p.value

MDA1v2 <- data.frame(cbind(MDAJ1[,1],MDAJ2[,1]))
MDA1v2 <- stack(MDA1v2)
MDA1v2 <- t.test(values~ind,data=MDA1v2)
T_diferencias1v2[3,1] <- MDA1v2$statistic
T_diferencias1v2[3,2] <- MDA1v2$p.value

MDAP1v2 <- data.frame(cbind(MDAPJ1[,1],MDAPJ2[,1]))
MDAP1v2 <- stack(MDAP1v2)
MDAP1v2 <- t.test(values~ind,data=MDAP1v2)
T_diferencias1v2[4,1] <- MDAP1v2$statistic
T_diferencias1v2[4,2] <- MDAP1v2$p.value

Dif1v2r3 <- T_diferencias1v2