#t-test de comparación entre las diferencias de creencias y elecciones de experimentado vs no experimentados,
# todas las sesiones

T_diferenciasD1vs23 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs in choices & beliefs in all periods in 4 conditions
dimnames(T_diferenciasD1vs23) = list(c(1:8),c("BO1 t","BO1 p","BOP1 t","BOP1 p","BA1 t","BA1 p","BAP1 t","BAP1 p"))

DO23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
DO23[a,b] <- DO2[a,b]
DO23[a,b+10] <- DO3[a,b]
}}
DOP23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DOP23[a,b] <- DOP2[a,b]
  DOP23[a,b+10] <- DOP3[a,b]
}}
DA23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DA23[a,b] <- DA2[a,b]
  DA23[a,b+10] <- DA3[a,b]
}}
DAP23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DAP23[a,b] <- DAP2[a,b]
  DAP23[a,b+10] <- DAP3[a,b]
}}


for (a in 1:8){
  t_DO1vs23 <- data.frame(cbind(DO1[a,],DO23[a,]))
  t_DO1vs23[11:20,1] <- NA
  t_DO1vs23 <- stack(t_DO1vs23)
  t_DO1vs23 <- t.test(values~ind,data=t_DO1vs23)
  T_diferenciasD1vs23[a,1] <- t_DO1vs23$statistic
  T_diferenciasD1vs23[a,2] <- t_DO1vs23$p.value
}
for (a in 1:8){
  t_DOP1vs23 <- data.frame(cbind(DOP1[a,],DOP23[a,]))
  t_DOP1vs23[11:20,1] <- NA
  t_DOP1vs23 <- stack(t_DOP1vs23)
  t_DOP1vs23 <- t.test(values~ind,data=t_DOP1vs23)
  T_diferenciasD1vs23[a,3] <- t_DOP1vs23$statistic
  T_diferenciasD1vs23[a,4] <- t_DOP1vs23$p.value
}
for (a in 1:8){
  t_DA1vs23 <- data.frame(cbind(DA1[a,],DA23[a,]))
  t_DA1vs23[11:20,1] <- NA
  t_DA1vs23 <- stack(t_DA1vs23)
  t_DA1vs23 <- t.test(values~ind,data=t_DA1vs23)
  T_diferenciasD1vs23[a,5] <- t_DA1vs23$statistic
  T_diferenciasD1vs23[a,6] <- t_DA1vs23$p.value
}
for (a in 1:8){
  t_DAP1vs23 <- data.frame(cbind(DAP1[a,],DAP23[a,]))
  t_DAP1vs23[11:20,1] <- NA
  t_DAP1vs23 <- stack(t_DAP1vs23)
  t_DAP1vs23 <- t.test(values~ind,data=t_DAP1vs23)
  T_diferenciasD1vs23[a,7] <- t_DAP1vs23$statistic
  T_diferenciasD1vs23[a,8] <- t_DAP1vs23$p.value
}


#t-test de comparación entre las diferencias de creencias y elecciones de experimentado vs no experimentados,
# todas las sesiones, normalizada

DO2n <- DO2 / AC123
DOP2n <- DOP2 / AC123
DA2n <- DA2 / AC123
DAP2n <- DAP2 / AC123
DO3n <- DO3 / AC123
DOP3n <- DOP3 / AC123
DA3n <- DA3 / AC123
DAP3n <- DAP3 / AC123

T_diferenciasD1vs23 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs in choices & beliefs in all periods in 4 conditions
dimnames(T_diferenciasD1vs23) = list(c(1:8),c("BO1 t","BO1 p","BOP1 t","BOP1 p","BA1 t","BA1 p","BAP1 t","BAP1 p"))

DO23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DO23[a,b] <- DO2n[a,b]
  DO23[a,b+10] <- DO3n[a,b]
}}
DOP23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DOP23[a,b] <- DOP2n[a,b]
  DOP23[a,b+10] <- DOP3n[a,b]
}}
DA23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DA23[a,b] <- DA2n[a,b]
  DA23[a,b+10] <- DA3n[a,b]
}}
DAP23 <- matrix(data=NA, nrow=8,ncol=20) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:10){
  DAP23[a,b] <- DAP2n[a,b]
  DAP23[a,b+10] <- DAP3n[a,b]
}}


for (a in 1:8){
  t_DO1vs23 <- data.frame(cbind(DO1[a,],DO23[a,]))
  t_DO1vs23[11:20,1] <- NA
  t_DO1vs23 <- stack(t_DO1vs23)
  t_DO1vs23 <- t.test(values~ind,data=t_DO1vs23)
  T_diferenciasD1vs23[a,1] <- t_DO1vs23$statistic
  T_diferenciasD1vs23[a,2] <- t_DO1vs23$p.value
}
for (a in 1:8){
  t_DOP1vs23 <- data.frame(cbind(DOP1[a,],DOP23[a,]))
  t_DOP1vs23[11:20,1] <- NA
  t_DOP1vs23 <- stack(t_DOP1vs23)
  t_DOP1vs23 <- t.test(values~ind,data=t_DOP1vs23)
  T_diferenciasD1vs23[a,3] <- t_DOP1vs23$statistic
  T_diferenciasD1vs23[a,4] <- t_DOP1vs23$p.value
}
for (a in 1:8){
  t_DA1vs23 <- data.frame(cbind(DA1[a,],DA23[a,]))
  t_DA1vs23[11:20,1] <- NA
  t_DA1vs23 <- stack(t_DA1vs23)
  t_DA1vs23 <- t.test(values~ind,data=t_DA1vs23)
  T_diferenciasD1vs23[a,5] <- t_DA1vs23$statistic
  T_diferenciasD1vs23[a,6] <- t_DA1vs23$p.value
}
for (a in 1:8){
  t_DAP1vs23 <- data.frame(cbind(DAP1[a,],DAP23[a,]))
  t_DAP1vs23[11:20,1] <- NA
  t_DAP1vs23 <- stack(t_DAP1vs23)
  t_DAP1vs23 <- t.test(values~ind,data=t_DAP1vs23)
  T_diferenciasD1vs23[a,7] <- t_DAP1vs23$statistic
  T_diferenciasD1vs23[a,8] <- t_DAP1vs23$p.value
}
#t-test de comparación entre las diferencias de creencias y elecciones de experimentado vs no experimentados,
# sesión 3 eliminada

DO2r3 <- DO2[,-3]
DO3r3 <- DO3[,-3]
DOP2r3 <- DO2[,-3]
DOP3r3 <- DO3[,-3]
DA2r3 <- DO2[,-3]
DA3r3 <- DO3[,-3]
DAP2r3 <- DO2[,-3]
DAP3r3 <- DO3[,-3]

T_diferenciasD1vs23 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs in choices & beliefs in all periods in 4 conditions
dimnames(T_diferenciasD1vs23) = list(c(1:8),c("BO1 t","BO1 p","BOP1 t","BOP1 p","BA1 t","BA1 p","BAP1 t","BAP1 p"))

DO23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DO23[a,b] <- DO2r3[a,b]
  DO23[a,b+9] <- DO3r3[a,b]
}}
DOP23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DOP23[a,b] <- DOP2r3[a,b]
  DOP23[a,b+9] <- DOP3r3[a,b]
}}
DA23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DA23[a,b] <- DA2r3[a,b]
  DA23[a,b+9] <- DA3r3[a,b]
}}
DAP23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DAP23[a,b] <- DAP2r3[a,b]
  DAP23[a,b+9] <- DAP3r3[a,b]
}}


for (a in 1:8){
  t_DO1vs23 <- data.frame(cbind(DO1r3[a,],DO23[a,]))
  t_DO1vs23[10:18,1] <- NA
  t_DO1vs23 <- stack(t_DO1vs23)
  t_DO1vs23 <- t.test(values~ind,data=t_DO1vs23)
  T_diferenciasD1vs23[a,1] <- t_DO1vs23$statistic
  T_diferenciasD1vs23[a,2] <- t_DO1vs23$p.value
}
for (a in 1:8){
  t_DOP1vs23 <- data.frame(cbind(DOP1r3[a,],DOP23[a,]))
  t_DOP1vs23[10:18,1] <- NA
  t_DOP1vs23 <- stack(t_DOP1vs23)
  t_DOP1vs23 <- t.test(values~ind,data=t_DOP1vs23)
  T_diferenciasD1vs23[a,3] <- t_DOP1vs23$statistic
  T_diferenciasD1vs23[a,4] <- t_DOP1vs23$p.value
}
for (a in 1:8){
  t_DA1vs23 <- data.frame(cbind(DA1r3[a,],DA23[a,]))
  t_DA1vs23[10:18,1] <- NA
  t_DA1vs23 <- stack(t_DA1vs23)
  t_DA1vs23 <- t.test(values~ind,data=t_DA1vs23)
  T_diferenciasD1vs23[a,5] <- t_DA1vs23$statistic
  T_diferenciasD1vs23[a,6] <- t_DA1vs23$p.value
}
for (a in 1:8){
  t_DAP1vs23 <- data.frame(cbind(DAP1r3[a,],DAP23[a,]))
  t_DAP1vs23[10:18,1] <- NA
  t_DAP1vs23 <- stack(t_DAP1vs23)
  t_DAP1vs23 <- t.test(values~ind,data=t_DAP1vs23)
  T_diferenciasD1vs23[a,7] <- t_DAP1vs23$statistic
  T_diferenciasD1vs23[a,8] <- t_DAP1vs23$p.value
}


#t-test de comparación entre las diferencias de creencias y elecciones de experimentado vs no experimentados,
# sesión 3 eliminada, normalizada

DO2nr3 <- DO2n[,-3]
DO3nr3 <- DO3n[,-3]
DOP2nr3 <- DO2n[,-3]
DOP3nr3 <- DO3n[,-3]
DA2nr3 <- DO2n[,-3]
DA3nr3 <- DO3n[,-3]
DAP2nr3 <- DO2n[,-3]
DAP3nr3 <- DO3n[,-3]

T_diferenciasD1vs23 <- matrix(data=NA, nrow=8, ncol=8) #t-test of diffs in choices & beliefs in all periods in 4 conditions
dimnames(T_diferenciasD1vs23) = list(c(1:8),c("BO1 t","BO1 p","BOP1 t","BOP1 p","BA1 t","BA1 p","BAP1 t","BAP1 p"))

DO23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DO23[a,b] <- DO2nr3[a,b]
  DO23[a,b+9] <- DO3nr3[a,b]
}}
DOP23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DOP23[a,b] <- DOP2nr3[a,b]
  DOP23[a,b+9] <- DOP3nr3[a,b]
}}
DA23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DA23[a,b] <- DA2nr3[a,b]
  DA23[a,b+9] <- DA3nr3[a,b]
}}
DAP23 <- matrix(data=NA, nrow=8,ncol=18) #diferencias de jugador 2 y 3 en una sola matriz
for(a in 1:8){for(b in 1:9){
  DAP23[a,b] <- DAP2nr3[a,b]
  DAP23[a,b+9] <- DAP3nr3[a,b]
}}


for (a in 1:8){
  t_DO1vs23 <- data.frame(cbind(DO1r3[a,],DO23[a,]))
  t_DO1vs23[10:18,1] <- NA
  t_DO1vs23 <- stack(t_DO1vs23)
  t_DO1vs23 <- t.test(values~ind,data=t_DO1vs23)
  T_diferenciasD1vs23[a,1] <- t_DO1vs23$statistic
  T_diferenciasD1vs23[a,2] <- t_DO1vs23$p.value
}
for (a in 1:8){
  t_DOP1vs23 <- data.frame(cbind(DOP1r3[a,],DOP23[a,]))
  t_DOP1vs23[10:18,1] <- NA
  t_DOP1vs23 <- stack(t_DOP1vs23)
  t_DOP1vs23 <- t.test(values~ind,data=t_DOP1vs23)
  T_diferenciasD1vs23[a,3] <- t_DOP1vs23$statistic
  T_diferenciasD1vs23[a,4] <- t_DOP1vs23$p.value
}
for (a in 1:8){
  t_DA1vs23 <- data.frame(cbind(DA1r3[a,],DA23[a,]))
  t_DA1vs23[10:18,1] <- NA
  t_DA1vs23 <- stack(t_DA1vs23)
  t_DA1vs23 <- t.test(values~ind,data=t_DA1vs23)
  T_diferenciasD1vs23[a,5] <- t_DA1vs23$statistic
  T_diferenciasD1vs23[a,6] <- t_DA1vs23$p.value
}
for (a in 1:8){
  t_DAP1vs23 <- data.frame(cbind(DAP1r3[a,],DAP23[a,]))
  t_DAP1vs23[10:18,1] <- NA
  t_DAP1vs23 <- stack(t_DAP1vs23)
  t_DAP1vs23 <- t.test(values~ind,data=t_DAP1vs23)
  T_diferenciasD1vs23[a,7] <- t_DAP1vs23$statistic
  T_diferenciasD1vs23[a,8] <- t_DAP1vs23$p.value
}