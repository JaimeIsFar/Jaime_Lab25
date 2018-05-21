#### SCATTERPLOT DE NUMERO ELEGIDO VS CREENCIA PROMEDIO

#OMITE P
#par(mfcol=c(4,2))
par(mfcol=c(1,1))
for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Número elegido",ylab="Creencia",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Periodo ", a),cex=1.2, font=2)
  for (b in 1:10){
    points(C1[a,b],BO1[a,b],cex=1,pch=16,col="black")
    points(C2[a,b],BO2[a,b],cex=1,pch=16,col="black")
    points(C3[a,b],BO3[a,b],cex=1,pch=16,col="black")
    #points(C1[a,3],BO1[a,3],cex=1,pch=16,col="blue")
  }}

#INCLUYE P
par(mfcol=c(1,1))
for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Número elegido",ylab="Creencia",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Periodo ", a),cex=1.2, font=2)
  for (b in 1:10){
    points(C1[a,b],BOP1[a,b],cex=1,pch=16,col="red")
    points(C2[a,b],BOP2[a,b],cex=1,pch=16,col="black")
    points(C3[a,b],BOP3[a,b],cex=1,pch=16,col="black")
    #points(C1[a,3],BOP1[a,3],cex=1,pch=16,col="blue")
  }}

##### SCATTERPLOT DE CREENCIA PROMEDIO VS NUMERO ELEGIDO PROMEDIO DE LOS OTROS

C23 <- matrix(data=NA, nrow=8, ncol=10)
C13 <- matrix(data=NA, nrow=8, ncol=10)
C12 <- matrix(data=NA, nrow=8, ncol=10)

for (a in 1:8){
  for (b in 1:10){
C23[a,b] <- mean(c(C2[a,b],C3[a,b]))
C13[a,b] <- mean(c(C1[a,b],C3[a,b]))
C12[a,b] <- mean(c(C1[a,b],C2[a,b]))
  }}

for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Número de otros jugadores",ylab="Creencia",main="", cex.lab=1.3)
  lines(c(-10,110), c(-10,110), col="Black")
  lines(c(0,95), c(5,100), lty=3, col="Black")
  lines(c(5,100), c(0,95), lty=3, col="Black")
  text(10,90,paste("Periodo ", a),cex=1.2, font=2)
  for (b in 1:10){
    points(C23[a,b],BO1[a,b],cex=1,pch=16,col="red")
    points(C13[a,b],BO2[a,b],cex=1,pch=16,col="black")
    points(C12[a,b],BO3[a,b],cex=1,pch=16,col="black")
    #points(C1[a,3],BO1[a,3],cex=1,pch=16,col="blue")
  }}

for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Número de otros jugadores",ylab="Creencia",main="", cex.lab=1.3)
  lines(c(-10,110), c(-10,110), col="Black")
  lines(c(0,95), c(5,100), lty=3, col="Black")
  lines(c(5,100), c(0,95), lty=3, col="Black")
  text(10,90,paste("Periodo ", a),cex=1.2, font=2)
  for (b in 1:10){
    points(C23[a,b],BOP1[a,b],cex=1,pch=16,col="red")
    points(C13[a,b],BOP2[a,b],cex=1,pch=16,col="black")
    points(C12[a,b],BOP3[a,b],cex=1,pch=16,col="black")
    #points(C1[a,3],BOP1[a,3],cex=1,pch=16,col="blue")
  }}


### VIOLIN PLOT

library(vioplot)
vioplot(C1[4,], ylim=c(0,100),col="grey")