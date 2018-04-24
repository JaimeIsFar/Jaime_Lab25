### TIRADA PROMEDIO DEL PARTICIPANTE A Y TODOS LOS DEMAS A LO LARGO DEL JUEGO PARA COMPROBAR SI HAY EFECTO DE RESET, INCLUYENDO AL PARTICIPANTE 3

MC1[a,1] <- mean(C1[a,])
MC23[a,1] <- mean(c(C2[a,],C3[a,]))

MC1_x <- matrix(data=NA, nrow=8, ncol=1) 
MC23_x <- matrix(data=NA, nrow=8, ncol=1)   

for (a in 1:8){
MC1_x[a,1] <- mean(C1_x[a,])
MC23_x[a,1] <- mean(c(C2_x[a,],C3_x[a,]))
}

plot(c(4,5),MC1[4:5],type="l",lty=3,lwd=3,pch=1,cex=1,col="black",xlab="Periodo",ylab="Número elegido",
       xlim=c(1,8),ylim=c(0,60),xaxt='n',yaxp=c(0,100,10),las=1)
  axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
  
  lines(MC1[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  lines(c(5,6,7,8),MC1[5:8],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  
  lines(MC23[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="blue")
  lines(c(5,6,7,8),MC23[5:8],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="blue")
  
  points(5.8,60,pch=16,cex=1,col="red")
  points(5.8,56,pch=16,cex=1,col="blue")
  text(6,60,"Participante A",adj=0,cex=0.9,font=2)
  text(6,56,"Participantes B-E",adj=0,cex=0.9,font=2)
  
  
### ### TIRADA PROMEDIO DEL PARTICIPANTE A Y TODOS LOS DEMAS A LO LARGO DEL JUEGO PARA COMPROBAR SI HAY EFECTO DE RESET, EXCLUYENDO AL PARTICIPANTE 3
  
  plot(c(4,5),MC1_x[4:5],type="l",lty=3,lwd=3,pch=1,cex=1,col="black",xlab="Periodo",ylab="Número elegido",
       xlim=c(1,8),ylim=c(0,60),xaxt='n',yaxp=c(0,100,10),las=1)
  axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
  
  lines(MC1_x[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  lines(c(5,6,7,8),MC1_x[5:8],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  
  lines(MC23_x[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="blue")
  lines(c(5,6,7,8),MC23_x[5:8],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="blue")
  
  points(5.8,60,pch=16,cex=1,col="red")
  points(5.8,56,pch=16,cex=1,col="blue")
  text(6,60,"Participante A",adj=0,cex=0.9,font=2)
  text(6,56,"Participantes B-E",adj=0,cex=0.9,font=2)
  
  
  ### NO RECUERDO QUE ES ESTO, IGNORALO
  
  MB123 <- matrix(data=NA, nrow=8, ncol=1)
  MBP123 <- matrix(data=NA, nrow=8, ncol=1)
  for (a in 1:8) {
  MB123[a,] <- mean(c(B1[a,],B2[a,],B3[a,]))
  }
  MBP123 <- MB123*2/3
  
  plot(0,type="l",lty=3,lwd=3,pch=1,cex=1,col="black",xlab="Periodo",ylab="Número",
       xlim=c(1,4),ylim=c(0,60),xaxt='n',yaxp=c(0,100,10),las=1)
  axis(1,at=c(1:4),labels=c(1,2,3,4))
  
  lines(MC123[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  
  lines(MB123[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="blue")
  
  lines(MBP123[1:4],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="green")
  
  points(5.8,60,pch=16,cex=1,col="red")
  points(5.8,56,pch=16,cex=1,col="blue")
  text(6,60,"Elección real",adj=0,cex=0.9,font=2)
  text(6,56,"Creencia",adj=0,cex=0.9,font=2)
  
  ### LAS TIRADAS DE TODOS LOS PARTICIPANTES EN TODOS LOS PERIODOS, SEPARADOS POR SESION
  
  for (b in 1:10){
    plot(c(4,5),C1[4:5,b],type="l",lty=3,lwd=3,pch=1,cex=1,col="black",xlab="Periodo",ylab="Número elegido",
         xlim=c(1,8),ylim=c(0,100),xaxt='n',yaxp=c(0,100,10),las=1)
    axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
    

    
    lines(C1[1:4,b],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
    lines(c(5,6,7,8),C1[5:8,b],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
    lines(C2[1:4,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="blue")
    lines(c(5,6,7,8),C2[5:8,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="purple")
    lines(C3[1:4,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="darkgreen")
    lines(c(5,6,7,8),C3[5:8,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="orange")
    text(4.5,99,paste("Sesión ", b),cex=0.9, font=2)
    points(5.8,96,pch=16,cex=1,col="red")
    points(5.8,92,pch=16,cex=1,col="blue")
    points(5.8,88,pch=16,cex=1,col="darkgreen")
    points(5.8,84,pch=16,cex=1,col="purple")
    points(5.8,80,pch=16,cex=1,col="orange")
    text(6,96,"Participante A",adj=0,cex=0.9,font=2)
    text(6,92,"Participante B",adj=0,cex=0.9,font=2)
    text(6,88,"Participante C",adj=0,cex=0.9,font=2)
    text(6,84,"Participante D",adj=0,cex=0.9,font=2)
    text(6,80,"Participante E",adj=0,cex=0.9,font=2)
  }
  
  ### SCATTERPLOT DE TIRADA EN CADA PERIODO VS LA TIRADA EN EL SIGUIENTE PERIODO (PARA COMPROBAR EFECTO DE RESET)
  
  for(a in 1:4){
    plot(C1[a,],C1[a+1,],type="p",lty=1,lwd=4,pch=20,cex=1,col="black",xlab=paste("Elección en Periodo ",a),
         ylab=paste("Elección en Periodo ",a+1),xlim=c(0,100),ylim=c(0,100))
    lines(c(-10,110), c(-10,110), col="Black")
  }
  
  
  ### BOXPLOT QUE ACOMPAÑA AL VIOLINPLOT
  
  boxplot(C1[4,],ylim=c(0,100))
  text(1.02,83,"Sesión 3",adj=0,cex=0.9,font=2)