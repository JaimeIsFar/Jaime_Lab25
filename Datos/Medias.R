#setwd("D:/APLICACION/EXP2")
#setwd("G:/APLICACION/EXP2")
setwd("C:/Users/Alejandro/Desktop/Jaime_Lab25/Datos")
library("grDevices", lib.loc="C:/Program Files/R/R-3.1.1/library")

#### Mean Choices and Individual Beliefs by Period ####

pdf_name<-'MeanChoices.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

  plot(c(4,5),MC1[4:5,1],type="l",lty=3,lwd=3,pch=1,cex=1,col="black",xlab="Period",ylab="Chosen Number",
       xlim=c(1,8),ylim=c(0,60),xaxt='n',yaxp=c(0,60,12),las=1)
  axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
  lines(MC1[1:4,1],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  lines(c(5,6,7,8),MC1[5:8,1],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
  lines(MC23[1:4,1],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="#0900FFCC")
  lines(c(5,6,7,8),MC23[5:8,1],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="#0900FFCC")
  points(5.8,50,pch=16,cex=1,col="red")
  points(5.8,46,pch=16,cex=1,col="blue")
  text(6,50,"Experienced",adj=0,cex=0.9,font=2)
  text(6,46,"Inexperienced",adj=0,cex=0.9,font=2)
  text(4.5,59,"Average Choices of all Sessions",cex=1.1,font=2)
text(4.5,12,paste("SubGame2 Exp Wins = ", sum(W1[5:8,])),adj=0,cex=0.8, font=2)
text(4.5,8,paste("SubGame2 In1 Wins = ", sum(W2[5:8,])),adj=0,cex=0.8, font=2)
text(4.5,4,paste("SubGame2 In2 Wins = ", sum(W3[5:8,])),adj=0,cex=0.8, font=2)

dev.off()

#### Mean Choices and All Kind of Beliefs by Period ####

pdf_name<-'MeanChoices&AllBeliefs.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

  plot(MC1[1:4,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF",xlab="Period",ylab="Chosen Number",
       xlim=c(1,8),ylim=c(0,100),xaxt='n',yaxp=c(0,100,5),las=1)
  axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
  
  lines(c(5,6,7,8),MC1[5:8,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF")
  lines(MBO1[1:4,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
  lines(c(5,6,7,8),MBO1[5:8,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
  lines(MBOP1[1:4,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
  lines(c(5,6,7,8),MBOP1[5:8,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
  lines(MBA1[1:4,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
  lines(c(5,6,7,8),MBA1[5:8,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
  lines(MBAP1[1:4,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
  lines(c(5,6,7,8),MBAP1[5:8,1],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
  lines(c(4,5),MC1[4:5,1],type="l",lty=3, lwd=3,cex=1,col="black")
  text(4.5,90,"Experienced",cex=0.9,font=2)
  points(5.8,90,pch=16,cex=1,col="#FF0101FF")
  points(5.8,85,pch=16,cex=1,col="#01FF67FF")
  points(5.8,80,pch=16,cex=1,col="#019AFFFF")
  points(5.8,75,pch=16,cex=1,col="#FFE701FF")
  points(5.8,70,pch=16,cex=1,col="#FF6701FF")
  text(6,90,"Choice",adj=0,cex=0.9,font=2)
  text(6,85,"Beliefs Others",adj=0,cex=0.9,font=2)
  text(6,80,"Beliefs Others * P",adj=0,cex=0.9,font=2)
  text(6,75,"Beliefs All",adj=0,cex=0.9,font=2)
  text(6,70,"Beliefs All * P",adj=0,cex=0.9,font=2)

for (a in 1:4){
  if (CSD1[a,1] == abs(MDO1[a,1])) {text(a,MBO1[a,1]+2,"Others",cex=0.7,font=2)}
  if (CSD1[a,1] == abs(MDOP1[a,1])) {text(a,MBOP1[a,1]+1,"Others*P",cex=0.7,font=2)}
  if (CSD1[a,1] == abs(MDA1[a,1])) {text(a,MBA1[a,1]-1,"All",cex=0.7,font=2)}
  if (CSD1[a,1] == abs(MDAP1[a,1])) {text(a,MBAP1[a,1]-2,"All*P",cex=0.7,font=2)}
  if (CSD1[a+4,1] == abs(MDO1[a+4,1])) {text(a+4,MBO1[a+4,1]+2,"Others",cex=0.7,font=2)}
  if (CSD1[a+4,1] == abs(MDOP1[a+4,1])) {text(a+4,MBOP1[a+4,1]+1,"Others*P",cex=0.7,font=2)}
  if (CSD1[a+4,1] == abs(MDA1[a+4,1])) {text(a+4,MBA1[a+4,1]-1,"All",cex=0.7,font=2)}
  if (CSD1[a+4,1] == abs(MDAP1[a+4,1])) {text(a+4,MBAP1[a+4,1]-2,"All*P",cex=0.7,font=2)}
}

dev.off()

#### Mean Choices VS All Kind of Beliefs by Period



#### Mean Choices VS All Kind of Beliefs by Sesion



#### Mean Raw Differences Between Choices and Beliefs



#### Mean Absolute Differences Between Choices and Beliefs
