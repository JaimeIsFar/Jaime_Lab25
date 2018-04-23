setwd("C:/Users/Alejandro/Desktop/Jaime_Lab25/Datos")
#setwd("G:/APLICACION/EXP2")
library("grDevices", lib.loc="C:/Program Files/R/R-3.1.1/library")

#### Choices and Individual Beliefs of all Players in all Periods in each Sesion ####

pdf_name<-'Choices Allby1.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

for (b in 1:10){
plot(c(4,5),C1[4:5,b],type="l",lty=3,lwd=3,pch=1,cex=1,col="black",xlab="Period",ylab="Chosen Number",
     xlim=c(1,8),ylim=c(0,100),xaxt='n',yaxp=c(0,100,10),las=1)
axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))

lines(c(1,2,3,4),B1[1:4,b+c[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#FF00004D")  
lines(c(5,6,7,8),B1[5:8,b+c[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#FF00004D")
lines(c(1.1,2.1,3.1,4.1),B2[1:4,b+c[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#0900FF4D")
lines(c(5.1,6.1,7.1,8.1),B2[5:8,b+c[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#0900FF4D")
lines(c(0.9,1.9,2.9,3.9),B3[1:4,b+c[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#09FF004D")
lines(c(4.9,5.9,6.9,7.9),B3[5:8,b+c[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#09FF004D")
lines(c(1,2,3,4),B1[1:4,b+d[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#FF00004D")  
lines(c(5,6,7,8),B1[5:8,b+d[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#FF00004D")
lines(c(1.1,2.1,3.1,4.1),B2[1:4,b+d[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#0900FF4D")
lines(c(5.1,6.1,7.1,8.1),B2[5:8,b+d[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#0900FF4D")
lines(c(0.9,1.9,2.9,3.9),B3[1:4,b+d[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#09FF004D")
lines(c(4.9,5.9,6.9,7.9),B3[5:8,b+d[b]],type="p",lty=1,lwd=1,pch=18,cex=5,col="#09FF004D")

lines(C1[1:4,b],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
lines(c(5,6,7,8),C1[5:8,b],type="b",lty=1,lwd=4,pch=20,cex=1.5,col="red")
lines(C2[1:4,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="#0900FFCC")
lines(c(5,6,7,8),C2[5:8,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="#0900FFCC")
lines(C3[1:4,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="#09FF00CC")
lines(c(5,6,7,8),C3[5:8,b],type="b",lty=1,lwd=4,pch=20,cex=1,col="#09FF00CC")
text(4.5,95,paste("Sesion ", b),cex=0.9, font=2)
points(5.8,90,pch=16,cex=1,col="red")
points(5.8,86,pch=16,cex=1,col="blue")
points(5.8,82,pch=16,cex=1,col="green")
text(6,90,"Experienced",adj=0,cex=0.9,font=2)
text(6,86,"Inexperienced",adj=0,cex=0.9,font=2)
text(6,82,"Inexperienced",adj=0,cex=0.9,font=2)

for (a in 1:4){
if (W1[a,b] == 1){ points(a,C1[a,b],pch=3,cex=2.5,col="red") }
if (W2[a,b] == 1){ points(a,C2[a,b],pch=3,cex=2,col="blue") }
if (W3[a,b] == 1){ points(a,C3[a,b],pch=3,cex=1.5,col="green") }
if (W1[a+4,b] == 1){ points(a+4,C1[a+4,b],pch=3,cex=2.5,col="red") }
if (W2[a+4,b] == 1){ points(a+4,C2[a+4,b],pch=3,cex=2,col="blue") }
if (W3[a+4,b] == 1){ points(a+4,C3[a+4,b],pch=3,cex=1.5,col="green") }
}}

dev.off()

#### Choices and All Kind of Beliefs of each Player in all Periods in each Sesion ####

pdf_name<-'All Kind of Beliefs Expby1.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

for(b in 1:10){
plot(C1[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF",xlab="Period",ylab="Chosen Number",
     xlim=c(1,8),ylim=c(0,100),xaxt='n',yaxp=c(0,100,5),las=1)
axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))

lines(c(5,6,7,8),C1[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF")
lines(BO1[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
lines(c(5,6,7,8),BO1[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
lines(BOP1[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
lines(c(5,6,7,8),BOP1[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
lines(BA1[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
lines(c(5,6,7,8),BA1[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
lines(BAP1[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
lines(c(5,6,7,8),BAP1[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
lines(c(4,5),C1[4:5,b],type="l",lty=3, lwd=3,cex=1,col="black")
text(4.5,95,paste("Session ", b),cex=0.9, font=2)
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
  if (SD1[a,b] == abs(DO1[a,b])) {text(a,BO1[a,b]+2,"Others",cex=0.7,font=2)}
  if (SD1[a,b] == abs(DOP1[a,b])) {text(a,BOP1[a,b]+1,"Others*P",cex=0.7,font=2)}
  if (SD1[a,b] == abs(DA1[a,b])) {text(a,BA1[a,b]-1,"All",cex=0.7,font=2)}
  if (SD1[a,b] == abs(DAP1[a,b])) {text(a,BAP1[a,b]-2,"All*P",cex=0.7,font=2)}
  if (SD1[a+4,b] == abs(DO1[a+4,b])) {text(a+4,BO1[a+4,b]+2,"Others",cex=0.7,font=2)}
  if (SD1[a+4,b] == abs(DOP1[a+4,b])) {text(a+4,BOP1[a+4,b]+1,"Others*P",cex=0.7,font=2)}
  if (SD1[a+4,b] == abs(DA1[a+4,b])) {text(a+4,BA1[a+4,b]-1,"All",cex=0.7,font=2)}
  if (SD1[a+4,b] == abs(DAP1[a+4,b])) {text(a+4,BAP1[a+4,b]-2,"All*P",cex=0.7,font=2)}
}}

for(b in 1:10){
  plot(C2[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF",xlab="Period",ylab="Chosen Number",
       xlim=c(1,8),ylim=c(0,100),xaxt='n',yaxp=c(0,100,5),las=1)
  axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
  
  lines(c(5,6,7,8),C2[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF")
  lines(BO2[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
  lines(c(5,6,7,8),BO2[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
  lines(BOP2[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
  lines(c(5,6,7,8),BOP2[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
  lines(BA2[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
  lines(c(5,6,7,8),BA2[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
  lines(BAP2[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
  lines(c(5,6,7,8),BAP2[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
  text(4.5,95,paste("Session ", b),cex=0.9, font=2)
  text(4.5,90,"Inexperienced (1)",cex=0.9,font=2)
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
  if (SD2[a,b] == abs(DO2[a,b])) {text(a,BO2[a,b]+2,"Others",cex=0.7,font=2)}
  if (SD2[a,b] == abs(DOP2[a,b])) {text(a,BOP2[a,b]+1,"Others*P",cex=0.7,font=2)}
  if (SD2[a,b] == abs(DA2[a,b])) {text(a,BA2[a,b]-1,"All",cex=0.7,font=2)}
  if (SD2[a,b] == abs(DAP2[a,b])) {text(a,BAP2[a,b]-2,"All*P",cex=0.7,font=2)}
  if (SD2[a+4,b] == abs(DO2[a+4,b])) {text(a+4,BO2[a+4,b]+2,"Others",cex=0.7,font=2)}
  if (SD2[a+4,b] == abs(DOP2[a+4,b])) {text(a+4,BOP2[a+4,b]+1,"Others*P",cex=0.7,font=2)}
  if (SD2[a+4,b] == abs(DA2[a+4,b])) {text(a+4,BA2[a+4,b]-1,"All",cex=0.7,font=2)}
  if (SD2[a+4,b] == abs(DAP2[a+4,b])) {text(a+4,BAP2[a+4,b]-2,"All*P",cex=0.7,font=2)}
}}

for(b in 1:10){
  plot(C3[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF",xlab="Period",ylab="Chosen Number",
       xlim=c(1,8),ylim=c(0,100),xaxt='n',yaxp=c(0,100,5),las=1)
  axis(1,at=c(1:8),labels=c(1,2,3,4,1,2,3,4))
  
  lines(c(5,6,7,8),C3[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF0101FF")
  lines(BO3[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
  lines(c(5,6,7,8),BO3[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#01FF6780")
  lines(BOP3[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
  lines(c(5,6,7,8),BOP3[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#019AFF80")
  lines(BA3[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
  lines(c(5,6,7,8),BA3[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FFE70180")
  lines(BAP3[1:4,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
  lines(c(5,6,7,8),BAP3[5:8,b],type="b",lty=1,lwd=4,pch=21,cex=1,col="#FF670180")
  text(4.5,95,paste("Session ", b),cex=0.9, font=2)
  text(4.5,90,"Inexperienced (2)",cex=0.9,font=2)
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
    if (SD3[a,b] == abs(DO3[a,b])) {text(a,BO3[a,b]+2,"Others",cex=0.7,font=2)}
    if (SD3[a,b] == abs(DOP3[a,b])) {text(a,BOP3[a,b]+1,"Others*P",cex=0.7,font=2)}
    if (SD3[a,b] == abs(DA3[a,b])) {text(a,BA3[a,b]-1,"All",cex=0.7,font=2)}
    if (SD3[a,b] == abs(DAP3[a,b])) {text(a,BAP3[a,b]-2,"All*P",cex=0.7,font=2)}
    if (SD3[a+4,b] == abs(DO3[a+4,b])) {text(a+4,BO3[a+4,b]+2,"Others",cex=0.7,font=2)}
    if (SD3[a+4,b] == abs(DOP3[a+4,b])) {text(a+4,BOP3[a+4,b]+1,"Others*P",cex=0.7,font=2)}
    if (SD3[a+4,b] == abs(DA3[a+4,b])) {text(a+4,BA3[a+4,b]-1,"All",cex=0.7,font=2)}
    if (SD3[a+4,b] == abs(DAP3[a+4,b])) {text(a+4,BAP3[a+4,b]-2,"All*P",cex=0.7,font=2)}
}}

dev.off()

#### Choices VS All Kind of Beliefs of all Players in each Period of all Sesions ####

pdf_name<-'Choices VS Beliefs Othby1.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=F))

for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Period ", a),cex=1.2, font=2)
  text(80,90,"Other",cex=1.2, font=2)
  for (b in 1:10){
    text(C1[a,b],BO1[a,b],paste(b),cex=0.75,font=2,col="red")
    text(C2[a,b],BO2[a,b],paste(b),cex=0.75,font=2,col="blue")
    text(C3[a,b],BO3[a,b],paste(b),cex=0.75,font=2,col="green")
  }}

for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Period ", a),cex=1.2, font=2)
  text(80,90,"Other*P",cex=1.2, font=2)
  for (b in 1:10){
    text(C1[a,b],BOP1[a,b],paste(b),cex=0.75,font=2, col="red")
    text(C2[a,b],BOP2[a,b],paste(b),cex=0.75,font=2, col="blue")
    text(C3[a,b],BOP3[a,b],paste(b),cex=0.75,font=2, col="green")
  }}

for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Period ", a),cex=1.2, font=2)
  text(80,90,"All",cex=1.2, font=2)
  for (b in 1:10){
    text(C1[a,b],BA1[a,b],paste(b),cex=0.75,font=2, col="red")
    text(C2[a,b],BA2[a,b],paste(b),cex=0.75,font=2, col="blue")
    text(C3[a,b],BA3[a,b],paste(b),cex=0.75,font=2, col="green")
  }}

for (a in 1:8){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Period ", a),cex=1.2, font=2)
  text(80,90,"All*P",cex=1.2, font=2)
  for (b in 1:10){
    text(C1[a,b],BAP1[a,b],paste(b),cex=0.75,font=2, col="red")
    text(C2[a,b],BAP2[a,b],paste(b),cex=0.75,font=2, col="blue")
    text(C3[a,b],BAP3[a,b],paste(b),cex=0.75,font=2, col="green")
  }}

dev.off()

#### Choices VS All Kind of Beliefs of all Players in each Sesion of all Periods ####

pdf_name<-'Choices VS Beliefs bSOthby1.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

for (b in 1:10){
plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
     xlab="Chosen",ylab="Belief",main="")
lines(c(-10,110), c(-10,110), col="Black")
text(10,90,paste("Session ", b),cex=1.2, font=2)
text(80,90,"Other",cex=1.2, font=2)
for (a in 1:8){
text(C1[a,b],BO1[a,b],paste(a),cex=0.75,font=2,col="#FF000080")
text(C2[a,b],BO2[a,b],paste(a),cex=0.75,font=2,col="#0900FF80")
text(C3[a,b],BO3[a,b],paste(a),cex=0.75,font=2,col="#09FF0080")
  }}

for (b in 1:10){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Session ", b),cex=1.2, font=2)
  text(80,90,"Other*P",cex=1.2, font=2)
  for (a in 1:8){
    text(C1[a,b],BOP1[a,b],paste(a),cex=0.75,font=2,col="#FF000080")
    text(C2[a,b],BOP2[a,b],paste(a),cex=0.75,font=2,col="#0900FF80")
    text(C3[a,b],BOP3[a,b],paste(a),cex=0.75,font=2,col="#09FF0080")
  }}

for (b in 1:10){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Session ", b),cex=1.2, font=2)
  text(80,90,"All",cex=1.2, font=2)
  for (a in 1:8){
    text(C1[a,b],BA1[a,b],paste(a),cex=0.75,font=2,col="#FF000080")
    text(C2[a,b],BA2[a,b],paste(a),cex=0.75,font=2,col="#0900FF80")
    text(C3[a,b],BA3[a,b],paste(a),cex=0.75,font=2,col="#09FF0080")
  }}

for (b in 1:10){
  plot(1,type="n",xlim=c(0,100),ylim=c(0,100),xaxp=c(0,100,10),yaxp=c(0,100,10),
       xlab="Chosen",ylab="Belief",main="")
  lines(c(-10,110), c(-10,110), col="Black")
  text(10,90,paste("Session ", b),cex=1.2, font=2)
  text(80,90,"All*P",cex=1.2, font=2)
  for (a in 1:8){
    text(C1[a,b],BAP1[a,b],paste(a),cex=0.75,font=2,col="#FF000080")
    text(C2[a,b],BAP2[a,b],paste(a),cex=0.75,font=2,col="#0900FF80")
    text(C3[a,b],BAP3[a,b],paste(a),cex=0.75,font=2,col="#09FF0080")
  }}

dev.off()

#### Raw Differences Between Choices and Beliefs ####

pdf_name<-'RawDifferences Othby1.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
barplot(t(BarO[,b:(b+2)]),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
        xlab="Period", ylab="Difference between choice and belief", ylim=c(-60,100),yaxp=c(-60,100,16),las=1)
lines(c(0,30),c(0,0))
text(10,95,paste("Session ", f),cex=0.9, font=2)
text(1,97,"Other",adj=0,cex=0.9,font=2)
points(20,90, pch=15, col="grey20")
points(20,80, pch=15, col="grey80")
text(21,90,"Experienced",adj=0,cex=0.9,font=2)
text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)

if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
f <- f + 1
}

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(BarOP[,b:(b+2)]),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(-60,100),yaxp=c(-60,100,16),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"Other*P",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(BarA[,b:(b+2)]),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(-60,100),yaxp=c(-60,100,16),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"All",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(BarAP[,b:(b+2)]),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(-60,100),yaxp=c(-60,100,16),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"All*P",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

dev.off()

#### Absolute Differences Between Choices and Beliefs ####

pdf_name<-'AbsDifferences Othby1.pdf'
pdf(file=pdf_name,width=6,height=6)
layout(matrix(1,ncol=1, byrow=T))

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(abs(BarO[,b:(b+2)])),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(0,100),yaxp=c(0,100,10),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"Other",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(abs(BarOP[,b:(b+2)])),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(0,100),yaxp=c(0,100,10),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"Other*P",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(abs(BarA[,b:(b+2)])),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(0,100),yaxp=c(0,100,10),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"All",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

f <- 1
for (b in c(1,4,7,10,13,16,19,22,25,28)){
  barplot(t(abs(BarAP[,b:(b+2)])),names.arg=c(1,2,3,4,1,2,3,4), beside=T, col=c("grey20","grey80","grey80"),
          xlab="Period", ylab="Difference between choice and belief", ylim=c(0,100),yaxp=c(0,100,10),las=1)
  lines(c(0,30),c(0,0))
  text(10,95,paste("Session ", f),cex=0.9, font=2)
  text(1,97,"All*P",adj=0,cex=0.9,font=2)
  points(20,90, pch=15, col="grey20")
  points(20,80, pch=15, col="grey80")
  text(21,90,"Experienced",adj=0,cex=0.9,font=2)
  text(21,80,"Inexperienced",adj=0,cex=0.9,font=2)
  
  if (C1[4,f] < C1[5,f]) { text(10,80,"Reset exist",adj=0,cex=0.9,font=2) }
  f <- f + 1
}

dev.off()