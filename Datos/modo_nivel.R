##### cuantos participantes tiran dentro de los niveles 2 y 3 en el primer periodo de cada subjuego
#adicionalmente, hacer t-test del nivel promedio.

rm(list=ls())
setwd("C:/Users/Laboratorio25_2/Desktop/Jaime_Lab25/Datos")
library("grDevices", lib.loc="C:/Program Files/R/R-3.1.1/library")
datn <- read.csv("modo_nivel.csv")

p1 <- datn$n_EA.1
p1_1 <- c()
p1_5 <- c()
for(a in 1:10){
p1_1[a] <- datn$n_EA.1[-7+(a*8)]
p1_5[a] <- datn$n_EA.1[-3+(a*8)]}

t.test(p1_1,p1_5, alternative="l",paired=T) #el nivel cognitivo de los experimentados es significativamente menor en p5

