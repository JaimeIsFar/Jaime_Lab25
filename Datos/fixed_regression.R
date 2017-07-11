rm(list=ls())
setwd("C:/Users/Laboratorio25_2/Desktop/Jaime_Lab25/Datos/Regresiones")
library("lfe", lib.loc="~/R/win-library/3.4")

dat <- read.csv("fixed_numero.csv")

fe_numero <- felm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1 | dat$Nuevo)

summary(fe_numero)

lm_numero <- lm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1+dat$Nuevo)

summary(lm_numero)

fe_numero$N
