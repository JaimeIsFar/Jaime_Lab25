rm(list=ls())
setwd("C:/Users/Laboratorio25_2/Desktop/Jaime_Lab25/Datos/Regresiones")
library("lfe", lib.loc="~/R/win-library/3.4")
library("lme4", lib.loc="~/R/win-library/3.4")
library("plm", lib.loc="~/R/win-library/3.4")

##### Primera regresión de efectos fijos en los jugadores experimentados #####

#dat <- read.csv("fixed_numero_r3.csv")
dat <- read.csv("fixed_numero.csv")

#fe_numero <- felm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1 | dat$Nuevo)
fe_numero <- felm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1+dat$EBCS.1 | dat$Nuevo) #Incluyendo la variable de las 
#tiradas del superjuego anterior.


summary(fe_numero)

#lm_numero <- lm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1+dat$Nuevo)

#summary(lm_numero)

#fe_numero$N

dat <- read.csv("fixed_cambio_relativo_r3.csv")

fe_cambio <- felm(dat$CREA~dat$Periodo+dat$Periodo2 | dat$Nuevo)

summary(fe_cambio)


#####Segunda regresión, variabilidad de experimentado entre subjuegos

dat2 <- read.csv("random_var.csv")

re_var <- lmer(dat2$Var~1+(1|dat2$Subjuego)+(1|dat2$Subjuego2),REML=FALSE)

re_var <- lm(dat2$Var~dat2$Subjuego+dat2$Subjuego2)
summary(re_var)


##### Intento 2 #####

dat <- read.csv("fixed_numero.csv")

fe_nu <- plm(EA~Periodo+Periodo2+MOT.1+EBCS.1,data=dat,model="within",index="Nuevo")


fe_numero <- felm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1+dat$EBCS.1 | dat$Nuevo) #Incluyendo la variable de las 
#tiradas del superjuego anterior.


summary(fe_numero)