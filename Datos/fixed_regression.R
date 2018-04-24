rm(list=ls())
#setwd("C:/Users/Laboratorio25_2/Desktop/Jaime_Lab25/Datos/Regresiones")
setwd("C:/Users/Alejandro/Desktop/Jaime_Lab25/Datos")
library("lfe", lib.loc="~/R/win-library/3.4")
library("lme4", lib.loc="~/R/win-library/3.4")
library("plm", lib.loc="~/R/win-library/3.4")
library("nonnest2", lib.loc="~/R/win-library/3.4")

##### Primera regresión de efectos fijos en los jugadores experimentados #####

#dat <- read.csv("fixed_numero_r3.csv")
dat <- read.csv("fixed_numero_S1vsS2.csv")

#fe_numero <- felm(dat$EA~dat$Periodo+dat$Periodo2+dat$MOT.1 | dat$Nuevo)

#segunda versión de la regresión incluyendo la conducta de los oponentes en el subjuego anterior
fe_S1vsS2 <- felm(dat$EA~dat$Periodo+dat$EBCS.1 | dat$Nuevo) #Incluyendo la variable de las 
#tiradas del superjuego anterior.


summary(fe_S1vsS2)

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


##### Comparacion de modelos #####

comp <- read.csv("comparacion_modelos.csv")

MO <- lm(EA~BO1, data=comp)
MOp <- lm(EA~BOP1, data=comp)
MA <- lm(EA~BA1, data=comp)
MAp <- lm(EA~BAP1, data=comp)
MOAp <- lm(EA~BOP1+BAP1, data=comp)

MOp <- felm(EA~BOP1|Periodo,data=comp)
MAp <- felm(EA~BAP1|Periodo,data=comp)

summary(MO)
summary(MOp)
summary(MA)
summary(MAp)
summary(MOAp)

anova(MOp,MOAp,test="Chisq")

plot(comp$EA,comp$BOP1,type="p")
plot(comp$EA,comp$BAP1,type="p")

icci(MOp, MAp, conf.level = 0.95)
vuongtest(MOp, MAp, nested = FALSE, adj = "none")
