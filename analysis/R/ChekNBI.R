#code REDATAM'
# RUNDEF NBI
# SELECTION ALL
# FOR VIVIENDA.V02=1
# DEFINE PERSONA.EDADOK AS RECODE PERSONA.P03
# (0 = 1) 
# (1- 4 = 2) 
# (5 - 9 = 3) 
# (10 - 14 = 4) 
# (15 - 19 = 5) 
# (20 - 24 = 6) 
# (25 - 29 = 7) 
# (30 - 34 = 8) 
# (35 - 39 = 9) 
# (40 - 44 = 10) 
# (45 - 49 = 11) 
# (50 - 54 = 12) 
# (55 - 59 = 13) 
# (60 - 64 = 14) 
# (65 - 69 = 15) 
# (70 - 74 = 16) 
# (75 - 79 = 17) 
# (80 - 84 = 18) 
# (85 - 89 = 19) 
# (90 - 120 = 20) 
# VARLABEL "Edades Agrip"
# RANGE 1-20
# TYPE INTEGER 
# TABLE NBIDEP AS CROSSTABS OF 
# DPTO.DPTO BY PERSONA.EDADOK BY HOGAR.NBI1 
# OPTIONS COMPLETENAME  
# TABLE NBIDEP AS CROSSTABS OF 
# DPTO.DPTO BY PERSONA.EDADOK BY HOGAR.NBI2 
# OPTIONS COMPLETENAME  
# TABLE NBIDEP AS CROSSTABS OF 
# DPTO.DPTO BY PERSONA.EDADOK BY HOGAR.NBI3 
# OPTIONS COMPLETENAME  
# TABLE NBIDEP AS CROSSTABS OF 
# DPTO.DPTO BY PERSONA.EDADOK BY HOGAR.NBI4 
# OPTIONS COMPLETENAME  
# TABLE NBIDEP AS CROSSTABS OF 
# DPTO.DPTO BY PERSONA.EDADOK BY HOGAR.NBI5 
# OPTIONS COMPLETENAME    

# Definicion de NBI
# - Vivienda inconveniente (NBI 1): es el tipo de vivienda que habitan los hogares que moran
# en habitaciones de inquilinato, hotel o pensión, viviendas no destinadas a fines
# habitacionales, viviendas precarias y otro tipo de vivienda. Se excluye a las viviendas tipo
# casa, departamento o rancho.
# - Carencias sanitarias (NBI 2): incluye a los hogares que no poseen retrete.
# - Condiciones de Hacinamiento (NBI 3): es la relación entre la cantidad total de miembros
# del hogar y la cantidad de habitaciones de uso exclusivo del hogar. Técnicamente se considera
# que existe hacinamiento crítico cuando en el hogar hay más de tres personas por cuarto.
# - Inasistencia escolar (NBI 4): hogares que tienen al menos un niño en edad escolar (6 a 12)                                                                                años) que no asiste a la escuela.
# - Capacidad de subsistencia (NBI 5): incluye a los hogares que tienen cuatro o más personas
# por miembro ocupado y que tienen un jefe que no ha completado el tercer grado de
# escolaridad primaria.

###############
library("maps")
library("maptools")
library("sp")
library("raster")
library("ggplot2")
library("RColorBrewer")
library("rgdal")
library("spdep")
library("sqldf")
library("stringr")
library('dplyr')
library('tidyr')
library('classInt')


################# base con TM Estand e indicadores de NBI Estand TAMBIEN!

## Arg structure
p = read.csv("data/Expuestos.csv", header = T)
str(p)
p$EDAD_q <- pmin(trunc(p$EDAD/5)*5, rep(100,nrow(p)))
p$EDAD_q[p$EDAD>0 & p$EDAD<5] = 1
p[p$EDAD_q>90,] = 90
Estr = sqldf('select EDAD_q, sum(N)/(select sum(N) from p) as porc from p group by EDAD_q')

## NBI por EDAD QUINQUENAL (solo viviendas particulares)
pNBI = readxl::read_xlsx('data/pob_expuestos/NBI.xlsx', sheet = 'T')
# head(pNBI, 20); summary(pNBI$X); unique(pNBI$X)
pNBI = pNBI[!is.na(pNBI$X),]
pNBI$EDAD_q = (pNBI$X-2) * 5; pNBI$X = NULL 
pNBI$EDAD_q[pNBI$EDAD_q==0] = 1
pNBI$EDAD_q[pNBI$EDAD_q==-5] = 0

# merge
unique(pNBI$COD)
pNBI = pNBI[pNBI$COD!='TOTAL',]
pNBI = merge(pNBI, Estr, by=c('EDAD_q'), all.x=T)
pNBI$link = str_pad(pNBI$COD, 5, pad = "0")
pNBI$PROV = as.integer(substr(pNBI$link, 1,2))
pNBI$pNBI = pNBI$`1 Cumple condición`/(pNBI$`1 Cumple condición`+pNBI$`0 No cumple condición`)
pNBI = cbind(pNBI[pNBI$NBI=='1',], 
						 NBI2 = pNBI$pNBI[pNBI$NBI=='2'], 
						 NBI3 = pNBI$pNBI[pNBI$NBI=='3'], 
						 NBI4 = pNBI$pNBI[pNBI$NBI=='4'], 
						 NBI5 = pNBI$pNBI[pNBI$NBI=='5'])
pNBI$NBI=NULL
pNBI$NBI1 = pNBI$pNBI; pNBI$pNBI=NULL

# standarize
pNBI[,c('NBI1','NBI2','NBI3','NBI4','NBI5')] = pNBI[,c('NBI1','NBI2','NBI3','NBI4','NBI5')] * pNBI$porc 
NBIest = sqldf('select link, PROV, sum(Total) Total, 
								sum(NBI1) NBI1, sum(NBI2) NBI2, sum(NBI3) NBI3, sum(NBI4) NBI4, sum(NBI5) NBI5 
								from pNBI group by link, PROV')
NBIest$link = as.integer(NBIest$link)

# get my provs
pNBI = pNBI[pNBI$PROV %in% provs_codes,]
summary(pNBI)

## agrego mortalidad standard gral
dbtQ = dbtQcLaMat
str(dbtQ);unique(dbtQ$PROVRE)
dbtQ$PROVRE[dbtQ$PROVRE==24] = 6 #si ya esta definido 24 P lo vuelvo atras
dbtQ = dbtQ[order(dbtQ$link, dbtQ$EDAD_q),]  
str(dbtQ); plot(Estr); sum(Estr$porc)
dbtQ_std = merge(dbtQ, Estr, by=c('EDAD_q'), all.x=T)
# si no tiene expuestos, q tenga las defunciones
dbtQ_std$N[dbtQ_std$N==0] = dbtQ_std$d[dbtQ_std$N==0] 
# tasa
dbtQ_std$TMesp = dbtQ_std$d/dbtQ_std$N * dbtQ_std$porc
TMEst = sqldf('select link, sum(TMesp) as TM from dbtQ_std group by link')

###  merge NBI and MORT
# nrow(TMEst); nrow(NBIest); length(unique(TMEst$link)); length(as.character(unique(NBIest$link)))
setdiff(unique(TMEst$link),unique(NBIest$link))
ChekNBI = merge(NBIest, TMEst, by=c('link')) 
pNBI = ChekNBI

ChekNBI$Nscale = scale(ChekNBI$Total)/3
tail(ChekNBI)

# La Matanza
LAMA = which(ChekNBI$link==6427)
# representa...
LAMArep = sum(ChekNBI$Total[ChekNBI$link==6427])/sum(ChekNBI$Total[ChekNBI$PROV==6]) * 100

# graf chek
library(scales)
cols <- c("blue", "red", "purple", "blue", "green")

pdf('plots/ChekNBI.pdf')
par(mfrow=c(1,1))
plot(ChekNBI$NBI4[-LAMA], ChekNBI$TM[-LAMA], cex=ChekNBI$Nscale[-LAMA]*2, xlab='%NBI', ylab='TM Est.', 
		 ylim =  c(.005, .012), xlim = c(0,.06), pch=19, col = alpha(cols[4], 0.2))
points(ChekNBI$NBI5[-LAMA], ChekNBI$TM[-LAMA], cex=ChekNBI$Nscale[-LAMA]*2, pch=19, col = alpha(cols[5], 0.2))
points(ChekNBI$NBI4[LAMA], ChekNBI$TM[LAMA], pch=16, col=alpha(cols[4], 0.4), cex=ChekNBI$Nscale[LAMA]*2)
points(ChekNBI$NBI5[LAMA], ChekNBI$TM[LAMA], pch=16, col=alpha(cols[5], 0.4), cex=ChekNBI$Nscale[LAMA]*2)
text(ChekNBI$NBI4[LAMA], ChekNBI$TM[LAMA]-0.001, labels='La Matanza', cex=1, col=alpha(cols[4], 0.4))
text(ChekNBI$NBI5[LAMA], ChekNBI$TM[LAMA]-0.001, labels='La Matanza', cex=1, col=alpha(cols[5], 0.4))
fit4 = lm(formula = TM ~ NBI4, data = ChekNBI)
fit5 = lm(formula = TM ~ NBI5, data = ChekNBI)
abline(fit4, col=alpha(cols[4], 0.4), lty=2)
abline(fit5, col=alpha(cols[5], 0.4), lty=2)
legend('bottomright', c('NBI4', 'NBI5', 'Lin reg'), col=c(alpha(cols[4], 0.4), alpha(cols[5], 0.4), 'grey'), 
			 pch = c(16,16,NA), lty=c(NA,NA,2), bty = 'n', cex=.8)
dev.off()

pdf('plots/ChekNBIresto.pdf')
par(mfrow=c(1,1))
plot(ChekNBI$NBI1[-LAMA], ChekNBI$TM[-LAMA], cex=ChekNBI$Nscale[-LAMA]*2, xlab='%NBI', ylab='TM Est.', 
		 ylim =  c(.005, .012), xlim = c(0,.2), pch=19, col = alpha(cols[1], 0.2))
points(ChekNBI$NBI2[-LAMA], ChekNBI$TM[-LAMA], cex=ChekNBI$Nscale[-LAMA]*2, pch=19, col = alpha(cols[2], 0.2))
points(ChekNBI$NBI3[-LAMA], ChekNBI$TM[-LAMA], cex=ChekNBI$Nscale[-LAMA]*2, pch=19, col = alpha(cols[3], 0.2))
points(ChekNBI$NBI1[LAMA], ChekNBI$TM[LAMA], pch=16, col=alpha(cols[1], 0.4), cex=ChekNBI$Nscale[LAMA]*2)
points(ChekNBI$NBI2[LAMA], ChekNBI$TM[LAMA], pch=16, col=alpha(cols[2], 0.4), cex=ChekNBI$Nscale[LAMA]*2)
points(ChekNBI$NBI3[LAMA], ChekNBI$TM[LAMA], pch=16, col=alpha(cols[3], 0.4), cex=ChekNBI$Nscale[LAMA]*2)
fit4 = lm(formula = TM ~ NBI4, data = ChekNBI)
fit5 = lm(formula = TM ~ NBI5, data = ChekNBI)
abline(fit4, col=alpha(cols[4], 0.4), lty=2)
abline(fit5, col=alpha(cols[5], 0.4), lty=2)
legend('bottomright', c('NBI4', 'NBI5', 'Lin reg'), col=c(alpha(cols[4], 0.4), alpha(cols[5], 0.4), 'grey'), 
			 pch = c(16,16,NA), lty=c(NA,NA,2), bty = 'n', cex=.8)
dev.off()
