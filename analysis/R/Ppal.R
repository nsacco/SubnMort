##Código para estimador bayesiano empírico por método de contracción (Assuncao y otros, 2005). Arg 2009-2011
##Aún Desprolijo. Realizado por Iván "Máster" Williams. Cambios menores realizados por N. "Nikito" Sacco.

## Para todos los usuarios: poner working dir en la carpeta ppal del proyecto.

##Tasas de mortalidad quinquenales y EV de departamentos. Años 2009-2011
##Provincias: region pampeana
##Estimación bayesiana empírica por método de contracción ("shrinkage"). 
##Método de momentos para los parámetros

###Principales Resultados (a partir de línea)
#Diagnístico de Desconocidos (línea 122)
#Gráficos de tasas de dptos por provincia (cada prov un pdf) (línea 361)
#Gráficos de dispersión de esperanzas por provincia (línea 424)
#Tabla resúmen de medidas de dispersión (línea 451)

setwd("analysis")

###Creo que habría que separar en la fila 130: un script para el merge y otro de metdología y resultados

#paquetes q requiero
library(foreign)
library(sqldf)
library(xlsx)
library(stringr)
library(ggplot2)
library(knitr)
library(gridExtra)
library(tidyverse)
library("DemoTools")
library(RColorBrewer)
library("DescTools")
library(reshape2)
library(splines)
library(stringr)
library(scales)


#levanto expuestos
p = read.csv("data/Expuestos.csv")

#levanto defunciones
def091011 = read.csv("data/Defunciones.csv")

#levanto regionalización
Cluster = read.csv("data/Cluster.csv")

#levanto métodos
source("R/Metodos.R")

# provincias a estimar
provs_codes = c(6, 14, 30, 42, 82)
provs_names = c('Buenos Aires', 'Cordoba', 'Entre Rios', 'La Pampa', 'Santa Fe')
provs_names_codes = data.frame(provs_codes, provs_names)
provs_names_codes$provs_names = as.character(provs_names_codes$provs_names)

#######################################MERGE de DEFUNCIONES Y POBLACION

#para mergear veo estructura de datos y cambio nombres

# str(def091011);str(p);summary(def091011);summary(p)
p$PROVRE = p$PROV;p$PROV = NULL
p$EDAD_r = p$EDAD;p$EDAD = NULL

# qué dptos tengo por Censo y que dptos tengo por DEIS
# unique(p$PROVRE);unique(def091011$PROVRE)
dptosP = as.data.frame.matrix(table(p$DEPRE[p$PROVRE %in% provs_codes],
                                    p$PROVRE[p$PROVRE %in% provs_codes]))
dptosP$dpto = as.integer(rownames(dptosP))
dptosD = subset(def091011, PROVRE %in% provs_codes)
dptosD = as.data.frame.matrix(table(dptosD$DEPRE,dptosD$PROVRE)) #unica dif el 999
dptosD$dpto = as.integer(rownames(dptosD))

#diferencia
setdiff(dptosD$dpto,dptosP$dpto) #ver q pasa c dpto 218 y 466, por ahora los sacco :)

# Agrupar Edades! edades 0, 1 a 4, quinquenal hasta 90, grupo abierto final. Sin desagregar sexo

def091011$EDAD_r[def091011$EDAD_r>90] <- 90
p$EDAD_r[p$EDAD_r>90] <- 90
def091011 = sqldf('select PROVRE, DEPRE, EDAD_r, sum(ndef) ndef from def091011 group by PROVRE, DEPRE, EDAD_r')
p = sqldf('select PROVRE, DEPRE, EDAD_r, sum(N) N from p group by PROVRE, DEPRE, EDAD_r')

# merge de N y D!!!
str(def091011); str(p); summary(def091011); summary(p)
dbt <- merge(x = p, y = def091011, by = c("PROVRE","DEPRE","EDAD_r"), all.x = TRUE)
head(dbt); summary(dbt)
# completar con ceros
dbt$ndef[is.na(dbt$ndef)] <- 0
dbt$d = dbt$ndef; dbt$ndef=NULL
TBM_control = sum(dbt$d)/sum(dbt$N)*1000 
dbtNOsirve = dbt

## me quedo con provs de interes
dbt = dbt[dbt$PROVRE %in% provs_codes,]

## Grafico tasas de deptos al azar
par(mfrow=c(1,2))
for (i in 1:2){
	prov_graf = subset(dbt, PROVRE == sample(unique(dbt$PROVRE),1) & EDAD_r < 999)
	depre_g <- subset(prov_graf, DEPRE == sample(unique(prov_graf$DEPRE),1))
	plot(depre_g$EDAD_r,log(depre_g$d/depre_g$N), col = 1, ylim=c(-10,0),
			 type = "p", ylab = paste("log(m)"), xlab = "x")
}
par(mfrow=c(1,1))


#############Agrego nombres de provincias y dptos, que no estaban
nombres = read.xlsx("data/map/codigos_prov_depto_censo2010.xls", sheetIndex = 3, encoding = "UTF-8")
dbt = merge(dbt, nombres, by.x = c("PROVRE","DEPRE"), by.y = c("Prov_cod","Dpto_cod"), all.x = T)
summary(dbt)
dbt = dbt[!is.na(dbt$Prov_Nombre),]
dbt$Prov_Nombre = as.character(dbt$Prov_Nombre)
dbt$Dpto_Nombre = as.character(dbt$Dpto_Nombre)
dbt$link = as.integer(paste0(str_pad(dbt$PROVRE, 2, pad = "0"), 
															str_pad(dbt$DEPRE, 3, pad = "0")))

### 90+ edad quinquenal (Bayes Emp)
dbtQ <- dbt
dbtQ$EDAD_q <- trunc(dbtQ$EDAD_r/5)*5
dbtQ$EDAD_q[dbtQ$EDAD_q > 89 & dbtQ$EDAD_q < 120] <- 90
dbtQ$EDAD_q[dbtQ$EDAD_r<5] <- 1
dbtQ$EDAD_q[dbtQ$EDAD_r==0] <- 0
table(dbtQ$EDAD_r,dbtQ$EDAD_q)
dbtQ = sqldf("select PROVRE, DEPRE, EDAD_q, sum(N) N, sum(d) d
             from dbtQ group by PROVRE, DEPRE, EDAD_q")
dbtQ$d[is.na(dbtQ$d)] <- 0


### Consistency Cheks
dbtQ$link = as.integer(paste0(str_pad(dbtQ$PROVRE, 2, pad = "0"), 
															str_pad(dbtQ$DEPRE, 3, pad = "0")))

## P/F ratio
# todo ok parece
# aclarar relación ejes, lo más preocupante es a la izquierda de la recta

## NBI
dbtQcLaMat = dbtQ
dbtQ = dbtQ[dbtQ$link!=6427,] # saco La Matanza
dbt = dbt[dbt$link!=6427,]

### Agrego Regionalizacion

# str(Cluster); length(unique(Cluster$link))
dbt = merge(dbt, Cluster, by=c('link'), all.x=T)
dbtQ = merge(dbtQ, Cluster, by=c('link'), all.x=T)

####obtengo ev de provincias para ver si es coherente con ev de INDEC

#datos a nivel provincia
dbtQ_Prov=sqldf("select PROVRE, EDAD_q, sum(N) N, sum(d) d 
                from dbtQ group by PROVRE, EDAD_q")

ages    <- c(0, 1, seq(5, 90, by = 5))
interv <- c(diff(ages), NA)
e0_prov = sapply(provs_codes, function(x) {LTabr(Deaths = dbtQ_Prov$d[dbtQ_Prov$PROVRE==x], 
                                      Exposures = dbtQ_Prov$N[dbtQ_Prov$PROVRE==x], 
                                      Age = ages, AgeInt =interv, axmethod = 'UN')$ex[1]})

#relación c INDEC
INDEC0810=c(75.18, 75.75, 74.98, 76.2, 75.1) # TM08-10
e0_prov-INDEC0810
Dif_e0_INDEC = data.frame(Prov=c('Buenos Aires', 'Cordoba', 'Entre Rios', 'La Pampa', 'Santa Fe'), 
													e0Raw = e0_prov, e0_INDEC = INDEC0810,
													DifRel = round((e0_prov-INDEC0810)/INDEC0810*100,2)) #ok!
#diferencias por: 2010 vs 2009, INDEC redistribuyo quizas de otra manera y hay ajustes por cobert
  
# regionalizacion

regionalizacion = dbt %>% select(Prov_Nombre, Dpto_Nombre, K) %>% 
                  distinct() %>% arrange(K)

######################################## Aplico Metodos

##### Estimación Bayesiana
### Bootstrap de tasas con Dist Poisson, para darle aleatoriedad
### distribucion a priori (non-informative) de lambda en cada área menor (Normal truncada en 0) 

EB_AM_Boot = c(Boot = 0, K = 0, link = 0,EDAD_q = 0,N = 0, mua = 0,muaEB = 0)
for (j in 1:1000) {
  # j = 1
  dbtQ_dep <- dbtQ
  # lambda a priori
  # dbtQ_dep$lamb_prior = pmax(rnorm(nrow(dbtQ_dep), dbtQ_dep$d, 1/2), rep(0,nrow(dbtQ_dep)))
  dbtQ_dep$lamb_prior = dbtQ_dep$d + .01
  dbtQ_dep$d <- rpois(nrow(dbtQ_dep), dbtQ_dep$lamb_prior)
  for (AM in unique(dbtQ$K)) {
                # AM=2
                EB_AM = BE(dbtQ_dep, AM)
                EB_AM_Boot = rbind(EB_AM_Boot, data.frame(Boot = j,EB_AM))
  }
  print(j)
}

EB_AM_Boot = EB_AM_Boot[EB_AM_Boot$Boot != 0,]
EB_AM_Boot$PROVRE = as.integer(substr(str_pad(EB_AM_Boot$link, 5, pad = "0"),1,2))
EB_AM_Boot$DEPRE = as.integer(substr(str_pad(EB_AM_Boot$link, 5, pad = "0"),3,5))
# str(EB_AM_Boot);summary(EB_AM_Boot)

# conf interval
EB_AM = group.CI(muaEB ~ AM + link + PROVRE + DEPRE + EDAD_q, data = EB_AM_Boot, ci = 0.95)

# ejemplo gráfico al azar
PROV_sample = sample(unique(dbtQ$PROVRE),1)
prov_graf = subset(dbtQ, PROVRE == PROV_sample)
DEP_sample = sample(unique(prov_graf$DEPRE),1)
depre_graf <- subset(prov_graf, DEPRE == DEP_sample)
eb_graf = EB_AM_Boot[EB_AM_Boot$PROVRE== PROV_sample & EB_AM_Boot$DEPRE==DEP_sample,]
ebCI_graf = EB_AM[EB_AM$PROVRE== PROV_sample & EB_AM$DEPRE==DEP_sample,]
# graph
plot(depre_graf$EDAD_q,log(depre_graf$d/depre_graf$N), ylim=c(-10,0), xlab='x',ylab='log(m)')
for (b in unique(EB_AM_Boot$Boot)) {
  # b=1
  lines(eb_graf$EDAD_q[eb_graf$Boot == b],log(eb_graf$muaEB[eb_graf$Boot == b]),col = 'grey')
  }
lines(ebCI_graf$EDAD_q,log(ebCI_graf$muaEB.upper),col = 'red',lty=2)
lines(ebCI_graf$EDAD_q,log(ebCI_graf$muaEB.lower),col = 'red',lty=3)
lines(ebCI_graf$EDAD_q,log(ebCI_graf$muaEB.mean),col = 'blue')


########################## Estimación indirecta

EI_AM =  c(K = 0, link = 0, EDAD_q = 0, N = 0, mua = 0,muaEI = 0)
for (AM in unique(dbtQ$K)) {
  EI_AM_i = EI(dbtQ, AM)
  EI_AM   = rbind(EI_AM,EI_AM_i)
}
EI_AM$PROVRE = as.integer(substr(str_pad(EI_AM$link, 5, pad = "0"),1,2))
EI_AM$DEPRE = as.integer(substr(str_pad(EI_AM$link, 5, pad = "0"),3,5))

# ejemplo gráfico al azar
PROV_sample = sample(unique(dbtQ$PROVRE),1)
prov_graf = subset(dbtQ, PROVRE == PROV_sample)
DEP_sample = sample(unique(prov_graf$DEPRE),1)
depre_graf <- subset(prov_graf, DEPRE == DEP_sample)
plot(depre_graf$EDAD_q,log(depre_graf$d/depre_graf$N), ylim=c(-10,0), xlab='x',ylab='log(m)')
lines(depre_graf$EDAD_q, log(EI_AM$muaEI[EI_AM$PROVRE==PROV_sample & EI_AM$DEPRE==DEP_sample]),col = 2)
      # ver provincia si no respeta su forma

######################### TOPALS
# standards: tener regularidad implica no tener standard por cada cluster
# this.std = dbt
# this.std$EDAD_r[this.std$EDAD_r>89] = 90 
this.std = sqldf("select K, EDAD_q, sum(d)/sum(N) as mx from dbtQ group by K, EDAD_q")
this.std = this.std[this.std$K!=0,]
this.std$logmx = log(this.std$mx)
this.std = this.std[order(this.std$K, this.std$EDAD_q),]
# perfiles de clusters
plot(NA, NA, xlim=c(0,90), ylim=c(-10,0))
for (k in unique(this.std$K)) lines(this.std$EDAD_q[this.std$K==k], this.std$logmx[this.std$K==k], col=k)

# estimates
dbtQ = dbtQ[order(dbtQ$K, dbtQ$EDAD_q),]
ET_AM =  c(K = 0, link = 0, EDAD_q = 0, N = 0, mua = 0, muaET = 0)
for (AM in unique(dbtQ$K)) {
  # AM=1
  ET_AM_i = TOPALS(dbtQ, AM, this.std$logmx[this.std$K==AM])
  ET_AM   = rbind(ET_AM, ET_AM_i)
}
ET_AM = ET_AM[ET_AM$K != 0,]
ET_AM$PROVRE = as.integer(substr(str_pad(ET_AM$link, 5, pad = "0"),1,2))
ET_AM$DEPRE = as.integer(substr(str_pad(ET_AM$link, 5, pad = "0"),3,5))

#compare in choose standard
# this.std  = HMDstd[age+1,this.sex]
# ET_AM_i = TOPALS(dbtQ,AM)
# plot(unique(ET_AM_i$EDAD_q),ET_AM_i$muaTop[ET_AM_i$DEPRE==am]]
# ET_AM_i = TOPALS(dbtQ,AM)
# points(unique(ET_AM_i$EDAD_q),ET_AM_i$muaTop[ET_AM_i$DEPRE==am],col=2)

# ejemplo gráfico al azar
PROV_sample = sample(unique(dbt$PROVRE),1)
prov_graf = subset(dbt, PROVRE == PROV_sample)
DEP_sample = sample(unique(prov_graf$DEPRE),1)
depre_graf <- subset(prov_graf, DEPRE == DEP_sample)
par(mfrow = c(1,1))
plot(depre_graf$EDAD_r,log(depre_graf$d/depre_graf$N), ylim=c(-10,0), xlab='x',ylab='log(m)')
lines(ET_AM$EDAD_q[ET_AM$PROVRE==PROV_sample & ET_AM$DEPRE==DEP_sample], 
			ET_AM$muaTop[ET_AM$PROVRE==PROV_sample & ET_AM$DEPRE==DEP_sample], col = 2, t="o")

############# ejemplo GRAL al azar
PROV_sample = sample(unique(dbtQ$PROVRE),1)
prov_graf = subset(dbtQ, PROVRE == PROV_sample)
DEP_sample = sample(unique(prov_graf$DEPRE),1)
depre_graf <- subset(prov_graf, DEPRE == DEP_sample)
ages_mp = c(.5,2,rep(2.5,length(ages)-3),0)
ages_mp = ages+ages_mp
plot(ages_mp,log(depre_graf$d/depre_graf$N), ylim=c(-10,0), xlab='x',ylab='log(m)',pch=19)
lines(ages_mp,log(EB_AM$muaEB.mean[EB_AM$PROVRE==PROV_sample & EB_AM$DEPRE==DEP_sample]),col = 4,t="o",cex=.7)
lines(ages_mp, log(EI_AM$muaEI[EI_AM$PROVRE==PROV_sample & EI_AM$DEPRE==DEP_sample]),col = 8,t="o",cex=.7)
lines(ages_mp, ET_AM$muaTop[ET_AM$PROVRE==PROV_sample & ET_AM$DEPRE==DEP_sample],col = 2,t="o",cex=.7)
legend("topleft", c("EB", "EI", "TP"), lty=1, col=c(4,8,2), cex=.8, bty="n")


#################################################### Graficos

########### Ajuste Gral

#order dbtQ
dbtQ=dbtQ[order(dbtQ$PROVRE,dbtQ$DEPRE,dbtQ$EDAD_q),]

pdf("analysis/plots/Ajuste2.pdf")
par(mfrow=c(2,2))
for (i in sample(unique(dbtQ$link),4)){
  
  #i=6175
  
  NombrePROV=unique(dbt$Prov_Nombre[dbt$link==i])
  NombreDPTO=unique(dbt$Dpto_Nombre[dbt$link==i])
  N_prov = sum(dbt$N[dbt$Prov_Nombre==NombrePROV])

  #estimaciones
  EI_AM_i= subset(EI_AM, link==i)
  EB_AM_i= subset(EB_AM, link==i)
  ET_AM_i = subset(ET_AM, link==i)
  
  colsGraph = c('coral2', 'goldenrod2', 'darkorchid1')
  
  #graf
  steps = interv <- c(diff(ages), 5)
  plot(ages_mp,ET_AM_i$mua,cex=0.5,xlab="Age",ylab="log(m)",ylim = c(-10,0), pch=19)
  rug(ET_AM_i$EDAD_q[ET_AM_i$d==0],col = 8, lwd=2)
  lines(ages,log(EI_AM_i$muaEI),col=alpha(colsGraph[2], .5), lwd=2, type="s")
  lines(ages,log(EB_AM_i$muaEB.mean),col=alpha(colsGraph[1], .5), lwd=2, type="s")
  lines(ages,ET_AM_i$muaTop,cex=0.8, col=alpha(colsGraph[3], .5), lwd=2, type="s")
      
  # text(x=15,y=-2, paste0("evBE=",round(evEBm,1)),cex = .7)
  legend(x=60, y=-7, c("Dpto Obs", "Bayes Emp.", "Indirect Est.", "TOPALS"),
             col = c(1, alpha(colsGraph, .5)), box.lwd=0,
             lty = c(NA, 1, 1, 1), pch = c(19, NA, NA, NA),
             cex=0.6, box.lty=0, lwd=c(NA,3,3,3))
  text(x=20, y=-1, paste0("Población: ", round(sum(EI_AM_i$N),0),
                          "\n% Provincia: ", round(sum(EI_AM_i$N)/N_prov*100,1), "%"),cex=.7)
  if(length(ET_AM_i$EDAD_q[ET_AM_i$d==0]>0)) {mtext("Cero muertes",col="grey",cex=.5,at = c(10),side=1)}
  mtext(paste0("Dpto. ", NombreDPTO,", ",NombrePROV), side=3, cex=.9)
  print(i)
}
dev.off()
par(mfrow=c(1,1))


##### Esp de vida y funciones de tabla

EB_AM_Boot_LT = data.frame(Boot=0, AM=0, link=0, EDAD_q=0, N =0, mua =0, muaEB=0, 
													 PROVRE=0, DEPRE=0, ex=0, qx=0)
Comparison_ex0 =  data.frame(link=0, e0BE=0, e0EI=0, e0ET=0) 

# loop cada dpto
ya_corridos = NA
for (i in (unique(EB_AM_Boot$link)[unique(EB_AM_Boot$link) %notin% ya_corridos])){
  # i =  42091
  EB_AM_i = subset(EB_AM_Boot, link==i & Boot<501)
  EI_AM_i= subset(EI_AM, link==i)
  ET_AM_i = subset(ET_AM, link==i)

  # get bootstrap distributions EB
  EB_AM_i$ex = as.numeric(sapply(1:500, function(boot)
														  	LTabr(nMx = EB_AM_i$muaEB[EB_AM_i$Boot==boot],
														  				Age = ages, AgeInt =interv, axmethod = 'UN')$ex))
  EB_AM_i$qx = as.numeric(sapply(1:500, function(boot)
                                LTabr(nMx = EB_AM_i$muaEB[EB_AM_i$Boot==boot],
                                      Age = ages, AgeInt =interv, axmethod = 'UN')$nqx))
  
  EB_AM_Boot_LT = rbind(EB_AM_Boot_LT, EB_AM_i)
  # comparison e0
  e0EB = mean(EB_AM_i$ex[seq(1, length(unique(EB_AM_i$Boot))*20, 20)])
  e0EI = LTabr(nMx = EI_AM_i$muaEI, Age = ages, AgeInt =interv, axmethod = 'UN')[1,10]
  e0ET = LTabr(nMx = exp(ET_AM_i$muaTop), Age = ages, AgeInt =interv, axmethod = 'UN')[1,10]
  Comparison_ex0 = rbind(Comparison_ex0, c(i, e0EB, e0EI, e0ET))
  print(i)
}
Compar_ex0 = Comparison_ex0[-1,]
EB_AM_Boot_LT = EB_AM_Boot_LT[-1,]

# intervalos de confianza
# link PROVRE DEPRE EDAD_q ex.upper  ex.mean ex.lower

EB_AM_exIC = EB_AM_Boot_LT %>% 
	group_by(link, PROVRE, DEPRE, EDAD_q) %>% 
	dplyr::summarize(ex.mean = mean(ex, na.rm = T),
									 ex.upper = quantile(ex, probs = .025),
									 ex.median = quantile(ex, probs = .5),
									 ex.lower = quantile(ex, probs = .975))

EB_AM_nqxIC = EB_AM_Boot_LT %>% 
	group_by(link, PROVRE, DEPRE, EDAD_q) %>% 
	dplyr::summarize(qx.mean = mean(qx, na.rm = T),
									 qx.upper = quantile(qx, probs = .025),
									 qx.median = quantile(qx, probs = .5),
									 qx.lower = quantile(qx, probs = .975))

# comparison between methods: differences in e0 and correlation
# head(Compar_ex0)
# head(EB_AM_exIC)
CorrMeth = cor(Compar_ex0[,2:4]) 
Compar_ex0$PROV = substr(str_pad(Compar_ex0[,1], 5, pad = "0"),1,2)
Compar_ex0$`EB-TOP` = Compar_ex0$e0BE-Compar_ex0$e0ET 
Compar_ex0$`EB-EI` = Compar_ex0$e0BE-Compar_ex0$e0EI 
Compar_ex0$`EI-TOP` = Compar_ex0$e0EI-Compar_ex0$e0ET# scatter graph
Compar_ex0 = Compar_ex0[order(Compar_ex0$`EB-TOP`),]
Compar_ex0$id = 1:nrow(Compar_ex0) 
Rango = range(Compar_ex0$e0BE, Compar_ex0$e0ET, Compar_ex0$e0EI)
pdf("analysis/plots/CompMethods.pdf")
plot(Compar_ex0$e0BE, ylab='e(0)', xlab = "Departamento",xaxt='n', pch=15, cex=.9, 
     ylim=Rango, col = alpha(colsGraph[1], .2))
points(Compar_ex0$e0EI, pch=15, cex=.9, col = alpha(colsGraph[2], .2))
points(Compar_ex0$e0ET, pch=15, cex=.9, col = alpha(colsGraph[3], .2))
lines(predict(loess(e0BE~id, Compar_ex0), 1:nrow(Compar_ex0)), lty=2, lwd = 2, col = colsGraph[1])
lines(predict(loess(e0EI~id, Compar_ex0), 1:nrow(Compar_ex0)), lty=2, col = colsGraph[2])
lines(predict(loess(e0ET~id, Compar_ex0), 1:nrow(Compar_ex0)), lty=2, col = colsGraph[3])
legend('topright', bty = 'n', cex=.9,
			 c('Bayes Emp', 'Indirecto', 'TOPALS'), pch=19, col=colsGraph)
dev.off()

##### Consistencia con AM
estimaciones_am <-  EI_AM %>% select(K, link, N, EDAD_q, muaEI) %>% slice(-1) %>%  
										left_join(EB_AM %>% select(link, EDAD_q, muaEB.mean), by=c("link","EDAD_q"))  %>%
										left_join(ET_AM %>% mutate(muaTop = exp(muaTop)) %>% select(link, EDAD_q, muaTop), by=c("link","EDAD_q"))
estimaciones_am <- estimaciones_am %>% filter(!is.na(K)) %>% group_by(K, EDAD_q) %>% 
																						summarise(D_EI=sum(N*muaEI), 
																											D_EB=sum(N*muaEB.mean), 
																											D_ET=sum(N*muaTop))
estimaciones_AM <- dbtQ %>% filter(!is.na(K)) %>% group_by(K, EDAD_q) %>% summarise(d = sum(d))
diferencias <- estimaciones_AM %>% left_join(estimaciones_am) %>% 
								mutate(dif_EI = (D_EI/d-1)*100, dif_EB = (D_EB/d-1)*100, dif_ET = (D_ET/d-1)*100) %>% 
								ungroup()
maximos <- rbind(diferencias %>% arrange(-dif_EI) %>% slice(1) %>% mutate(Met = "EI") %>% select(Met, EDAD_q, dif = dif_EI),
								 diferencias %>% arrange(-dif_EB) %>% slice(1) %>% mutate(Met = "EB") %>% select(Met, EDAD_q, dif = dif_EB),
								 diferencias %>% arrange(-dif_ET) %>% slice(1) %>% mutate(Met = "ET") %>% select(Met, EDAD_q, dif = dif_ET))
prom <- cbind(round(diferencias %>% summarise(dif_EI = mean(dif_EI, na.rm = T)),1),
						 	round(diferencias %>% summarise(dif_EB = mean(dif_EB, na.rm = T)),1),
						 	round(diferencias %>% summarise(dif_ET = mean(dif_ET, na.rm = T)),1))

pdf("analysis/plots/ConsistAM.pdf")
print(
	ggplot(diferencias %>% select(-d,-4,-5,-6) %>% 
	         rename(Indirecta=3, "Bayes Emp."=4, "TOPALS"=5) %>% 
	         gather(Met, Value, -c(1:2)),
			 aes(EDAD_q, Value, col = Met)) + geom_line() + 
			 theme_bw() + facet_grid(~K) +
	     theme(strip.background = element_rect(fill="#FFFFFF"),
	           legend.title = element_blank()) +
			 scale_y_continuous(name = "%") + 
	     scale_x_continuous(name = "Edad")
	)
dev.off()

# graf w problems
problems = Compar_ex0[Compar_ex0$`EB-TOP`<(-4),]$link
nombre_problems = unique(dbt$Dpto_Nombre[dbt$link %in% problems])
ages_mp = c(.5,2,rep(2.5,length(ages)-3),0)
ages_mp = ages+ages_mp

pdf("analysis/plots/AjusteFeos2.pdf")
par(mfrow=c(2,2))
for (i in problems[1:4]){
  NombrePROV=unique(dbt$Prov_Nombre[dbt$link==i])
  NombreDPTO=unique(dbt$Dpto_Nombre[dbt$link==i])
  N_prov = sum(dbt$N[dbt$Prov_Nombre==NombrePROV])
  #estimaciones
  EI_AM_i= subset(EI_AM, link==i)
  EB_AM_i= subset(EB_AM, link==i)
  ET_AM_i = subset(ET_AM, link==i)
  colsGraph = c('coral2', 'goldenrod2', 'darkorchid1')
  #graf
  steps = interv <- c(diff(ages), 5)
  plot(ages_mp,ET_AM_i$mua,cex=0.5,xlab="Age",ylab="log(m)",ylim = c(-10,0), pch=19)
  rug(ET_AM_i$EDAD_q[ET_AM_i$d==0],col = 8, lwd=2)
  lines(ages,log(EI_AM_i$muaEI),col=alpha(colsGraph[2], .5), lwd=2, type="s")
  lines(ages,log(EB_AM_i$muaEB.mean),col=alpha(colsGraph[1], .5), lwd=2, type="s")
  lines(ages,ET_AM_i$muaTop,cex=0.8, col=alpha(colsGraph[3], .5), lwd=2, type="s")
  
  # text(x=15,y=-2, paste0("evBE=",round(evEBm,1)),cex = .7)
  legend(x=60, y=-7, c("Observado", "Bayes Emp.", "Indirecta", "TOPALS regr."),
  			 col = c(1, alpha(colsGraph, .5)), box.lwd=0,
  			 lty = c(NA, 1, 1, 1), pch = c(1, NA, NA, NA),
  			 cex=0.6, box.lty=0, lwd=c(NA,3,3,3))
  text(x=20, y=-1, paste0("Población: ", round(sum(EI_AM_i$N),0),
  												"\n% Provincia: ", round(sum(EI_AM_i$N)/N_prov*100,1), "%"),cex=.7)
  if(length(ET_AM_i$EDAD_q[ET_AM_i$d==0]>0)) {mtext("Cero deaths",col="grey",cex=.5,at = c(10),side=1)}
  mtext(paste0("Dpto. ", NombreDPTO,", ",NombrePROV), side=3, cex=.9)}
dev.off()


#########graf despersión de EV por cada PROV
for (AMay in provs_codes){
# AMay=6 
NombrePROV=unique(dbt$Prov_Nombre[dbt$PROVRE==AMay])
### esp de vida
Edad_ex = 0
EB_AM_exIC_x = EB_AM_exIC[EB_AM_exIC$PROVRE==AMay & EB_AM_exIC$EDAD_q==Edad_ex,]
EB_AM_exIC_x = merge(EB_AM_exIC_x,nombres, by.x = c("PROVRE","DEPRE"),
										 											 by.y = c("Prov_cod","Dpto_cod"),all.x = T)
EB_AM_exIC_x$link = EB_AM_exIC_x$link.x 
EB_AM_exIC_x = merge(EB_AM_exIC_x,EI_AM[EI_AM$EDAD_q==0,c(2,4)],by=c('link'),all.x=T)
EB_AM_exIC_x$DEPRE = as.character(EB_AM_exIC_x$DEPRE)
EB_AM_exIC_x$DEPRE = factor(EB_AM_exIC_x$DEPRE, levels=EB_AM_exIC_x[order(EB_AM_exIC_x$ex.mean),3])
pdf(paste0("plots/", NombrePROV, ".pdf"))
  print(ggplot(data=EB_AM_exIC_x, aes(x=ex.mean, y=DEPRE)) +
            xlab('e(0)') + ylab('Department') +
            scale_x_continuous(limits=c(72,80.5),breaks=72:80, minor_breaks =NULL) +
            geom_point(color=2, size=1) +
            geom_segment(aes(x=ex.lower, y=DEPRE, xend=ex.upper, yend=DEPRE), color=alpha("red",.5), lwd=.4) +
            geom_text(aes(x=ex.upper+1,y=DEPRE,label=Dpto_Nombre, size = N/sum(EB_AM_exIC_x$N)),
                      hjust = 0, nudge_x = 0.01) + scale_radius(range = c(1.5,8)) +
            theme_bw()+ theme(title=element_text(face='bold'), 
                                      axis.line.y=element_blank(),
                                      axis.text.y=element_blank(),
                                      axis.ticks.y=element_blank(),
                                      panel.grid.minor.y=element_blank(),
                                      panel.grid.major.y=element_blank())+
            theme(legend.position="none")+
            ggtitle(ifelse(AMay==6,"Buenos Aires (s/ La Matanza)",NombrePROV))
  			)
 dev.off()
}

########### Bs As

dptos_BA_codes = c(6756, 6357, 6560)
dptos_BA_names = c(paste0("San Isidro e(0)=",round(Compar_ex0$e0BE[Compar_ex0$link==dptos_BA_codes[1]],1)), 
									 paste0("Gral. Pueyrredón e(0)=",round(Compar_ex0$e0BE[Compar_ex0$link==dptos_BA_codes[2]],1)), 
									 paste0("Moreno e(0)=",round(Compar_ex0$e0BE[Compar_ex0$link==dptos_BA_codes[3]],1)))

# graph max min with CI
pdf("plots/SanIsyMoreno.pdf")
plot(NULL, NULL, xlim=c(0,85), ylim=c(-8.5,0), ylab="log(m)", xlab="Edad")
polygon(x = c(rev(ages_mp),ages_mp),
				y = c(rev(log(EB_AM_nqxIC$qx.lower[EB_AM_nqxIC$link==dptos_BA_codes[1]])),
							log(EB_AM_nqxIC$qx.upper[EB_AM_nqxIC$link==dptos_BA_codes[1]])),
				col = alpha(colsGraph[1], .1), border = NA)
points(ages_mp,log(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$link==dptos_BA_codes[1]]), 
			 col=colsGraph[1], pch = 19, type="o",
			 xlab="Age", ylab="log(q)")
polygon(x = c(rev(ages_mp),ages_mp),
				y = c(rev(log(EB_AM_nqxIC$qx.lower[EB_AM_nqxIC$link==dptos_BA_codes[3]])),
							log(EB_AM_nqxIC$qx.upper[EB_AM_nqxIC$link==dptos_BA_codes[3]])),
				col = alpha(colsGraph[3], .1), border = NA)
points(ages_mp,log(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$link==dptos_BA_codes[3]]), col=colsGraph[3], pch = 19, type="o",
			 xlab="Age", ylab="log(q)")
polygon(x = c(rev(ages_mp),ages_mp),
				y = c(rev(log(EB_AM_nqxIC$qx.lower[EB_AM_nqxIC$link==dptos_BA_codes[2]])),
							log(EB_AM_nqxIC$qx.upper[EB_AM_nqxIC$link==dptos_BA_codes[2]])),
				col = alpha(colsGraph[2], .1), border = NA)
points(ages_mp,log(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$link==dptos_BA_codes[2]]), col=colsGraph[2], pch = 19, type="o",
			 xlab="Age", ylab="log(q)")
legend("bottomright", dptos_BA_names[c(1,2,3)], col=colsGraph[c(1,2,3)], lty=1, pch=19, bty = "n", cex=.8)
dev.off()
	
#  ver datos crudos - OK
plot(NA, NA, ylim=c(-10,0), xlim=c(0,90))
for (i in 1:4){
	depre_g <- subset(dbtQ, link %in% dptos_BA_codes[i])
	points(depre_g$EDAD_q,log(depre_g$d/depre_g$N), col = i,
			 type = "p", ylab = paste("log(m)"), xlab = "x")}


############ HETEROGENEIDAD ANALISIS

ineq_df <- EB_AM_exIC %>% filter(EDAD_q==0) %>% 
														group_by(PROVRE) %>% 
														dplyr::summarise(mean = mean(ex.mean),
																						 	n_dptos = n() + 1, # no sé donde perdí uno 
																							variance = var(ex.mean),
																							cv = round(variance^.5/mean*100,1),
																							rango = diff(range(ex.mean)))



######### table final

table_e0_puntual <- Compar_ex0 %>% 
                    mutate(e0BE=round(e0BE,1), e0EI=round(e0EI,1), e0ET=round(e0ET,1)) %>% 
                    left_join(nombres %>% 
                                select(Prov_Nombre, link, Dpto_Nombre)) %>% 
                    select(Prov_Nombre, Dpto_Nombre, e0BE, e0EI, e0ET) %>% 
                    arrange(Prov_Nombre, Dpto_Nombre)
                    

###############mapa
# item=2
# evMap=data.frame(PROV=0,DEP=0,mean=0,p5=0,p95=0)
# for (AMay in provs){
#   #AMay=10
#   NombrePROV=unique(dbtQ$Prov_Nombre[dbtQ$PROVRE==AMay])
#   ev1=ev_list[[item]]
#   ev1_st=data.frame(PROV=rep(AMay,length(colnames(ev1))),
#                     DEP=as.numeric(as.character(colnames(ev1))),
#                     mean=apply(ev1,2,mean),
#                     p5=apply(ev1,2,quantile, probs = c(0.05)),
#                     p95=apply(ev1,2,quantile, probs = c(0.95)))
#   evMap=rbind(evMap,ev1_st)
#   item=item+1}
# evMap=evMap[-1,]
# #shapes
# library("maps")
# library("maptools")
# library("sp")
# library("raster")
# library("ggplot2")
# library("RColorBrewer")
# library("rgdal")
# #leo shape a nivel departamental PROBLEM HERE
# Arg <- rgdal::readOGR("analysis/data/pxdptodatosok.shp")
# class(Arg)
# Arg$PROV=as.numeric(substr(Arg$link,1,2))
# Arg$DEP=as.numeric(substr(Arg$link,3,5))
# head(Arg);tail(Arg)
# #merge
# Arg2 <- merge(Arg,evMap,by=c("PROV","DEP"),all.x=T)
# head(Arg2)
# my.palette <- brewer.pal(n = 7, name = "Reds")
# BA <- spplot(Arg2[Arg2$PROV==6,], "mean", col.regions = my.palette, cuts = 6)
# P24 <- spplot(Arg2[Arg2$PROV==24,], "mean", col.regions = my.palette,cuts = 6)
# Cat <- spplot(Arg2[Arg2$PROV==10,], "mean", col.regions = my.palette,cuts = 6)
# Cor <- spplot(Arg2[Arg2$PROV==14,], "mean", col.regions = my.palette,cuts = 6)
# Salt <- spplot(Arg2[Arg2$PROV==66,], "mean", col.regions = my.palette,cuts = 6)
# Neuq <- spplot(Arg2[Arg2$PROV==58,], "mean", col.regions = my.palette,cuts = 6)
# grid.arrange(BA, P24, Cat, Cor, Salt, Neuq, nrow=3 ,ncol= 2) 
# ####habría que poner misma escala y dejar solo una leyenda