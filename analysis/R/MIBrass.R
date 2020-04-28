BrassCM<-function(data, mlt = "UN - Latin America", xm = 27.93,  Cd=2010.907){

## Following IUSSP stages	
## Assumptions
    # modelo de estructura fec y mort vigente
    # mi no varia por edad de madre
    # no corr entre mortalidad de mujeres e infantil (sesgo sobrevivientes)
    # cambio reciente gradual y unidireccional
  
## handle erros
		#Data frame con: x, Nw, Ceb, Cba
		# table 'UN - General' | 'UN - Latin America' | 'Princeton - West' | 'Princeton - South'
		# grupos quinquenales
    #paridad creciente c edad
    #% difuntos creciente con edad
  
# data ejemplo IUSSP, ya divididos por numero de mujeres
# data = data.frame(x = seq(15, 45, 5),
# 									Ceb = c(0.2833, 1.5316,	2.8487,	4.1853,	5.2136,	6.0343,	6.4527),
# 									Cba = c(0.2540, 1.3562, 2.4698,	3.5170,	4.2500,	4.7152,	4.8666))
	
# Calculate proportions dead of children ever born
data$PD=(data$Ceb-data$Cba)/data$Ceb

# Select a model life table family
lt = read.csv('data/IM/mlt.csv')
lt=lt[lt$Tabla==mlt,]

# Estimate nq0 from each 5PDx
ParityRatios = c(data$Ceb[data$x==15]/data$Ceb[data$x==20], data$Ceb[data$x==20]/data$Ceb[data$x==25])

library(xlsx)
tCoef = read.csv('data/IM/Coeff.csv')
tCoef=tCoef[tCoef$Tabla==mlt,]

#mortality n period
qn = (tCoef$a + tCoef$b * ParityRatios[1] + tCoef$c * ParityRatios[2] + tCoef$d * xm) * data$PD

#time reference
tr = Cd - (tCoef$e + tCoef$f * ParityRatios[1] + tCoef$g * ParityRatios[2])

# final estimate 
alpha= .5 * (log(qn/(1-qn)))- lt$Ambos[1:7]
q05= exp(2*(alpha+lt$Ambos[lt$x==5])) / (1+exp(2*(alpha+lt$Ambos[lt$x==5])))
q01= exp(2*(alpha+lt$Ambos[lt$x==1])) / (1+exp(2*(alpha+lt$Ambos[lt$x==1])))
cm = cbind(time = tr, q01 = q01, q05 = q05)
return(cm)
}


#Data y coeficientes y tabla
dataT = read.table("data/IM/DatosC2010.csv", header = T, sep = ',' )
colnames(dataT) = c('dpto', 'x', 'Nw', 'Ceb', 'Cba')
dataT$Ceb = dataT$Ceb/dataT$Nw 
dataT$Cba = dataT$Cba/dataT$Nw 
dataT$Nw = NULL

# edades medias
emedias = read.csv("data/IM/NacWeb10.csv")
emedias = xtabs(CUENTA~IMEDAD+PROVRES, emedias, PROVRES %in% provs_codes)
x_me = c(15, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, NA)
x_me = data.frame(PROV = provs_codes, 
									x_me = colSums(emedias*x_me, na.rm = T)/colSums(emedias[-9,]))

# t de obs
t2010 = 2010 + as.numeric(as.Date("2010/10/27") - as.Date("2010/01/01"))/365

# estimates
BrassIM = data.frame(dpto = NA, time = NA, q01 = NA, q05 = NA)
for (dpto in unique(dataT$dpto)){
	# dpto = 42028
	xm = x_me$x_me[which(x_me$PROV==trunc(dpto/1000))] 
	data = dataT[dataT$dpto==dpto & dataT$x>14 & dataT$x<50,]
	Bsubp = BrassCM(data, mlt = "UN - Latin America", xm = xm, Cd = t2010)
	BrassIM = rbind(BrassIM, data.frame(dpto = dpto, Bsubp))
}  
BrassIM = BrassIM[!is.na(BrassIM$q01),] 
BrassIM$time = trunc(BrassIM$time)
table(BrassIM$dpto, BrassIM$time)
barplot(sort(BrassIM$q01[BrassIM$time>2005]))
BrassIM$link = BrassIM$dpto

# merge indicadores
str(dbtQ)
dbtQ_IM = dbtQ[dbtQ$EDAD_q==0,]
dbtQ_IM$m = dbtQ_IM$d/dbtQ_IM$N
ChekPF = merge(BrassIM, dbtQ_IM[,c('link', 'm')], by = 'link', x.all = T)

# add pop
ChekPF = merge(ChekPF, pNBI[,c('link', 'Total')], by='link', x.all=T)
ChekPF$Nscale = scale(ChekPF$Total)

ChekPF = ChekPF[ChekPF$time>2005 & ChekPF$time<2009,]
LAMA = which(ChekPF$link==6427)
ROSA = which(ChekPF$link==82084)

#graf
pdf('analysis/plots/ChekPF.pdf')
par(mfrow=c(1,2), oma = c(2, 2, 2, 2), # two rows of text at the outer left and bottom margin
		mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
		mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
		xpd = NA)
plot(ChekPF$q01[-c(LAMA,ROSA)], ChekPF$m[-c(LAMA,ROSA)], pch=16, #cex=ChekPF$Nscale[-c(LAMA,ROSA)]*2, 
		 xlim=c(0, .04), ylim=c(0, .04), col=alpha(cols[4], 0.2), xaxs="i",yaxs="i", 
		 xlab='q(0) estimación indirecta', ylab='m(0) observado', xaxt="n", yaxt="n")
axis(1, at=seq(0, .04, by=.01), labels=seq(0, .04, by=.01), cex.axis=.8)
axis(2, at=seq(0, .04, by=.01), labels=seq(0, .04, by=.01), cex.axis=.8)
points(ChekPF$q01[c(LAMA,ROSA)], ChekPF$m[c(LAMA,ROSA)], pch=16, #cex=ChekPF$Nscale[c(LAMA,ROSA)]*2, 
			 col=alpha(cols[2], 0.2))
lines(c(0,.04), c(0,.04), col='grey', lty=2, xlim=c(0,.04), ylim=c(0,.04))
text(.03, .01, 'S/ ponderar', col='grey')
plot(ChekPF$q01[-c(LAMA,ROSA)], ChekPF$m[-c(LAMA,ROSA)], pch=16, cex=ChekPF$Nscale[-c(LAMA,ROSA)]*2, 
		 xlim=c(0, .04), ylim=c(0, .04),  col=alpha(cols[4], 0.2), 
		 xaxs="i",yaxs="i", xlab='q(0) obs', ylab='', xaxt="n", yaxt="n")
axis(1, at=seq(0, .04, by=.01), labels=seq(0, .04, by=.01), cex.axis=.8)
points(ChekPF$q01[c(LAMA,ROSA)], ChekPF$m[c(LAMA,ROSA)], pch=16, cex=ChekPF$Nscale[c(LAMA,ROSA)]*2, col=alpha(cols[2], 0.2))
lines(c(0,.04), c(0,.04), col='grey', lty=2, xlim=c(0,.04), ylim=c(0,.04))
text(.03, .01, 'Ponderado', col='grey')
text(.006, .012, 'Rosario \n La Matanza', col=alpha(cols[2], 0.5))
dev.off()
