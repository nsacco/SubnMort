### Metodología Schmertmann

source("data/pob_expuestos/exp_Schmertmann/exposure_calculation.R") 

########################################### Data

# POBLACIÓN

# Levanta base de datos original de población censal (2010) en las provincias elegigas. Octubre-2010. 
# Ajusta poblacion por conciliada
# Luego, en vez de multiplicar por tres para dividir las tasas de los tres años, se aplica el ajuste propuesto por Schmertman, 
# usando una tabla de mortalidad: cada cohorte se sobrevive y se mata para computar años-persona en cada edad simple.
# Cuál es la base original de estos datos? Están tomados del Redatam Versión online del cuestinario básico. Si!

db_p <- readxl::read_xlsx("data/pob_expuestos/DptosOk_TProv.xlsx", sheet = 'N',col_names = T)

# Chekeo respecto a datos (tabulados) publicados. Ej: suma total de población provincias. 
head(db_p); summary(db_p); unique(db_p$x) # NAs son Total en titulos
db_p = db_p[db_p$x!="Total" & !is.na(db_p$x),]
db_p$TOTAL = as.integer(db_p$Total)
db_p$Total = NULL
sum(db_p$TOTAL) #ok

# creo provincia y dpto
db_p$COD = str_pad(db_p$COD, 5, pad = "0")
db_p$PROV = as.integer(substr(db_p$COD,1,2))
db_p$DEPRE = as.integer(substr(db_p$COD,3,5))
db_p$EDAD = as.integer(db_p$x)

# Ajustar total segun conciliada. Cambia fecha de referencia a 1/7! Solo me quedo con provincias pampeanas
db_conc = read.xlsx('data/pob_expuestos/PobConciliada/PobConc.xlsx', 
										sheetIndex = 1, encoding = 'UTF-8')
db_conc$COD = str_pad(db_conc$COD, 5, pad = "0")
db_pT = sqldf('select COD, sum(TOTAL) sumTotal from db_p group by COD')
db_p = merge(db_p, db_pT, by = 'COD', all.x = T)
db_p = merge(db_p, db_conc[,c('COD','N')], by = 'COD', all.x = T)
db_p$Naj = db_p$TOTAL/db_p$sumTotal*db_p$N 
db_p = db_p[,c('PROV', 'DEPRE', 'NOM', 'EDAD', 'Naj')]
colnames(db_p) = c('PROV', 'DEPRE', 'NOM', 'EDAD', 'TOTAL')
db_p = db_p[!is.na(db_p$TOTAL) & db_p$PROV %in% provs_codes,]
summary(db_p); unique(db_p$PROV)

# Se ajusta la exposición de manera no lineal a los menores de 100. Los mayores se suman luego
db_pP <- db_p[db_p$EDAD < 100,]
sum(db_pP$TOTAL)

#Centenarios
db_pC = db_p[db_p$EDAD >= 100,] #centenarios en 2010
sum(db_pC$TOTAL)

#Se calcula exposición

p = data.frame(PROV = 0, DEPRE = 0, EDAD = 0, N = 0) # arma un data frame en 0. Ver armar data.frame vacío

for (prov in unique(db_pP$PROV)) {
  for (dpto in unique(db_pP$DEPRE[db_pP$PROV == prov])) { #se asigna a cada prov se aplica la función y ajusan los expuestos. En lugar de multiplicar *3 se ajusta de acuerdo a la propuesta del Carl. S.
    #prov=6;dpto=35
    tmp = db_pP %>% filter(PROV == prov, DEPRE == dpto)
    allx = union_all(tmp[, c("EDAD", "TOTAL")], data.frame(EDAD = 0:99, TOTAL = rep(0,100)))
    tmp = sqldf("select EDAD, sum(TOTAL) TOTAL from allx group by EDAD")
    N = tmp$TOTAL
    #length(N);dim(Em)
    p_i = Em %*% N
    p_i = data.frame(PROV = prov, DEPRE = dpto, EDAD = 0:99, N = p_i)
    p <- union_all(p,p_i)
  }
}
p = p[p$PROV != 0,]

#graf 4 deptos al azar para ver efecto (misma estética que Carl Schm)
#(regresión local loess() funciona muuuy parecido)
# get back names first
pNames = merge(p, unique(db_pP[,c('PROV', 'DEPRE', 'NOM')]), by=c('PROV', 'DEPRE'), all.x = T)

pdf('analysis/plots/AdjExp2.pdf')
par(mfrow = c(1,2))
for (i in 1:2) {
  #i=1
  prov = sample(unique(pNames$PROV),1)
  nomprov = provs_names_codes[provs_names_codes$provs_codes==prov, 2]
  dpto = sample(unique(pNames$DEPRE[pNames$PROV == prov]), 1)
  tmp = db_pP[db_pP$PROV == prov & db_pP$DEPRE == dpto,]
  plot(tmp$EDAD, tmp$TOTAL*3, lwd = 2, xlab = "Edad", ylab = "Población", t='p', pch=19, cex=.8)
  lines(0:99, pNames$N[pNames$PROV == prov & pNames$DEPRE == dpto], lwd = 3, col = 4)
  title(paste("Prov", nomprov, ". Dpto ", unique(tmp$NOM)), cex.main = .8)
  abline(v = seq(0,100,10), lty = 2, lwd=1, col='grey')
  NLocal=loess(TOTAL*3~EDAD,span=.1,data=tmp)
  lines(0:99, predict(NLocal, data.frame(EDAD=0:99)), col="red", lwd=2)
  legend('topright', c('Observado', 'Método Sobrev.', 'LOESS'), col=c(1,4,2), 
         lwd=c(NA,3,2), pch=c(1,NA,NA), bty = 'n', cex=.7)
} 
dev.off()
par(mfrow = c(1,1))

#Se suman centenarios, grupo 100+
db_pC$N = db_pC$TOTAL
p = union_all(p, cbind(db_pC[,c('PROV', 'DEPRE', 'EDAD', 'N')], EDAD = rep(100, nrow(db_pC))))
p$N[p$EDAD==100] = p$N[p$EDAD==100] * 2.5 # las Tablas total pais INDEC dan e(100)=2.9

#cheks
tail(p); unique(p$EDAD); summary(p)
table(p$PROV)
table(p$DEPRE, p$PROV)
table(p$DEPRE[p$PROV==30])
table(dbA$DEPRE[dbA$PROV==30])


# export
write.csv(p,"data/Expuestos.csv", row.names = F)

