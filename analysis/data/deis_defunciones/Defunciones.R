#DEFUNCIONES

# Se agrupan por año según fecha de ocurrencia
# Se hace diagnóstico de desconocidos
# Se redistribuyen desconocidos: provincia, departamento, edad
# Levanto años 2012 y 2013 para recuperar tardíos
# Anio de ocurrecnia deconocido se asigna de registro 
# No ajuste por omisión

cont = 1
for (i in 2009:2013) {
  #i=2010
  arch <- paste('data/deis_defunciones/DEFT/DEFT', i,".dbf", sep = "")
  db_i <- read.dbf(arch)[c("DEPRE","PROVRE", 'JURI', 'PROVOC', 'DEPOC', "EDAD", "UNIEDA", "SEXO", "FECDEF")]
  # Separa meses y años de la fecha de defunción
  db_i$FECDEF <- gsub(".", "/", db_i$FECDEF, fixed = TRUE)
  db_i$fd_mm <- colsplit(db_i$FECDEF, '/', names =  c('dd','mm', 'aa'))[,2]
  db_i$fd_aa <- colsplit(db_i$FECDEF, '/', names =  c('dd','mm', 'aa'))[,3]
  # gr_edad # Pasa a 0 edades que no son años
  db_i$EDAD_r <- db_i$EDAD
  db_i$EDAD_r[db_i$UNIEDA == 2 | db_i$UNIEDA == 3 | db_i$UNIEDA == 4 | db_i$UNIEDA == 8] <- 0
  # set anio
  db_i$anio <- i
  db_i$SEXO[db_i$SEXO == 9] <- 3 # Sexo desconocido se le pone cod. 3.
  # agrupo
  db_l <- sqldf("select DEPRE, PROVRE, JURI, PROVOC, DEPOC, SEXO, EDAD_r, anio, fd_aa, fd_mm,
                count(SEXO) as ndef 
                from db_i 
                group by DEPRE, PROVRE, JURI, PROVOC, DEPOC, SEXO, EDAD_r, anio, fd_aa, fd_mm")
  
  if (cont == 1) {db <- db_l}
  if (cont > 1) {db <- sqldf("select * from db_l union all select * from db")}
  cont = cont + 1
  print(i)
}

# Resumen 
# db = db_0
summary(db)
str(db)
head(db)

### matriz de lugar ocurrencia/registro --> saco CABA mas adelante!!!
round(prop.table(xtabs(ndef~PROVOC+PROVRE, db),2),2)
# el 8% de UNK dep de residentes en CABA es porque ocurrio en BsAS
round(prop.table(xtabs(ndef~PROVOC, db, DEPRE==999 & PROVRE==2)),2)
# el 92% de ese 8% viene de BsAs
UnkDepCABA = sort(round(prop.table(xtabs(ndef~DEPOC, db, DEPRE==999 & PROVRE==2 & PROVOC==6)),2), decreasing = T)[1:8]*100
pUnkDepCABA = sum(UnkDepCABA) #Tres de febrero, Vicente Lopez y La Matanza
# saco CABA 2009-2010 por cambio de circ electoral - comunas
db = db[!(db$PROVRE==2 & db$anio<2011),]

# patrón de registros tardíos: al tomar año de ocurrencia las tardías no nos afectan (fecha de inscripción vs. fecha de defunción)

def_tardias = round(prop.table(table(db$anio,db$fd_aa),margin = 1)*100, 2)
diag(def_tardias)
Prom_tardias = round(mean(diag(def_tardias)),2)

# Muertes de menores de un año. Control con los datos publicados oficiales (anuario de  la DEIS)
# http://www.deis.msal.gov.ar/index.php/estadisticas-vitales/
sum(db$ndef[db$EDAD_r == 0 & db$anio == 2009]) #ok para distintos anios
sum(db$ndef[db$EDAD_r == 0 & db$PROVRE == 30 & db$anio == 2009]) #ok para distintas provs

############## Reasigna año y mes desconocidos o erróneos de la fecha de defunción
								# Si no tiene anio de ocurrencia se le asigna fecha de registro

db$fd_aa[is.na(db$fd_aa)] <- db$anio[is.na(db$fd_aa)]

############# Reacomodo base
# Pone años en columnas

def09 <- sqldf("select PROVRE, DEPRE, EDAD_r, SEXO, sum(ndef) as d from db where fd_aa=2009 group by PROVRE, DEPRE, EDAD_r")
def10 <- sqldf("select PROVRE, DEPRE, EDAD_r, SEXO, sum(ndef) as d from db where fd_aa=2010 group by PROVRE, DEPRE, EDAD_r")
def11 <- sqldf("select PROVRE, DEPRE, EDAD_r, SEXO, sum(ndef) as d from db where fd_aa=2011 group by PROVRE, DEPRE, EDAD_r")

# Merge todas las defunciones en un mismo df
def0910 <- merge(x = def09, y = def10, by = c("PROVRE", "DEPRE","SEXO","EDAD_r"), all = TRUE)
def091011 <- merge(x = def0910, y = def11, by = c("PROVRE", "DEPRE","SEXO","EDAD_r"), all = TRUE)
# Limpia variables auxiliares del merge y crea suma total
def091011$d11 <- def091011$d.x; def091011$d.x <- NULL
def091011$d10 <- def091011$d.y; def091011$d.y <- NULL
def091011$d09 <- def091011$d; def091011$d <- NULL
def091011[is.na(def091011)] <- 0
def091011$dT <- def091011$d09 + def091011$d10 + def091011$d11

############### Diagnóstico de datos desconocidos. Pequeño análisis de calidad de los datos.
							# No ajustamos omisión de manera de directa (como Carl & Gonzaga en 2018) que tenían una estimación de omisión en cada área menor
							# un pequeño análisis de la calidad de los datos viene.
							# HAY TABLAS Y DATOS QUE VAN EN APARTADO DE CALIDAD

####Total País: edad y sexo (para comentar en texto)

# Agrego nombres
#####Desconocido en edad Total País
DescEdad = round(sqldf("select sum(dT) from def091011 where EDAD_r=999")/sum(def091011$dT)*100,2)

#####Desconocido en sexo Total País
DescSexo = round(sqldf("select sum(dT) from def091011 where SEXO=3")/sum(def091011$dT)*100,2)

####Desconocido en dpto: ¿como estan las provincias elegidas? 
SinDEP = cbind(sqldf("select PROVRE, sum(dT) DsinDEP from def091011 where DEPRE=999 or DEPRE=0 group by PROVRE"),
               sqldf("select sum(dT) D from def091011 group by PROVRE"))
SinDEP$Porc = round(SinDEP$DsinDEP/SinDEP$D*100,1)
SinDEP = SinDEP[order(SinDEP$Porc,decreasing = T),][-c(1:2),] #df de SALIDA, estan muy bien posicionados los nuestros (podemos mencionarlo)
nombres_prov = xlsx::read.xlsx("data/map/codigos_prov_depto_censo2010.xls",sheetIndex = 3, encoding = "UTF-8")[,1:2]
SinDEP = merge(SinDEP, unique(nombres_prov), by.x = 'PROVRE', by.y = 'Prov_cod', all.x = T)
SinDEP = data.frame(Provincia = SinDEP[,5], `Unknown(%)` = SinDEP[,4])

##### Desconocidos en sexo y edad por dpto
# cálculo de desconocidos
defNAEdad <- sqldf("select PROVRE, DEPRE, sum(dT) as nsnc from def091011 where EDAD_r=999 group by PROVRE, DEPRE")
defNASexo <- sqldf("select PROVRE, DEPRE, sum(dT) as nsnc from def091011 where SEXO=3 group by PROVRE, DEPRE")
defT <- sqldf("select PROVRE, DEPRE, sum(dT) as tot from def091011 group by PROVRE, DEPRE")

# merge para porcentajes
PorcDefDesc <- merge(x = defT, y = defNAEdad, by = c("PROVRE","DEPRE"), all.x = TRUE)
PorcDefDesc <- merge(x = PorcDefDesc, y = defNASexo, by = c("PROVRE","DEPRE"), all.x = TRUE)
PorcDefDesc$DescEdad <- PorcDefDesc$nsnc.x; PorcDefDesc$nsnc.x <- NULL
PorcDefDesc$DescSexo <- PorcDefDesc$nsnc.y; PorcDefDesc$nsnc.y <- NULL
PorcDefDesc[is.na(PorcDefDesc)] <- 0
PorcDefDesc$PorcEdad = round(PorcDefDesc$DescEdad/PorcDefDesc$tot*100,1)
PorcDefDesc$PorcSexo = round(PorcDefDesc$DescSexo/PorcDefDesc$tot*100,1)

# Departamentos más complicados con desconocidos en sexo y edad, del TOTAL PAÍS (mencionar tops)
PorcDefDesc = PorcDefDesc[PorcDefDesc$PROVRE!=99,]
PorcDefDesc = PorcDefDesc[PorcDefDesc$DEPRE!=999,]
DescEdad_dpto = head(PorcDefDesc[order(PorcDefDesc$PorcEdad,decreasing = T),],10)
DescSexo_dpto = head(PorcDefDesc[order(PorcDefDesc$PorcSexo,decreasing = T),],10) #BsAs!!!

# De las provincias que seleccionamos, cual es el panorama de desconocidos
PorcDefDescPamp <- PorcDefDesc[PorcDefDesc$PROVRE %in% provs_codes,]
PorcDefDescPamp$PROVf<-as.factor(PorcDefDescPamp$PROVRE)
# desconocido en Edad: el máx es el dpto 287 de BsAs con un 2%, luego todos menores al 2%.
PorcDefDescPamp_age = head(PorcDefDescPamp[order(PorcDefDescPamp$PorcEdad, decreasing = T),], 10)
PorcDefDescPamp_sex = head(PorcDefDescPamp[order(PorcDefDescPamp$PorcSexo, decreasing = T),], 10)

# tabla resumen
nombres = xlsx::read.xlsx("data/map/codigos_prov_depto_censo2010.xls",sheetIndex = 3, encoding = "UTF-8")
nombres$link = as.integer(paste0(str_pad(nombres$Prov_cod, 2, pad = "0"),str_pad(nombres$Dpto_cod, 3, pad = "0")))
PorcDefDescPamp_age$link = as.integer(paste0(str_pad(PorcDefDescPamp_age$PROVRE, 2, pad = "0"),
																						 str_pad(PorcDefDescPamp_age$DEPRE, 3, pad = "0")))
PorcDefDescPamp_age = merge(PorcDefDescPamp_age, nombres, by = 'link', all.x = T)
PorcDefDescPamp_sex$link = as.integer(paste0(str_pad(PorcDefDescPamp_sex$PROVRE, 2, pad = "0"),
																						 str_pad(PorcDefDescPamp_sex$DEPRE, 3, pad = "0")))
PorcDefDescPamp_sex = merge(PorcDefDescPamp_sex, nombres, by = 'link', all.x = T)
UnkSexAge = cbind(PorcDefDescPamp_age[order(PorcDefDescPamp_age$PorcEdad, decreasing = T), c('Prov_Nombre', 'Dpto_Nombre', 'PorcEdad')],
									PorcDefDescPamp_sex[order(PorcDefDescPamp_sex$PorcSexo, decreasing = T), c('Prov_Nombre', 'Dpto_Nombre', 'PorcSexo')])
# Razón para en este ejercicio no desagregar por sexo!!!

# Otro chek con los datos publicados oficiales (anuario de  la DEIS)
# http://www.deis.msal.gov.ar/index.php/estadisticas-vitales/
sum(def091011$dT[def091011$PROVRE == 6 & def091011$EDAD_r == 0]) #ok 3439+3457+3488
sum(def091011$dT[def091011$PROVRE == 14 & def091011$EDAD_r >74]) #ok 13828+15348+15244

# Asign todos 999
dbA = def091011[,c(1:4,8)]
dbA$PROVRE[dbA$PROVRE==99] = 999

#saco sexo y residencia extranjera
dbA = sqldf('select PROVRE, DEPRE, EDAD_r, sum(dT) ndef from dbA group by PROVRE, DEPRE, EDAD_r')
dbA = dbA[dbA$PROVRE!=98,]
sum(dbA$ndef);summary(dbA)

############ redistribuir dentro de cada estrato

# red edad
dbA1 = dbA[dbA$EDAD_r!=999,]; sum(dbA1$ndef)
tmp = sqldf('select PROVRE, DEPRE, sum(ndef) Total from dbA1 group by PROVRE, DEPRE')
dbA2 = merge(dbA1,tmp,by=c('PROVRE', 'DEPRE'),all.x = T); sum(dbA2$ndef)
tmp = dbA[dbA$EDAD_r==999, c('PROVRE', 'DEPRE', 'ndef')]; sum(tmp$ndef)
dbA3 = merge(dbA2, tmp, by=c('PROVRE', 'DEPRE'), all = T)
dbA3$ndef = ifelse(is.na(dbA3$ndef.y), dbA3$ndef.x, dbA3$ndef.x + dbA3$ndef.x/dbA3$Total*dbA3$ndef.y)
sum(dbA3$ndef)
dbA4 = dbA3[,c(1,2,3,7)]
summary(dbA4)
dbA = dbA4

# red DEPRE
dbA1 = dbA[dbA$DEPRE!=999,]; sum(dbA1$ndef)
tmp = sqldf('select PROVRE, EDAD_r, sum(ndef) Total from dbA1 group by PROVRE, EDAD_r')
dbA2 = merge(dbA1,tmp,by=c('PROVRE', 'EDAD_r')); sum(dbA2$ndef)
tmp = dbA[dbA$DEPRE==999, c('PROVRE', 'EDAD_r', 'ndef')]; sum(tmp$ndef)
dbA3 = merge(dbA2, tmp, by=c('PROVRE', 'EDAD_r'), all = T)
dbA3$DEPRE[is.na(dbA3$DEPRE)] = 999
summary(dbA3)
dbA3$ndef = ifelse(is.na(dbA3$ndef.y), dbA3$ndef.x, dbA3$ndef.x + dbA3$ndef.x/dbA3$Total*dbA3$ndef.y)
dbA3$ndef = ifelse(is.na(dbA3$ndef), dbA3$ndef.y, dbA3$ndef)
sum(dbA3$ndef)
dbA3[is.na(dbA3$ndef),]
dbA4 = dbA3[,c(1,2,3,7)]
sum(dbA4$ndef)
dbA = dbA4

# red PROVRE
dbA1 = dbA[dbA$PROVRE!=999,]; sum(dbA1$ndef)
tmp = sqldf('select DEPRE, EDAD_r, sum(ndef) Total from dbA1 group by DEPRE, EDAD_r')
dbA2 = merge(dbA1,tmp,by=c('DEPRE', 'EDAD_r')); sum(dbA2$ndef)
tmp = dbA[dbA$PROVRE==999, c('DEPRE', 'EDAD_r', 'ndef')]; sum(tmp$ndef)
dbA3 = merge(dbA2, tmp, by=c('DEPRE', 'EDAD_r'), all = T)
dbA3$PROVRE[is.na(dbA3$PROVRE)] = 999
summary(dbA3)
dbA3$ndef = ifelse(is.na(dbA3$ndef.y), dbA3$ndef.x, dbA3$ndef.x + dbA3$ndef.x/dbA3$Total*dbA3$ndef.y)
dbA3$ndef = ifelse(is.na(dbA3$ndef), dbA3$ndef.y, dbA3$ndef)
sum(dbA3$ndef)
dbA3[is.na(dbA3$ndef),]
dbA4 = dbA3[,c(1,2,3,7)]
summary(dbA4)

# red RESABIO
Nred = sum(dbA$ndef[dbA$PROVRE==999 | dbA$DEPRE==999 | dbA$EDAD_r==999])
dbA = dbA[dbA$PROVRE<999 & dbA$DEPRE<999 & dbA$EDAD_r<999,]
NTotal = sum(dbA$ndef)
dbA$ndef = dbA$ndef+dbA$ndef/NTotal*Nred
sum(dbA$ndef)
summary(dbA)

# me quedo con provs
dbA = dbA[dbA$PROVRE %in% provs_codes,]
table(dbA$DEPRE, dbA$PROVRE)

### cheks finales

# Control con los datos publicados oficiales (anuario de  la DEIS)
# http://www.deis.msal.gov.ar/index.php/estadisticas-vitales/
sum(dbA$ndef[dbA$PROVRE == 6]) # 124280+129403+131626
# Entre Rios x=0
sum(dbA$ndef[dbA$PROVRE == 30 & dbA$EDAD_r == 0])  # 245+260+263
# La Pampa 2011 x = 15-24
sum(dbA$ndef[dbA$PROVRE == 42 & dbA$EDAD_r>14 & dbA$EDAD_r<25])  # 44+40+36

# Data final de defunciones. Escribe base final de defunciones
write.csv(dbA,"data/Defunciones.csv",row.names = F)
