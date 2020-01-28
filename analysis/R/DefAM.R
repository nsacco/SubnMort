#CLUSTERIZACION DE DPTOS

# Un conjunto de Am tiene menos variabilidad interna  cuando su AM (prom ponderado) tenga menos varianza
# cluster by NBI
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

################leo shape, create a SpatialPolygonDataFrame 

Arg <- rgdal::readOGR("C:/Proyectos/mortalidad_Argentina/analysis/data/map/pxdptodatosok.shp", 
											encoding = 'UTF-8')
Arg@data$link = as.integer(as.character(Arg@data$link))
plot(Arg[Arg@data$link!=94028 & Arg@data$link!=94021 & Arg@data$link!=94007 & Arg@data$link!=94014,],
		 border=gray(0.5))
# summary(Arg)
# class(Arg)
# head(Arg@data)

# utilizo data de pNBI standarizado
Arg = Arg[Arg@data$link %in% unique(dbtQ$link),]
unique(Arg@data$codpcia)

#merge
# str(Arg@data)
# str(NBIest)

Arg@data$link = as.integer(Arg@data$link)
pNBI$link = as.integer(pNBI$link)
length(table(Arg@data$link))
length(table(pNBI$link))
Arg = merge(Arg, pNBI, by=c('link'), all.x=T)
# head(Arg@data)
# str(Arg@data)

#centroides
plot(Arg, border=gray(0.5))
centr = coordinates(Arg)
points(coordinates(Arg))


############## veo distribucion primero

# equal-frequency class intervals
# par(mar=c(0,4,4,0))
plotvar <- Arg@data$NBI4
nclr <- 5
plotclr <- brewer.pal(nclr,"YlOrRd")
library(classInt)
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
plot(Arg, col=colcode)
legend('bottomright', legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.8, bty="n")

############### preparo

#https://geodacenter.github.io/tutorials/spatial_cluster/skater.html

#DF de variables explicativas
str(Arg@data)
dpadA <- data.frame(scale(Arg@data[,13:17]))

# neighbor list object
# https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
Arg.nb <- poly2nb(Arg,
                  snap=sqrt(.Machine$double.eps), # solo polyg pegados
                  queen=T # con un punto alcanza, cambia una conexion cerca de La Plata nada mas
                  )
#head(Arg.nb)
#summary(Arg.nb)
plot(Arg, border=gray(0.5))
plot(Arg.nb, centr, col="blue", cex.lab=0.7)

# costo de cada dpto respecto a sus vecinos
lcosts <- nbcosts(Arg.nb, dpadA , method = "euclidean")
                  # others "maximum", "manhattan", "canberra", "binary" or "minkowisk"
                  # https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/dist
# head(lcosts)

#makin list
nb.w <- nb2listw(Arg.nb, lcosts, style="B")
# head(nb.w)

# find a minimum spanning tree: cada nodo elije otro (el de min costo)
# Note that the dimension is n-1, since the minimum spanning tree consists of n-1 edges (links) needed to traverse all the nodes.
mst.Arg <- mstree(nb.w)
# class(mst.Arg)
# dim(mst.Arg)
# head(mst.Arg)

### the mstree plot
plot(mst.Arg, coordinates(Arg), col=2, cex.lab=.7, cex.circles=0.035, fg="blue")
plot(Arg, border=gray(.5), add=TRUE)

##################### Clusterizacion!!!

### the skater plot
# display.brewer.all()
paleta = "Pastel1"
# pdf("MapCluster.pdf")

### EXPERT OPTIONS
### Divisiones progresivas
### with the minimun criteria
clus2 <- skater(mst.Arg[,1:2], dpadA, 2, crit = 20, rep(1,nrow(Arg@data)))
plot(Arg, col=brewer.pal(n = max(clus2$groups), name = paleta)[clus2$groups], main = 'Cluster of departments')
table(clus2$groups)

### and do more one division with the full criteria
clus3 <- skater(clus2, dpadA, 2, crit = c(20,100), rep(1,nrow(Arg@data)))
plot(Arg, col=brewer.pal(n = max(clus3$groups), name = paleta)[clus3$groups], main = 'Cluster of departments')
table(clus3$groups)

### and do more one division with the full criteria
clus4 <- skater(clus3, dpadA, 1, crit = c(20,100), rep(1,nrow(Arg@data)))
plot(Arg, border='black', col=brewer.pal(n = max(clus4$groups), name = paleta)[clus4$groups], main = 'Cluster of departments')
table(clus4$groups)

### and do more one division with the full criteria
clus5 <- skater(clus4, dpadA, 1, crit = c(30,100), rep(1,nrow(Arg@data)))
plot(Arg, border='black', col=brewer.pal(n = max(clus5$groups), name = paleta)[clus5$groups], main = 'Cluster of departments')
table(clus5$groups)

####### graph cluster

ArgProv <- rgdal::readOGR("C:/Proyectos/mortalidad_Argentina/analysis/data/map/pxpciadatosok.shp",encoding = 'UTF-8')

pdf("plots/cluster.pdf")
plot(Arg, border='black', col=brewer.pal(n = max(clus5$groups), name = paleta)[clus5$groups])
plot(ArgProv, border="red", add=TRUE)
dev.off()

## Resultado Final

Arg$K = clus5$groups

################### Chek efecto homogeneizacion

Arg@data$codpcia = as.character(Arg@data$codpcia)

# varianza between y entre grupos
MeanVarProv = mean(sapply(unique(Arg@data$codpcia), 
                          function(x) var(Arg@data$TM[Arg@data$codpcia==x])))
MeanCVProv = mean(sapply(unique(Arg@data$codpcia), 
												 function(x) var(Arg@data$TM[Arg@data$codpcia==x]/mean(Arg@data$TM[Arg@data$codpcia==x]))))
VarMeanProv = var(sapply(unique(Arg@data$codpcia), 
												 function(x) mean(Arg@data$TM[Arg@data$codpcia==x])))

MeanVarClust = mean(sapply(unique(Arg@data$K), 
													 function(x) var(Arg@data$TM[Arg@data$K==x])))
MeanCVClust = mean(sapply(unique(Arg@data$K), 
													function(x) var(Arg@data$TM[Arg@data$K==x])/mean(Arg@data$TM[Arg@data$K==x])))
VarMeanClust = var(sapply(unique(Arg@data$K), 
													function(x) mean(Arg@data$TM[Arg@data$K==x])))

#ratios
AumHeterogEntreGr = round((1-MeanVarClust/MeanVarProv)*100,0) # son mas homogeneos hacia adentro los grupos? con Var
DismHeterogIntraGr = round((VarMeanClust/VarMeanProv-1)*100,0) # son mas heterogeneos entre si?

## guardo base

write.csv(Arg@data[c('link', 'K')],"data/Cluster.csv", row.names = F)


#############24 Partidos
# Aglo = c(28,35,91,260,270,274,371,408,410,412,427,434,490,515,539,560,568,
#          658,749,756,760,805,840,861)
# length(Aglo)
# dbtQ$PROVRE[dbtQ$PROVRE == 6 & (dbtQ$DEPRE %in% Aglo)] = 24
# dbtQ$Prov_Nombre[dbtQ$PROVRE == 24] = "BsAs - 24 Partidos"
# dbtQ$Prov_Nombre[dbtQ$PROVRE == 6] = "BsAs - Interior"
# unique(dbtQ$Prov_Nombre)
