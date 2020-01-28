###################################################################################
# Tomado de la web http://schmert.net/, del profesor Carl Schmertmann
# estimate the exposure over calendar years 2009-2011, per person who was integer
# age A on the 2010 census date (1 Aug 2010)
#
# results are 100x100 matrices Em and Ef for males and females, respectively
#
# For example,
#
# Em %*% [2010 male census pop ages 0..99]  =  [approx. 2009-2010 male exposure at 0..99]
###################################################################################

## some explanation

library(dplyr)

# if (.Platform$OS.type == 'windows') windows(record = TRUE)

## construct survival functions l(x) for males and females, based on HMD standard
## the calculated multipliers won't be very sensitive to the standard chosen
## the key point is to account for non-survival at high ages: a 97.58-year old
## observed in the Census (t=2010.58) represents *more* than one expected person-year 
## of exposure at age 96 over [2009.0, 2010.0), for example, 
## because we only see the survivors
# Utiliza supuesto exponencial dentro de cada edad (Keifitz pag 31)

mort <- read.csv("data/pob_expuestos/exp_Schmertmann/HMDstd.csv") %>%
				mutate( sf = exp(-exp(f)), sm = exp(-exp(m)),
        				lf = c(1, head(cumprod(sf),-1)),
        				lm = c(1, head(cumprod(sm),-1)))

this.lx = approxfun( x=mort$age, y=mort$lf*1/2.04 + mort$lm*1.04/2.04, yleft=1, yright=0)

# #Miro otras st?ndar
# library("LifeTables")
# age=mod.lt(child.value=0.01236, child.mort=1, sex="male",e0.target=75.3)$lt[,1]
# lx=mod.lt(child.value=0.01236, child.mort=1, sex="male",e0.target=75.3)$lt[,7]
# library(splines)
# model.lx=interpSpline(age,lx)
# age2=data.frame(age=0:99)
# lx.spline=predict(model.lx,newdata=(age2$age))
# #comparo standar
#     plot(mort$age,mort$lm,col=2)
#     points(seq(0,100,2),lx.spline$y/100000,col=3)


## calculate the person-years of exposure at ages [A,A+h) x times [0,bigT]
##   per exactly x-year-old at census date bigC 
## if 2009.0 is t=0, 27 Oct 2010 Census was at t=1.82

PY = function(x, A , h=1, bigC=1.5, bigT=3, this.sex='m', dt=.05) {
  tA = bigC - x + A   # time of A-th birthday
  tgrid = seq(dt/2, bigT-dt/2, dt) # grid of times at which exposure is possible
  
  # per x year old at time bigC, how many p-y of exposure in ages [A,A+n) x times [0,T)
  sum( (tgrid >= tA) * (tgrid < (tA+h)) * this.lx(x-bigC+tgrid) * dt) / this.lx(x)
}

#EJEMPLOS a mano
#2.9 a la edad 4
# tA=1.82-2.9+4 #va a transcurrir 0.08 a?os
# (this.lx(2.9-1.82+2.925)*0.05+this.lx(2.9-1.82+2.975)*0.05)/this.lx(2.9)
# #20.3 a la edad 19
# x=20.3;dt=0.05
# tA=1.82-20.3+19
# sum( (tgrid >= tA) * (tgrid < (tA+h)) * this.lx(x-bigC+tgrid) * dt) / this.lx(x)
#11 AL 30 DE GRID

## build a (long) data frame with all combinations of exact census ages (in 1/5ths of a year)
##  and one-year age groups. The expos variables will contain the expected person-years
##  lived at ages [A,A+1) over years [0,3], per x-year-old at the Census date of 1.82 
census.age  = seq(-1.9, 99.9,.20)   # possible ages at time C 
age.group   = 0:99

df = expand.grid( x=census.age, A=age.group)

for (i in 1:nrow(df)) {
  df[i,'m.expos'] = PY(x=df$x[i], A=df$A[i], this.sex='m')
  df[i,'f.expos'] = PY(x=df$x[i], A=df$A[i], this.sex='f')
} 

## construct a multiplier matrix to convert Census populations by single years of age
## to period exposure.  Aggregate exact census ages into integer age groups and then\
## calculate the average exposure. For example
##   Em [100x100] %*% male.census.pop [100x1] = male period exposure [100x1]


# esto no andaba!!!!
# tmp = df %>% 
#   mutate(intx = floor(x)) %>% 
#   group_by(intx,A) %>% 
#   summarize(f.expos=mean(f.expos), m.expos=mean(m.expos))

df$intx = floor(df$x)
tmp = sqldf('select avg("m.expos") as "m.expos" from df group by intx, A')

Em  = matrix(round(tmp$m.expos,2), nrow=100, 
             dimnames=list(paste0('A',0:99),paste0('x',-2:99)))


## collapse the first 3 columns (floor(x) = -2, -1, 0) by summing. 
##This is equivalent to assuming that the cohorts born over the two years after the Census 
##will be identical in size to current 0-yr-olds

W = rbind( cbind(1, matrix(0, 2, 99)) , 
           diag(100))


Em = Em %*% W


###########Ejemplos propios
# setwd("C:/Users/Iv?n/Google Drive/Proyecciones/SubNacionales/Sub_Mort_ARG/Poblaci?n-Expuestos")
# Ndptos <- read.csv("DptosOK.csv",sep=";")
# head(Ndptos);tail(Ndptos);summary(Ndptos);str(Ndptos)
# table(Ndptos$DEPRE,Ndptos$PROV)
# Ndptos$EDAD[Ndptos$EDAD>99]<-99
# library(sqldf);library(dplyr)
# Ndptos=sqldf("select PROV, DEPRE, EDAD, SUM(TOTAL) TOTAL FROM Ndptos group by PROV, DEPRE, EDAD")
# #DPTO
# prov=6;dpto=518
#     tmp = Ndptos %>% filter(PROV==prov,DEPRE==dpto) 
#     N=tmp$TOTAL
#     length(N);dim(Em)
#     X = Em %*% N
# #graf
#     plot( 0:99, N*3, type='h', lwd=2)
#     lines(0:99, X, lwd=3, col='seagreen')
#     title(paste("Prov", prov, ". Dpto ",dpto))
#     abline(v= seq(0,100,10), lty=2)
# #suavizado. MUY PARECIDO
# NLocal=loess(TOTAL*3~EDAD,span=.1,data=tmp)
#     lines(0:99,predict(NLocal,data.frame(EDAD=0:99)),
#     col="red",lwd=2)


########### examples