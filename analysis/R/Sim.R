### relacion qx

pdf("plots/qx.Relation.pdf")
# q0 vs q1,4
plot(0.01,0.01,col='white', ylim=c(0,.2), xlim=c(0,.03),xaxs="i",yaxs="i", 
		 xlab="q0,1", ylab="")
cols=1
for (AMay in provs_codes){
	points(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==0 & EB_AM_nqxIC$PROVRE==AMay],
				 EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==1 & EB_AM_nqxIC$PROVRE==AMay], col=cols, pch=19, cex=.8)
	cols=cols+1
}
reg<-lm(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==1] ~ EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==0])
abline(reg, col="blue", lty=2)
abline(a = 0, 1, col="grey", lty=3)
legend("topleft", legend=provs_codes, col=1:5, pch=19, bty = "n", title = "Provinces")

#q0 vs q15-49
q15.49 = data.frame(q=(EB_AM_lxIC$lx.mean[EB_AM_lxIC$EDAD_q==15]-EB_AM_lxIC$lx.mean[EB_AM_lxIC$EDAD_q==50])/EB_AM_lxIC$lx.mean[EB_AM_lxIC$EDAD_q==15])
q15.49$PROVRE = EB_AM_lxIC$PROVRE[EB_AM_lxIC$EDAD_q==15]
# plot(0.01,0.01,col='white', ylim=c(0,.08), xlim=c(0,.08), xlab="q0,1", ylab="q15,49")
cols=1
for (AMay in provs_codes){
	points(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==0 & EB_AM_nqxIC$PROVRE==AMay],
				 q15.49[q15.49$PROVRE==AMay,1], col=cols, pch=19, cex=.8)
	cols=cols+1
}
reg<-lm(q15.49$q ~ EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==0])
abline(reg, col="pink", lty=2)
# abline(a = 0, 1, col="grey", lty=3)

#q15-49 vs 50-64
q50.64 = data.frame(q=(EB_AM_lxIC$lx.mean[EB_AM_lxIC$EDAD_q==50]-EB_AM_lxIC$lx.mean[EB_AM_lxIC$EDAD_q==65])/EB_AM_lxIC$lx.mean[EB_AM_lxIC$EDAD_q==50])
q50.64$PROVRE = EB_AM_lxIC$PROVRE[EB_AM_lxIC$EDAD_q==50]
# plot(0.01,0.01,col='white', ylim=c(0,.2), xlim=c(0,.2), xlab="q15,49", ylab="q50,64")
cols=1
for (AMay in provs_codes){
	points(EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==0 & EB_AM_nqxIC$PROVRE==AMay],
				 q50.64[q50.64$PROVRE==AMay,1], col=cols, pch=19, cex=.8)
	cols=cols+1
}
reg<-lm(q50.64$q ~ EB_AM_nqxIC$qx.mean[EB_AM_nqxIC$EDAD_q==0])
abline(reg, col="violet", lty=2)
abline(a = 0, 1, col="grey", lty=3)
text(0.02,0.15,"q(50,15)", col="violet")
text(0.02,0.08,"q(15,40)", col="pink")
text(0.02,0.01,"q(1,4)", col="blue")
dev.off()
#what model table is better?
# https://cran.r-project.org/web/packages/LifeTables/LifeTables.pdf


# 4 families
library(sqldf)
library(demogR)
library(DemoTools)

# model life tables with e10 belongs ()
Wf = cdmltw(sex = "F")
Wm = cdmltw(sex = "M")
ages = Wf$age
Sf = cdmlts(sex = "F")
Sm = cdmlts(sex = "M")
ages = Sf$age
Ef = cdmlte(sex = "F")
Em = cdmlte(sex = "M")
ages = Ef$age
Nf = cdmltn(sex = "F")
Em = cdmltn(sex = "M")
ages = Nf$age

# select e10
Wf$e10
Sf$e10
Ef$e10
Nf$e10

# shapes
WmModel = (Wf$nmx[22,]*1.04+Wf$nmx[22:25,]*1.05)/2.05

# graph
plot(ages, log(WmModel[4,]),t='s', col=1)
lines(ages, log(WmModel[3,]),t='s', col=2)
lines(ages, log(WmModel[2,]),t='s', col=3)
lines(ages, log(WmModel[1,]),t='s', col=4)

#exposures level
N = read.csv("U:/Data/NCensales.csv")
N = subset(N, Censo==2010 & Type=='C')
N$Edadq = trunc(N$Edad/5)*5
N$Edadq[N$Edad<5] = 1
N$Edadq[N$Edad==0] = 0
N$Edadq[N$Edadq>95] = 95
N = sqldf('select Edadq, sum(Totales) as N from N group by Edadq')
N$Estr = N$N/sum(N$N)
plot(N$Edadq, N$Estr, t='s')

# omision estocastica
omi_fun = function(x, d){
	# x = 0:100; d = d_sim[1,]
	omi = rep(.01, length(x)) 
	omi[x<5] = .1 - x[x<5] * .02
	omi[x>=70] = .01 + (x[x>69]-69) * .005
	# plot(0:100, omi)
	d_omi = as.integer(runif(length(x))<omi) * omi * d
	d_omi
}

######### Simula poblaciones
colNames = c('flia', 'level', 'con_omi', ages)
Model_sim = data.frame(t(rep(0,length(colNames))))
colnames(Model_sim) = colNames

for (flia in 1:nrow(mModel)){
	for (level in c(1000, 10000, 100000, 1000000)){
		# level = 1000000
		# flia = 1
		lambdas = c(level * N$Estr * mModel[flia,])
		d_sim = sapply(lambdas, FUN = function(x, y) rpois(y, x), y = 100)
		m_sim = data.frame(flia, level, con_omi=0, d_sim/N$N)
		colnames(m_sim) = colNames
		# points(ages, log(m_sim[1,]),t='s')
		
		# omision estocastica
		d_sim_omi = t(sapply(1:nrow(d_sim), function(i) omi_fun(ages, d_sim[i,])))
		d_sim_reg = d_sim - d_sim_omi
		m_sim_reg = d_sim_reg/N$N
		m_sim_reg = data.frame(flia, level, con_omi=0, d_sim_reg/N$N)
		colnames(m_sim_reg) = colNames
		# both things
		# d_sim-d_sim_reg
		# i = 43
		# plot(ages, log(m_sim[i,]), t = 's')
		# points(ages, log(m_sim_reg[i,]), col = 2, t = 's')
		
		Model_sim = rbind(Model_sim, m_sim, m_sim_reg)
	}
}
Model_sim = Model_sim[-1,]
# realizacion estocastica

# test methods

