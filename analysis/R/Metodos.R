
########################## Bayesiano Empirico

BE <- function(dbtQ_dep, AM){ 
  # dbtQ es un df con PROVRE, DEPRE, EDAD_q, d y N
  # el desarrrollo sigue a Assuncao y otros (2005)
  # AM es el Area mayor elegida
  # ejemplo 
    # AM=1

	# small area
  dbtq = subset(dbtQ_dep, K == AM & dbtQ_dep$EDAD_q < 999)
  
  # rare case: doy el doble de muertos cuando no hay expuestos, o al menos 1 persona
  dbtq$N[dbtq$N==0] = pmax(1, dbtq$d[dbtq$N==0]*2) 
  n_ages = length(ages)
  
  ##construyo matrices P, Omega, Q, Sigma
  # P matriz de poblacion
  P = data.frame(matrix(NA, nrow = 1, ncol = n_ages+1))
  colnames(P) = c("link", as.character(ages))
  for (i in unique(dbtq$link)) {
    P = rbind(P,c(i,subset(dbtq,link == i)$N))}
  P = P[-1,]
  PA = colSums(P[,-1])
  
  # MuA, rate de área mayor: EB assumption
  muA <- sqldf("select EDAD_q, sum(d)/sum(N) mu from dbtq group by EDAD_q")$mu
  
  # mua, rate de áreas menores
  dbtq$mua <- dbtq$d/dbtq$N 
  
  # Q : Covariance matrix of ML estimates
  Q = matrix(0,n_ages,n_ages)
  for (i in unique(dbtq$link)) {
    #i=42007
    Pa = diag(x = as.numeric((P[P$link == i,-1])/PA))
    mua = as.numeric(dbtq$mua[dbtq$link == i])
    #dim(Q);dim(Pa);length(mua);length(muA)
    Q = Q + (Pa^0.5) %*% (mua - muA) %*% t(mua - muA) %*% (Pa^0.5)
  }
  
  # Sigma
  Sigma0 = matrix(0,n_ages,n_ages)
  for (i in unique(dbtq$link)) {
    #i=42007
    Omega = diag(x = muA/as.numeric(P[P$link == i,-1])) # grupos con 0 poblacion
    Pa = diag(x = as.numeric((P[P$link == i,-1])/PA))
    Sigma0 = Sigma0 + Pa %*% Omega
  }
  
  Pjk = matrix(0,n_ages,n_ages)
  
  for (i in unique(dbtq$link)) {
    Pjk = Pjk + (as.numeric((P[P$link == i,-1])/PA) %*% t(as.numeric((P[P$link == i,-1])/PA)))^0.5
  }
  
  Sigma = (Q - Sigma0)/Pjk
  # any(is.na(Sigma))
  
  # Para lograr Sigma sin negativos, diagonalizo y reemplazo
  autovalores = eigen(Sigma)$values
  autovectores = eigen(Sigma)$vectors
  autovalores[autovalores < 0] = 0
  Sigma = autovectores %*% diag(autovalores) %*% t(autovectores)
  
  #df de salida
  EBout = c(K = 0,link = 0,EDAD_q = 0,N = 0,mua = 0,muaEB = 0)
  for (i in unique(dbtq$link)) {
    # i=6028
    db_i = subset(dbtq,link == i)
    Omega_a = diag(x = muA/as.numeric(P[P$link == i,-1]))
    #estimación tasa
    mua = db_i$mua
    muaEB = mua + Omega_a %*% solve(Omega_a + Sigma) %*% (muA - mua)
    muaEB[which(muaEB<0)] = muA[which(muaEB<0)] #fuerzo no negatividad dandole AM (poquisimos casos)
    EBout = rbind(EBout,data.frame(AM,link = i,EDAD_q = db_i$EDAD_q,N = db_i$N,mua,muaEB))
  }
  EBout = EBout[EBout$AM != 0,]
  
  return(EBout)
}


########################### Estimacion Indirecta (cambiar nivel para que sume defunciones)
EI <- function(dbtQ,AM){
  #AM=5
  dbtq=subset(dbtQ, K==AM)
  muA<-sqldf("select EDAD_q, sum(d)/sum(N) mu
             from dbtq
             group by EDAD_q")$mu
  #result
  EIout=c(K=0,link=0,EDAD_q=0,N=0,mua=0,muaEI=0)
  for (i in unique(dbtq$link)){
    #i=6518
    db_i=subset(dbtq,link==i)
    #estimación tasa
    raj=sum(db_i$d)/db_i$N%*%muA
    muaEI=rep(raj,length(ages))*muA
    EIout=rbind(EIout, data.frame(K=AM,link=i,EDAD_q=db_i$EDAD_q,N=db_i$N,muaEI))
  }
  EIout=EIout[EIout$K!=0,]
  return(EIout)
}

########################## TOPALS (tomado de http://schmert.net/topals-mortality/)


# B   = bs( 0:90, knots=c(0,5,10,20,40,70), degree=1)
B   = bs( 0:19, knots=c(0,2,6,10,13), degree=1) # nodos en 0, 5-9, 25-29, 45-49, 60-64
nalpha = ncol(B)
alpha0 = rep(0, ncol(B))  # initial offsets (all 0 means start at standard schedule)
# HMDstd = read.csv('data/pob_expuestos/exp_Schmertmann/HMDstd.csv') %>%
#          filter(age %in% 0:90)
# this.sex=c('m') #,'f')
# this.std  = HMDstd[ages+1,this.sex]

### log likelihood function L(alpha | D,N)
penalized.logL = function( alpha, D, N, std, Basis=B, k=1) {
                            logmx.hat = std + (Basis %*% alpha)
                            penalty = k * sum(diff(alpha)^2)
                            return( sum( -N * exp(logmx.hat) + D * logmx.hat) - penalty)
}

## standardized control parameters for all nonlinear fits
this.control = list(maxit=1000,fnscale= -1, parscale=rep(.01,ncol(B)))

#prueba


TOPALS <- function(dbtQ, AM, this.std){

  ETopals=c(K=0, link=0, EDAD_q=0, N=0, mua=0, muaTop=0)
      # AM = 3; this.std = this.std[this.std$K==AM,"logmx"]
  		dbtq = subset(dbtQ, K == AM)
    
      code.list=unique(dbtq$link)
      
      for (this.code in code.list) {
          
        #this.code=30008
          
          this.pop  = filter(dbtq, link==this.code)
          
          fit = optim( par=alpha0, fn=penalized.logL, 
                       #alpha=alpha0,
                       D=this.pop$d, N=this.pop$N, 
                       std= this.std, Basis=B,
                       method='BFGS', control=this.control)
          
          if (fit$convergence != 0) stop('Poisson regression did not converge')
          # poisson estimates
          logmx.hat = this.std + (B %*% fit$par)
          ETopals=rbind(ETopals,data.frame(K=AM,link=this.code,
                                           EDAD_q=this.pop$EDAD_q,
                                           N=this.pop$N,d=this.pop$d,
                                           mua=log(this.pop$d/this.pop$N),
                                           muaTop=logmx.hat,
                                           std=this.std,
                                           offset=B %*% fit$par))
      } 
      ETopals=ETopals[ETopals$K!=0,]
  return(ETopals)
}

##########################

