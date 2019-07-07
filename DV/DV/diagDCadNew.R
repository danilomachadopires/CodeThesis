rm(list=ls(all=T))

library(coda)
#--------------Davidson simples com W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVcomW")
banco <- read.table("granprix2010a2013.txt",h=T)
nomes <- levels(banco$W)
attach(banco)

estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

m  <- length(estimandos)  # número de jogadores analisados
n  <- length(W)           # número de jogos
################### Verificar o banco de dados para partidas onde os participantes não serão analisados
GW <- matrix(0,n,m) # matriz de 0s com linhas igual ao número de partidas e colunas igual a jogadores
GB <- matrix(0,n,m)

# aqui é preenchido as matrizes com indices referentes aas partidas jogados pelos jogadores analisados
# por exemplo na primeira partida o jogador Shakhriyar jogou de brancas, logo na matriz GW cuja coluna
# é referente a este jogador irá aparecer indíce (1) referente a ele.
for(i in 1:m){
  GW[,i] <- as.integer(W==estimandos[i])
  GB[,i] <- as.integer(B==estimandos[i])
}

M <- GW+GB
fora <- which(apply(M,1,sum)==0)      # Partidas (posições) onde os jogadores analisados não participam 

GW <- GW[-fora,]
GB <- GB[-fora,]
y <- y[-fora]

WR <- WR[-fora]
BR <- BR[-fora]

W <- W[-fora]
n <- length(y)
Y <- matrix(0,n,3)
Y[y==0,1]  <- 1 
Y[y==.5,2] <- 1
Y[y==1,3]  <- 1
Data<-Data[-fora]
tmin <- min(Data)
tmax <- max(Data)
omega <- 1/sqrt((1+Data-tmin)/(1+tmax-tmin))

propto    <- (table(y)/length(y))    # proporcional
propigual <- c((1-propto[2])/2,propto[2],(1-propto[2])/2) # descartando empate


DVeropropto <- sum(Y[,1]*log(propto[1])+Y[,2]*log(propto[2])+Y[,3]*log(propto[3]));
AIC<--2*DVeropropto+4

AICT<--2*sum(omega*(Y[,1]*log(propto[1])+ Y[,2]*log(propto[2])+Y[,3]*log(propto[3]))) +4



DVeropropigual <- sum(Y[,1]*log(propigual[1])+Y[,2]*log(propigual[2])+Y[,3]*log(propigual[3]));
AIC<--2*DVeropropigual+4

AICT<--2*sum(omega*(Y[,1]*log(propigual[1])+Y[,2]*log(propigual[2])+Y[,3]*log(propigual[3])))+4
#DDistpropigual <-sum(sqrt((Y[,1]-propigual[1])^2 +(Y[,2]-propigual[2])^2  +(Y[,3]-propigual[3])^2 )) ; DDistpropigual 

-2*sum(Y[,1]*log(1/3)+Y[,2]*log(1/3)+Y[,3]*log(1/3))+4
-2*sum(omega*(Y[,1]*log(1/3)+Y[,2]*log(1/3)+Y[,3]*log(1/3)))+4
#-------------------------------------------------------------------------



c1DVgW <- mcmc(read.table("cadeia.gamaA.txt"))
c1DVEW<- mcmc(read.table("cadeia.lambdaA.txt"))
l1DVW <- read.table("cadeia.lVmA.txt")

c2DVgW <- mcmc(read.table("cadeia.gamaB.txt"))
c2DVEW<- mcmc(read.table("cadeia.lambdaB.txt"))
l2DVW <- read.table("cadeia.lVmB.txt")
                            
c3DVgW <- mcmc(read.table("cadeia.gamaC.txt"))
c3DVEW<- mcmc(read.table("cadeia.lambdaC.txt"))
l3DVW <- read.table("cadeia.lVmC.txt")

# raftery.diag(c1DVgW)
# raftery.diag(c2DVgW)
# raftery.diag(c3DVgW)
# cadeiag <- mcmc.list(c1DVgW,c2DVgW,c3DVgW)
# gelman.diag(cadeiag)

# raftery.diag(c1DVEW)
# raftery.diag(c2DVEW)
# raftery.diag(c3DVEW)
# cadeiad <- mcmc.list(c1DVEW,c2DVEW,c3DVEW)
# gelman.diag(cadeiad)
## CONVERGIU

## log-Verossimilhança Marginal
#l1DVWm <- -2*(mean(l1DVW$V1)+var(l1DVW$V1))
#l2DVWm <- -2*(mean(l2DVW$V1)+var(l2DVW$V1))
#l3DVWm <- -2*(mean(l3DVW$V1)+var(l3DVW$V1))


l1DVWm <- (mean(l1DVW$V1))
l2DVWm <- (mean(l2DVW$V1))
l3DVWm <- (mean(l3DVW$V1))

lVDVWm.vec <- c(l1DVWm,l2DVWm,l3DVWm)
lVmDVW <- mean(lVDVWm.vec)
varlVmDVW <- var(lVDVWm.vec)
AICM<--2*lVmDVW+2*varlVmDVW

#########################################################################################
cDVgW   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                      read.table("cadeia.gamaB.txt"),
                      read.table("cadeia.gamaC.txt")))
cDVEW <- mcmc(rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt")))

# plot(cDVgW)
# plot(cDVEW)

res.gama   <- summary(cDVgW)
dados7<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                  ic.inf=HPDinterval(cDVgW)[,1],
                  ic.sup=HPDinterval(cDVgW)[,2])


dados7$RAN <- rank(dados7$gama,ties.method= "first")
dados7$nomes<-ordered(dados7$nomes, levels=dados7$nomes[order(dados7$gama)])
aux<-dados7
for(i in 1:length(levels(dados7$nomes))){
  aux[i,]<-dados7[which(dados7$RAN==i),]
}
dados7<-aux;rm(aux)

save(dados7,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/dados7.rda")
#########################################################################################

#--------------Davidson simples sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVsemW")
c1DVg <- mcmc(read.table("cadeia.gamaA.txt"))
c1DVE<- mcmc(read.table("cadeia.lambdaA.txt"))
l1DV <- read.table("cadeia.lVmA.txt")

c2DVg <- mcmc(read.table("cadeia.gamaB.txt"))
c2DVE<- mcmc(read.table("cadeia.lambdaB.txt"))
l2DV <- read.table("cadeia.lVmB.txt")

c3DVg <- mcmc(read.table("cadeia.gamaC.txt"))
c3DVE<- mcmc(read.table("cadeia.lambdaC.txt"))
l3DV <- read.table("cadeia.lVmC.txt")


# raftery.diag(c1DVg)
# raftery.diag(c2DVg)
# raftery.diag(c3DVg)
# cadeiag <- mcmc.list(c1DVg,c2DVg,c3DVg)
# gelman.diag(cadeiag)
# 
# raftery.diag(c1DVE)
# raftery.diag(c2DVE)
# raftery.diag(c3DVE)
# cadeiad <- mcmc.list(c1DVE,c2DVE,c3DVE)
# gelman.diag(cadeiad)
## CONVERGIU

## log-Verossimilhança Marginal
#l1DVm <- -2*(mean(l1DV$V1)+var(l1DV$V1))
#l2DVm <- -2*(mean(l2DV$V1)+var(l2DV$V1))
#l3DVm <- -2*(mean(l3DV$V1)+var(l3DV$V1))

l1DVm <- (mean(l1DV$V1))
l2DVm <- (mean(l2DV$V1))
l3DVm <- (mean(l3DV$V1))

lVDVm.vec <- c(l1DVm,l2DVm,l3DVm)
lVmDV <- mean(lVDVm.vec)
varlVmDV <- var(lVDVm.vec)
AICM<--2*lVmDV+2*varlVmDV

#########################################################################################
cDVg   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                      read.table("cadeia.gamaB.txt"),
                      read.table("cadeia.gamaC.txt")))
cDVE <- mcmc(rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt")))

# plot(cDVg)
# plot(cDVE)

res.gama   <- summary(cDVg)
dados8<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cDVg)[,1],
                   ic.sup=HPDinterval(cDVg)[,2])

dados8$RAN <- rank(dados8$gama,ties.method= "first")
dados8$nomes<-ordered(dados8$nomes, levels=dados8$nomes[order(dados8$gama)])
aux<-dados8
for(i in 1:length(levels(dados8$nomes))){
  aux[i,]<-dados8[which(dados8$RAN==i),]
}
dados8<-aux;rm(aux)

save(dados8,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/dados8.rda")
#########################################################################################


#--------------Davidson vantagem (1delta) com W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagem1deltaW")
c1DV1Wdg <- mcmc(read.table("cadeia.gamaA.txt"))
c1DV1Wdd <- mcmc(read.table("cadeia.deltaA.txt"))
c1DV1WE <- mcmc(read.table("cadeia.lambdaA.txt"))
l1DV1Wd <- read.table("cadeia.lVmA.txt")

c2DV1Wdg <- mcmc(read.table("cadeia.gamaB.txt"))
c2DV1Wdd <- mcmc(read.table("cadeia.deltaB.txt"))
c2DV1WE <- mcmc(read.table("cadeia.lambdaB.txt"))
l2DV1Wd <- read.table("cadeia.lVmB.txt")

c3DV1Wdg <- mcmc(read.table("cadeia.gamaC.txt"))
c3DV1Wdd <- mcmc(read.table("cadeia.deltaC.txt"))
c3DV1WE <- mcmc(read.table("cadeia.lambdaC.txt"))
l3DV1Wd <- read.table("cadeia.lVmC.txt")


# raftery.diag(c1DV1Wdg)
# raftery.diag(c1DV1Wdd)
# raftery.diag(c1DV1WE)
# 
# raftery.diag(c2DV1Wdg)
# raftery.diag(c2DV1Wdd)
# raftery.diag(c2DV1WE)
# 
# raftery.diag(c3DV1Wdg)
# raftery.diag(c3DV1Wdd)
# raftery.diag(c3DV1WE)
# 
# cadeiag <- mcmc.list(c1DV1Wdg,c2DV1Wdg,c3DV1Wdg)
# cadeiad <- mcmc.list(c1DV1Wdd,c2DV1Wdd,c3DV1Wdd)
# cadeiaE <- mcmc.list(c1DV1WE,c2DV1WE,c3DV1WE)
# gelman.diag(cadeiag)
# gelman.diag(cadeiad)
# gelman.diag(cadeiaE)
# 
## CONVERGIU

## log-Verossimilhança Marginal
#l1DV1Wdm <- -2*(mean(l1DV1Wd$V1)+var(l1DV1Wd$V1))
#l2DV1Wdm <- -2*(mean(l2DV1Wd$V1)+var(l2DV1Wd$V1))
#l3DV1Wdm <- -2*(mean(l3DV1Wd$V1)+var(l3DV1Wd$V1))

l1DV1Wdm <- (mean(l1DV1Wd$V1))
l2DV1Wdm <- (mean(l2DV1Wd$V1))
l3DV1Wdm <- (mean(l3DV1Wd$V1))

lVDV1Wdm.vec <- c(l1DV1Wdm,l2DV1Wdm,l3DV1Wdm)
lVm1WdDV <- mean(lVDV1Wdm.vec)
varlVWmDV <- var(lVDV1Wdm.vec)
AICM<--2*lVm1WdDV+2*varlVWmDV 

##########################################################################################
#########################################################################################################
cDV1Wdg <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                      read.table("cadeia.gamaB.txt"),
                      read.table("cadeia.gamaC.txt")))
cDV1Wdd <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                      read.table("cadeia.deltaB.txt"),
                      read.table("cadeia.deltaC.txt")))
cDV1WE <- mcmc(rbind(read.table("cadeia.lambdaA.txt"),
                     read.table("cadeia.lambdaB.txt"),
                     read.table("cadeia.lambdaC.txt")))
# plot(cDV1Wdg)
# plot(cDV1Wdd)
# plot(cDV1WE)
# 
# summary(cDV1Wdd);HPDinterval(cDV1Wdd)
# summary(cDV1WE);HPDinterval(cDV1WE)

res.gama   <- summary(cDV1Wdg)
dados9<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                    ic.inf=HPDinterval(cDV1Wdg)[,1],
                    ic.sup=HPDinterval(cDV1Wdg)[,2])

dados9$RAN <- rank(dados9$gama,ties.method= "first")
dados9$nomes<-ordered(dados9$nomes, levels=dados9$nomes[order(dados9$gama)])
aux<-dados9
for(i in 1:length(levels(dados9$nomes))){
  aux[i,]<-dados9[which(dados9$RAN==i),]
}
dados9<-aux;rm(aux)

save(dados9,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/dados9.rda")
#########################################################################################################
##########################################################################################


#--------------Davidson vantagem (1delta) sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagem1deltasemW")
#setwd("/home/danilo/Dropbox/3marco2014/DV/DVvantagem1deltasemW")

c1DV1dg <- mcmc(read.table("cadeia.gamaA.txt"))
c1DV1dd <- mcmc(read.table("cadeia.deltaA.txt"))
c1DV1E <- mcmc(read.table("cadeia.lambdaA.txt"))
l1DV1d <- read.table("cadeia.lVmA.txt")

c2DV1dg <- mcmc(read.table("cadeia.gamaB.txt"))
c2DV1dd <- mcmc(read.table("cadeia.deltaB.txt"))
c2DV1E <- mcmc(read.table("cadeia.lambdaB.txt"))
l2DV1d <- read.table("cadeia.lVmB.txt")

c3DV1dg <- mcmc(read.table("cadeia.gamaC.txt"))
c3DV1dd <- mcmc(read.table("cadeia.deltaC.txt"))
c3DV1E <- mcmc(read.table("cadeia.lambdaC.txt"))
l3DV1d <- read.table("cadeia.lVmC.txt")


# raftery.diag(c1DV1dg)
# raftery.diag(c1DV1dd)
# raftery.diag(c1DV1E)
# 
# raftery.diag(c2DV1dg)
# raftery.diag(c2DV1dd)
# raftery.diag(c2DV1E)
# 
# raftery.diag(c3DV1dg)
# raftery.diag(c3DV1dd)
# raftery.diag(c3DV1E)
# 
# cadeiag <- mcmc.list(c1DV1dg,c2DV1dg,c3DV1dg)
# cadeiad <- mcmc.list(c1DV1dd,c2DV1dd,c3DV1dd)
# cadeiaE <- mcmc.list(c1DV1E,c2DV1E,c3DV1E)
# gelman.diag(cadeiag)
# gelman.diag(cadeiad)
# gelman.diag(cadeiaE)
# 
## CONVERGIU

## Estimativa do AICM
## Diferencça entre média e variância da 
## log-Verossimilhança Marginal (dobro)
#l1DV1dm <- -2*(mean(l1DV1d$V1)+var(l1DV1d$V1))
#l2DV1dm <- -2*(mean(l2DV1d$V1)+var(l2DV1d$V1))
#l3DV1dm <- -2*(mean(l3DV1d$V1)+var(l3DV1d$V1))

l1DV1dm <- (mean(l1DV1d$V1))
l2DV1dm <- (mean(l2DV1d$V1))
l3DV1dm <- (mean(l3DV1d$V1))

lVDV1dm.vec <- c(l1DV1dm,l2DV1dm,l3DV1dm)
lVm1dDV <- mean(lVDV1dm.vec)  ### AICM
varlVmDV <- var(lVDV1dm.vec)    ### Erro do estimativa do AICM
AICM<--2*lVm1dDV+2*varlVmDV 

##########################################################################################
#########################################################################################################
cDV1dg <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                     read.table("cadeia.gamaB.txt"),
                     read.table("cadeia.gamaC.txt")))
cDV1dd <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                     read.table("cadeia.deltaB.txt"),
                     read.table("cadeia.deltaC.txt")))
cDV1E <- mcmc(rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt")))
# plot(cDV1dg)
# plot(cDV1dd)
# plot(cDV1E)
# 
# summary(cDV1dd);HPDinterval(cDV1dd)
# summary(cDV1E);HPDinterval(cDV1E)


res.gama   <- summary(cDV1dg)
dados10<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cDV1dg)[,1],
                   ic.sup=HPDinterval(cDV1dg)[,2])

dados10$RAN <- rank(dados10$gama,ties.method= "first")
dados10$nomes<-ordered(dados10$nomes, levels=dados10$nomes[order(dados10$gama)])
aux<-dados10
for(i in 1:length(levels(dados10$nomes))){
  aux[i,]<-dados10[which(dados10$RAN==i),]
}
dados10<-aux;rm(aux)

save(dados10,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/dados10.rda")


#########################################################################################################
##########################################################################################

#--------------Davidson vantagem  com W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagemComW")
c1DVWdg <- mcmc(read.table("cadeia.gamaA.txt"))
c1DVWdd <- mcmc(read.table("cadeia.deltaA.txt"))
c1DVWdE <- mcmc(read.table("cadeia.lambdaA.txt"))
l1DVWd <- read.table("cadeia.lVmA.txt")

c2DVWdg <- mcmc(read.table("cadeia.gamaB.txt"))
c2DVWdd <- mcmc(read.table("cadeia.deltaB.txt"))
c2DVWdE <- mcmc(read.table("cadeia.lambdaB.txt"))
l2DVWd <- read.table("cadeia.lVmB.txt")

c3DVWdg <- mcmc(read.table("cadeia.gamaC.txt"))
c3DVWdd <- mcmc(read.table("cadeia.deltaC.txt"))
c3DVWdE <- mcmc(read.table("cadeia.lambdaC.txt"))
l3DVWd <- read.table("cadeia.lVmC.txt")


# raftery.diag(c1DVWdg)
# raftery.diag(c1DVWdd)
# raftery.diag(c1DVWdE)
# 
# raftery.diag(c2DVWdg)
# raftery.diag(c2DVWdd)
# raftery.diag(c2DVWdE)
# 
# raftery.diag(c3DVWdg)
# raftery.diag(c3DVWdd)
# raftery.diag(c3DVWdE)
# 
# cadeiag <- mcmc.list(c1DVWdg,c2DVWdg,c3DVWdg)
# gelman.diag(cadeiag)
# cadeiad <- mcmc.list(c1DVWdd,c2DVWdd,c3DVWdd)
# gelman.diag(cadeiad)
# cadeiaE <- mcmc.list(c1DVWdE,c2DVWdE,c3DVWdE)
# gelman.diag(cadeiaE)
# 
## CONVERGIU

## log-Verossimilhança Marginal
#l1DVWdm <- -2*(mean(l1DVWd$V1)+var(l1DVWd$V1))
#l2DVWdm <- -2*(mean(l2DVWd$V1)+var(l2DVWd$V1))
#l3DVWdm <- -2*(mean(l3DVWd$V1)+var(l3DVWd$V1))

l1DVWdm <- (mean(l1DVWd$V1))
l2DVWdm <- (mean(l2DVWd$V1))
l3DVWdm <- (mean(l3DVWd$V1))

lVDVWdm.vec <- c(l1DVWdm,l2DVWdm,l3DVWdm)
lVmWdDV <- mean(lVDVWdm.vec )
varlVWmDV <- var(lVDVWdm.vec)
AICM<--2*lVmWdDV+2*varlVWmDV 


##########################################################################################
#########################################################################################################
cDVWdg <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                      read.table("cadeia.gamaB.txt"),
                      read.table("cadeia.gamaC.txt")))
cDVWdd <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                      read.table("cadeia.deltaB.txt"),
                      read.table("cadeia.deltaC.txt")))
cDVWdE <- mcmc(rbind(read.table("cadeia.lambdaA.txt"),
                     read.table("cadeia.lambdaB.txt"),
                     read.table("cadeia.lambdaC.txt")))
# plot(cDVWdg)
# plot(cDVWdd)
# plot(cDVWdE)

res.gama   <- summary(cDVWdg)
dados11<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cDVWdg)[,1],
                   ic.sup=HPDinterval(cDVWdg)[,2])

dados11$RAN <- rank(dados11$gama,ties.method= "first")
dados11$nomes<-ordered(dados11$nomes, levels=dados11$nomes[order(dados11$gama)])
aux<-dados11
for(i in 1:length(levels(dados11$nomes))){
  aux[i,]<-dados11[which(dados11$RAN==i),]
}
dados11<-aux;rm(aux)

res.delta   <- summary(cDVWdd)
dados11a<-data.frame(nomes=estimandos,delta=res.delta[1]$statistics[,1],
                    ic.inf=HPDinterval(cDVWdd)[,1],
                    ic.sup=HPDinterval(cDVWdd)[,2])

dados11a$RAN <- rank(dados11a$delta,ties.method= "first")
dados11a$nomes<-ordered(dados11a$nomes, levels=dados11a$nomes[order(dados11a$delta)])
aux<-dados11a
for(i in 1:length(levels(dados11a$nomes))){
  aux[i,]<-dados11a[which(dados11a$RAN==i),]
}
dados11a<-aux;rm(aux)


save(dados11a,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/dados11a.rda")



#########################################################################################################
##########################################################################################


#--------------Davidson vantagem  sem W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagemSemW")
c1DVdg <- mcmc(read.table("cadeia.gamaA.txt"))
c1DVdd <- mcmc(read.table("cadeia.deltaA.txt"))
c1DVdE <- mcmc(read.table("cadeia.lambdaA.txt"))
l1DVd <- read.table("cadeia.lVmA.txt")

c2DVdg <- mcmc(read.table("cadeia.gamaB.txt"))
c2DVdd <- mcmc(read.table("cadeia.deltaB.txt"))
c2DVdE <- mcmc(read.table("cadeia.lambdaB.txt"))

l2DVd <- read.table("cadeia.lVmB.txt")

c3DVdg <- mcmc(read.table("cadeia.gamaC.txt"))
c3DVdd <- mcmc(read.table("cadeia.deltaC.txt"))
c3DVdE <- mcmc(read.table("cadeia.lambdaC.txt"))
l3DVd <- read.table("cadeia.lVmC.txt")


# raftery.diag(c1DVdg)
# raftery.diag(c1DVdd)
# raftery.diag(c1DVdE)
# 
# raftery.diag(c2DVdg)
# raftery.diag(c2DVdd)
# raftery.diag(c2DVdE)
# 
# raftery.diag(c3DVdg)
# raftery.diag(c3DVdd)
# raftery.diag(c3DVdE)
# 
# cadeiag <- mcmc.list(c1DVdg,c2DVdg,c3DVdg)
# gelman.diag(cadeiag)
# cadeiad <- mcmc.list(c1DVdd,c2DVdd,c3DVdd)
# gelman.diag(cadeiad)
# cadeiaE <- mcmc.list(c1DVdE,c2DVdE,c3DVdE)
# gelman.diag(cadeiaE)
# 
# ## CONVERGIU

## log-Verossimilhança Marginal
#l1DVdm <- -2*(mean(l1DVd$V1)+var(l1DVd$V1))
#l2DVdm <- -2*(mean(l2DVd$V1)+var(l2DVd$V1))
#l3DVdm <- -2*(mean(l3DVd$V1)+var(l3DVd$V1))
l1DVdm <- (mean(l1DVd$V1))
l2DVdm <- (mean(l2DVd$V1))
l3DVdm <- (mean(l3DVd$V1))


lVDVdm.vec <- c(l1DVdm,l2DVdm,l3DVdm)
lVmdDV <- mean(lVDVdm.vec )
varlVmDV <- var(lVDVdm.vec)
AICM<--2*lVmdDV+2*varlVmDV 



##########################################################################################
#########################################################################################################
cDVdg <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                     read.table("cadeia.gamaB.txt"),
                     read.table("cadeia.gamaC.txt")))
cDVdd <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                     read.table("cadeia.deltaB.txt"),
                     read.table("cadeia.deltaC.txt")))
cDVdE <- mcmc(rbind(read.table("cadeia.lambdaA.txt"),
                     read.table("cadeia.lambdaB.txt"),
                     read.table("cadeia.lambdaC.txt")))
# plot(cDVdg)
# plot(cDVdd)
# plot(cDVE)

res.gama   <- summary(cDVdg)
dados12<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cDVdg)[,1],
                   ic.sup=HPDinterval(cDVdg)[,2])

dados12$RAN <- rank(dados12$gama,ties.method= "first")
dados12$nomes<-ordered(dados12$nomes, levels=dados12$nomes[order(dados12$gama)])
aux<-dados12
for(i in 1:length(levels(dados12$nomes))){
  aux[i,]<-dados12[which(dados12$RAN==i),]
}
dados12<-aux;rm(aux)
#######
res.delta   <- summary(cDVdd)
dados12d<-data.frame(nomes=estimandos,delta=res.delta[1]$statistics[,1],
                    ic.inf=HPDinterval(cDVdd)[,1],
                    ic.sup=HPDinterval(cDVdd)[,2])

dados12d$RAN <- rank(dados12d$delta,ties.method= "first")
dados12d$nomes<-ordered(dados12d$nomes, levels=dados12d$nomes[order(dados12d$delta)])
aux<-dados12d
for(i in 1:length(levels(dados12d$nomes))){
  aux[i,]<-dados12d[which(dados12d$RAN==i),]
}
dados12d<-aux;rm(aux)

save(dados12,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/dados12.rda")


#########################################################################################################
##########################################################################################


