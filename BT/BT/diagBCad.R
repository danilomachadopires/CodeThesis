rm(list=ls(all=T))
library(coda)
#--------------estimativa de logverssimilhança baseado nas proporções e medias
#---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTcomW")

banco <- read.table("granprix2010a2013.txt",h=T)
nomes <- levels(banco$W)

estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

m  <- length(estimandos)  # número de jogadores analisados
attach(banco)
n  <- length(W)           # número de jogos

################### Veriricar o banco de dados para partidas onde os participantes não serão analisados
GW <- matrix(0,n,m) # matriz de 0s com linhas igual ao número de partidas e colunas igual a jogadores
GB <- matrix(0,n,m)


for(i in 1:m){
  GW[,i] <- as.integer(W==estimandos[i])
  GB[,i] <- as.integer(B==estimandos[i])
}

M <- GW+GB
fora <- which(apply(M,1,sum)==0)
n-length(fora)

GW <- GW[-fora,]
GB <- GB[-fora,]
y <- y[-fora]
Data<-Data[-fora]

WR <- WR[-fora]
BR <- BR[-fora]
B<-B[-fora]
W <- W[-fora]
n <- length(y)

propto    <- (table(y)/length(y))
#propigual <- c((1-propto[2])/2,propto[2],(1-propto[2])/2)    #?????????????????????


#--------------------------------log(Vero)ELO-------------------------------------------
# Descrição   ( Usando o elo como referencia)
pie <-rep(0,length(W))

ELO <- function(theta){
  1/(1+10^((-theta[1]+theta[2])/400))
}

for (j in 1:length(W)){
  Theta <-c(WR[j],BR[j])
  pie[j] <- ELO(Theta)
}

tmin <- min(Data)
tmax <- max(Data)
omega <- 1/sqrt((1+Data-tmin)/(1+tmax-tmin))

#LogElo Dinamico
    AICd<--2*sum(y*omega*log(pie)+(1-y)*omega*log(1-pie)) ;AICd
  
#LogElo estatico
    AICe<--2*sum(y*log(pie)+(1-y)*log(1-pie))   ;AICe


#------------------------------------------------------------------------------------------
# verossimilhança e definete  do modelo  Bradley-Terry supondo que as chances de
# de vitórias são as dadas pelas proporções de resultados verificados.

BVeropropto <- sum(y*log(propto[3])+(1-y)*log((1-propto[3])));
    AICproE<--2*BVeropropto+2;AICproE
BVeroproptow <- sum(y*omega*log(propto[3])+(1-y)*omega*log((1-propto[3])));
  AICproD<--2*BVeroproptow+2;AICproD


AICEquiE<--2*sum(y*log(1/2)+(1-y)*log(1/2))+2;AICEquiE

AICEquiD<--2*sum(y*omega*log(1/2)+(1-y)*omega*log(1/2))+2;AICEquiD




BVeropropigual <- sum(y*log(1964/(1964+1185))+(1-y)*log(1-(1964/(1964+1185))));
-2*BVeropropigual+2

-2*sum(y*omega*log(1964/(1964+1185))+(1-y)*omega*log(1-(1964/(1964+1185))))+2

#------------------------------------------------------------------------------------------






#--------------Bradley-Terry simples com W (tempo)---------------------------------

banco <- read.table("granprix2010a2013.txt",h=T)
nomes <- levels(banco$W)

estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]
m  <- length(estimandos)  # número de jogadores analisados
attach(banco)
n  <- length(W)           # número de jogos


c1BTW <- mcmc(read.table("cadeia.gamaA.txt"))
l1BTW <- read.table("cadeia.lVmA.txt")
c2BTW <- mcmc(read.table("cadeia.gamaB.txt"))
l2BTW <- read.table("cadeia.lVmB.txt")
c3BTW <- mcmc(read.table("cadeia.gamaC.txt"))
l3BTW <- read.table("cadeia.lVmC.txt")
  


l1BTWm <- mean(l1BTW$V1); max((l1BTW$V1))
l2BTWm <- mean(l2BTW$V1);max(l2BTW$V1)
l3BTWm <- mean(l3BTW$V1);max(l3BTW$V1)


lVBTWm.vec <- c(l1BTWm,l2BTWm,l3BTWm)
lVmBTW <- mean(lVBTWm.vec)
varlVmBTW <- var(lVBTWm.vec)
AICM<--2*lVmBTW +2*varlVmBTW


#########################################################################################
cBTW   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                      read.table("cadeia.gamaB.txt"),
                      read.table("cadeia.gamaC.txt")))

#plot(cBTW)

res.gama   <- summary(cBTW)
dados1<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cBTW)[,1],
                   ic.sup=HPDinterval(cBTW)[,2])


dados1$RAN <- rank(dados1$gama,ties.method= "first")
dados1$nomes<-ordered(dados1$nomes,levels=dados1$nomes[order(dados1$gama)])
aux<-dados1
for(i in 1:length(levels(dados1$nomes))){
  aux[i,]<-dados1[which(dados1$RAN==i),]
}
dados1<-aux;rm(aux)

save(dados1,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/dados1.rda")
#########################################################################################


#--------------Bradley-Terry simples sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTsemW")
c1BT <- mcmc(read.table("cadeia.gamaA.txt"))
l1BT <- read.table("cadeia.lVmA.txt")
c2BT <- mcmc(read.table("cadeia.gamaB.txt"))
l2BT <- read.table("cadeia.lVmB.txt")
c3BT <- mcmc(read.table("cadeia.gamaC.txt"))
l3BT <- read.table("cadeia.lVmC.txt")



cadeiag <- mcmc.list(c1BT,c2BT,c3BT)



l1BTm <- (mean(l1BT$V1))
l2BTm <- (mean(l2BT$V1))
l3BTm <- (mean(l3BT$V1))


lVBTm.vec <- c(l1BTm,l2BTm,l3BTm)
lVmBT <- mean(lVBTm.vec)
varlVmBT <- var(lVBTm.vec)
AICM<--2*lVmBT+2*varlVmBT

#################################################################################

cBT   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                     read.table("cadeia.gamaB.txt"),
                     read.table("cadeia.gamaC.txt")))

#plot(cBT)

res.gama   <- summary(cBT)
dados2<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cBT)[,1],
                   ic.sup=HPDinterval(cBT)[,2])

dados2$RAN <- rank(dados2$gama,ties.method= "first")
dados2$nomes<-ordered(dados2$nomes,levels=dados2$nomes[order(dados2$gama)])
aux<-dados2
for(i in 1:length(levels(dados2$nomes))){
  aux[i,]<-dados2[which(dados2$RAN==i),]
}
dados2<-aux;rm(aux)

save(dados2,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/dados2.rda")
#########################################################################################


#--------------Bradley-Terry vantagem (1delta) sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemsemW1delta")
c1BT1dg <- mcmc(read.table("cadeia.gamaA.txt"))
c1BT1dd <- mcmc(read.table("cadeia.deltaA.txt"))
l1BT1d <- read.table("cadeia.lVmA.txt")

c2BT1dg <- mcmc(read.table("cadeia.gamaB.txt"))
c2BT1dd <- mcmc(read.table("cadeia.deltaB.txt"))
l2BT1d <- read.table("cadeia.lVmB.txt")

c3BT1dg <- mcmc(read.table("cadeia.gamaC.txt"))
c3BT1dd <- mcmc(read.table("cadeia.deltaC.txt"))
l3BT1d <- read.table("cadeia.lVmC.txt")




cadeiag <- mcmc.list(c1BT1dg,c2BT1dg,c3BT1dg)
#gelman.diag(cadeiag)
cadeiad <- mcmc.list(c1BT1dd,c2BT1dd,c3BT1dd)
# gelman.diag(cadeiad)

## CONVERGIU


l1BT1dm <- (mean(l1BT1d$V1))
l2BT1dm <- (mean(l2BT1d$V1))
l3BT1dm <- (mean(l3BT1d$V1))


lVBT1dm.vec <- c(l1BT1dm,l2BT1dm,l3BT1dm)
lVm1dBT <- mean(lVBT1dm.vec)
varlVmBT <- var(lVBT1dm.vec)
AICM<--2*lVm1dBT+2*varlVmBT 


#########################################################################################
cBT1dg   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                    read.table("cadeia.gamaB.txt"),
                    read.table("cadeia.gamaC.txt")))

cBT1dd   <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                       read.table("cadeia.deltaB.txt"),
                       read.table("cadeia.deltaC.txt")))

res.gama   <- summary(cBT1dg)
dados4<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cBT1dg)[,1],
                   ic.sup=HPDinterval(cBT1dg)[,2])

dados4$RAN <- rank(dados4$gama,ties.method= "first")
dados4$nomes<-ordered(dados4$nomes,levels=dados4$nomes[order(dados4$gama)])
aux<-dados4
for(i in 1:length(levels(dados4$nomes))){
  aux[i,]<-dados4[which(dados4$RAN==i),]
}
dados4<-aux;rm(aux)

save(dados4,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/dados4.rda")
#########################################################################################

#--------------Bradley-Terry vantagem (1delta) com W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemcomW1delta")
c1BT1Wdg <- mcmc(read.table("cadeia.gamaA.txt"))
c1BT1Wdd <- mcmc(read.table("cadeia.deltaA.txt"))
l1BT1Wd <- read.table("cadeia.lVmA.txt")

c2BT1Wdg <- mcmc(read.table("cadeia.gamaB.txt"))
c2BT1Wdd <- mcmc(read.table("cadeia.deltaB.txt"))
l2BT1Wd <- read.table("cadeia.lVmB.txt")

c3BT1Wdg <- mcmc(read.table("cadeia.gamaC.txt"))
c3BT1Wdd <- mcmc(read.table("cadeia.deltaC.txt"))
l3BT1Wd <- read.table("cadeia.lVmC.txt")


# 
cadeiag <- mcmc.list(c1BT1Wdg,c2BT1Wdg,c3BT1Wdg)

cadeiad <- mcmc.list(c1BT1Wdd,c2BT1Wdd,c3BT1Wdd)


l1BT1Wdm <- (mean(l1BT1Wd$V1))
l2BT1Wdm <- (mean(l2BT1Wd$V1))
l3BT1Wdm <- (mean(l3BT1Wd$V1))


lVBT1Wdm.vec <- c(l1BT1Wdm,l2BT1Wdm,l3BT1Wdm)
lVm1WdBT <- mean(lVBT1Wdm.vec )
varlVWmBT <- var(lVBT1Wdm.vec  )
AICM<--2*lVm1dBT +2*varlVWmBT 


#########################################################################################
cBT1Wdg   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                       read.table("cadeia.gamaB.txt"),
                       read.table("cadeia.gamaC.txt")))
cBT1Wdd   <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                        read.table("cadeia.deltaB.txt"),
                        read.table("cadeia.deltaC.txt")))


res.gama   <- summary(cBT1Wdg)
dados3<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cBT1Wdg)[,1],
                   ic.sup=HPDinterval(cBT1Wdg)[,2])

dados3$RAN <- rank(dados3$gama,ties.method= "first")
dados3$nomes<-ordered(dados3$nomes,levels=dados3$nomes[order(dados3$gama)])
aux<-dados3
for(i in 1:length(levels(dados3$nomes))){
  aux[i,]<-dados3[which(dados3$RAN==i),]
}
dados3<-aux;rm(aux)

save(dados3,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/dados3.rda")
#########################################################################################



#--------------Bradley-Terry vantagem  com W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemComW")

c1BTWdg <- mcmc(read.table("cadeia.gamaA.txt"))
c1BTWdd <- mcmc(read.table("cadeia.deltaA.txt"))
l1BTWd <- read.table("cadeia.lVmA.txt")

c2BTWdg <- mcmc(read.table("cadeia.gamaB.txt"))
c2BTWdd <- mcmc(read.table("cadeia.deltaB.txt"))
l2BTWd <- read.table("cadeia.lVmB.txt")

c3BTWdg <- mcmc(read.table("cadeia.gamaC.txt"))
c3BTWdd <- mcmc(read.table("cadeia.deltaC.txt"))
l3BTWd <- read.table("cadeia.lVmC.txt")


# 
cadeiag <- mcmc.list(c1BTWdg,c2BTWdg,c3BTWdg)

cadeiad <- mcmc.list(c1BTWdd,c2BTWdd,c3BTWdd)


l1BTWdm <- (mean(l1BTWd$V1))
l2BTWdm <- (mean(l2BTWd$V1))
l3BTWdm <- (mean(l3BTWd$V1))

lVBTWdm.vec <- c(l1BTWdm,l2BTWdm,l3BTWdm)
lVmWdBT <- mean(lVBTWdm.vec )
varlVWmBT <- var(lVBTWdm.vec)
AICM<--2*lVmWdBT +2*varlVWmBT 


#########################################################################################
cBTWdg   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                        read.table("cadeia.gamaB.txt"),
                        read.table("cadeia.gamaC.txt")))

cBTWdd <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                     read.table("cadeia.deltaB.txt"),
                     read.table("cadeia.deltaC.txt")))



res.gama   <- summary(cBTWdg)
dados5<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cBTWdg)[,1],
                   ic.sup=HPDinterval(cBTWdg)[,2])

dados5$RAN <- rank(dados5$gama,ties.method= "first")
dados5$nomes<-ordered(dados5$nomes,levels=dados5$nomes[order(dados5$gama)])
aux<-dados5
for(i in 1:length(levels(dados5$nomes))){
  aux[i,]<-dados5[which(dados5$RAN==i),]
}
dados5<-aux;rm(aux)

res.delta   <- summary(cBTWdd)
dados5d<-data.frame(nomes=estimandos,delta=res.delta[1]$statistics[,1],
                   ic.inf=HPDinterval(cBTWdd)[,1],
                   ic.sup=HPDinterval(cBTWdd)[,2])

dados5d$RAN <- rank(dados5d$delta,ties.method= "first")
dados5d$nomes<-ordered(dados5d$nomes,levels=dados5d$nomes[order(dados5d$delta)])
aux<-dados5d
for(i in 1:length(levels(dados5d$nomes))){
  aux[i,]<-dados5d[which(dados5d$RAN==i),]
}
dados5d<-aux;rm(aux)


save(dados5,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/dados5.rda")
#########################################################################################


#--------------Bradley-Terry vantagem  sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemsemW")

c1BTdg <- mcmc(read.table("cadeia.gamaA.txt"))
c1BTdd <- mcmc(read.table("cadeia.deltaA.txt"))
l1BTd <- read.table("cadeia.lVmA.txt")

c2BTdg <- mcmc(read.table("cadeia.gamaB.txt"))
c2BTdd <- mcmc(read.table("cadeia.deltaB.txt"))
l2BTd <- read.table("cadeia.lVmB.txt")

c3BTdg <- mcmc(read.table("cadeia.gamaC.txt"))
c3BTdd <- mcmc(read.table("cadeia.deltaC.txt"))
l3BTd <- read.table("cadeia.lVmC.txt")



cadeiag <- mcmc.list(c1BTdg,c2BTdg,c3BTdg)

cadeiad <- mcmc.list(c1BTdd,c2BTdd,c3BTdd)


l1BTdm <- (mean(l1BTd$V1))
l2BTdm <- (mean(l2BTd$V1))
l3BTdm <- (mean(l3BTd$V1))

lVBTdm.vec <- c(l1BTdm,l2BTdm,l3BTdm)
lVmdBT <- mean(lVBTdm.vec )
varlVmBT <- var(lVBTdm.vec)
AICM<--2*lVmdBT +2*varlVmBT 


#########################################################################################
cBTdg   <- mcmc(rbind(read.table("cadeia.gamaA.txt"),
                       read.table("cadeia.gamaB.txt"),
                       read.table("cadeia.gamaC.txt")))
cBTdd   <- mcmc(rbind(read.table("cadeia.deltaA.txt"),
                      read.table("cadeia.deltaB.txt"),
                      read.table("cadeia.deltaC.txt")))




res.gama   <- summary(cBTdg)
dados6<-data.frame(nomes=estimandos,gama=res.gama[1]$statistics[,1],
                   ic.inf=HPDinterval(cBTdg)[,1],
                   ic.sup=HPDinterval(cBTdg)[,2])

dados6$RAN <- rank(dados6$gama,ties.method= "first")
dados6$nomes<-ordered(dados6$nomes,levels=dados6$nomes[order(dados6$gama)])
aux<-dados6
for(i in 1:length(levels(dados6$nomes))){
  aux[i,]<-dados6[which(dados6$RAN==i),]
}
dados6<-aux;rm(aux)

res.delta  <- summary(cBTdd)
dados6d<-data.frame(nomes=estimandos,delta=res.delta[1]$statistics[,1],
                   ic.inf=HPDinterval(cBTdd)[,1],
                   ic.sup=HPDinterval(cBTdd)[,2])

dados6d$RAN <- rank(dados6d$delta,ties.method= "first")
dados6d$nomes<-ordered(dados6d$nomes,levels=dados6d$nomes[order(dados6d$delta)])
aux<-dados6d
for(i in 1:length(levels(dados6d$nomes))){
  aux[i,]<-dados6d[which(dados6d$RAN==i),]
}
dados6d<-aux;rm(aux)





save(dados6d,file="C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/dados6d.rda")

#########################################################################################































