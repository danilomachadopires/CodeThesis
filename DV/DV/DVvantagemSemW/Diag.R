rm(list=ls(all=T))

require(latticeExtra)
library(coda)
dados <- read.table("granprix2010a2013.txt",h=T)
attach(dados)

nomes <- levels(W)

estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

cadeia.gamaA   <- read.table("cadeia.gamaA.txt")
cadeia.lambdaA <- read.table("cadeia.lambdaA.txt")
cadeia.deltaA <- read.table("cadeia.deltaA.txt")

cadeia.gamaB   <- read.table("cadeia.gamaB.txt")
cadeia.lambdaB <- read.table("cadeia.lambdaB.txt")
cadeia.deltaB <- read.table("cadeia.deltaB.txt")

cadeia.gamaC   <- read.table("cadeia.gamaC.txt")
cadeia.lambdaC <- read.table("cadeia.lambdaC.txt")
cadeia.deltaC <- read.table("cadeia.deltaC.txt")


cadeia.gamaA   <- mcmc(cadeia.gamaA)
cadeia.lambdaA <- mcmc(cadeia.lambdaA)
cadeia.deltaA <- mcmc(cadeia.deltaA)

cadeia.gamaB   <- mcmc(cadeia.gamaB)
cadeia.lambdaB <- mcmc(cadeia.lambdaB)
cadeia.deltaB <- mcmc(cadeia.deltaB)

cadeia.gamaC   <- mcmc(cadeia.gamaC)
cadeia.lambdaC <- mcmc(cadeia.lambdaC)
cadeia.deltaC <- mcmc(cadeia.deltaC)

rd.gamaA    <- raftery.diag(cadeia.gamaA)$resmatrix
res.gamaA   <- summary(cadeia.gamaA)
rd.lambdaA  <- raftery.diag(cadeia.lambdaA)$resmatrix
res.lambdaA <- summary(cadeia.lambdaA)
rd.deltaA  <- raftery.diag(cadeia.deltaA)$resmatrix
res.deltaA <-summary(cadeia.deltaA)

rd.gamaB    <- raftery.diag(cadeia.gamaB)$resmatrix
res.gamaB   <- summary(cadeia.gamaB)
rd.lambdaB  <- raftery.diag(cadeia.lambdaB)$resmatrix
res.lambdaB <- summary(cadeia.lambdaB)
rd.deltaB  <- raftery.diag(cadeia.deltaB)$resmatrix
res.deltaB <-summary(cadeia.deltaB)

rd.gamaC    <- raftery.diag(cadeia.gamaC)$resmatrix
res.gamaC   <- summary(cadeia.gamaC)
rd.lambdaC  <- raftery.diag(cadeia.lambdaC)$resmatrix
res.lambdaC <- summary(cadeia.lambdaC)
rd.deltaC  <- raftery.diag(cadeia.deltaC)$resmatrix
res.deltaC <-summary(cadeia.deltaC)


rd.gamaA
res.gamaA
rd.lambdaA
res.lambdaA
rd.deltaA
res.deltaA

rd.gamaB
res.gamaB
rd.lambdaB
res.lambdaB
rd.deltaB
res.deltaB

rd.gamaC
res.gamaC
rd.lambdaC
res.lambdaC
rd.deltaC
res.deltaC

apply(rd.gamaA,2,max);apply(rd.lambdaA,2,max);apply(rd.deltaA,2,max)
apply(rd.gamaB,2,max);apply(rd.lambdaB,2,max);apply(rd.deltaB,2,max)
apply(rd.gamaC,2,max);apply(rd.lambdaC,2,max);apply(rd.deltaC,2,max)

plot(cadeia.gamaA)
plot(cadeia.lambdaA)
plot(cadeia.deltaA)

plot(cadeia.gamaB)
plot(cadeia.lambdaB)
plot(cadeia.deltaB)

plot(cadeia.gamaC)
plot(cadeia.lambdaC)
plot(cadeia.deltaC)


HPDinterval(cadeia.gamaA)
dadosA<-data.frame(nomes=estimandos,gama=res.gamaA[1]$statistics[,1],
                  ic.inf=HPDinterval(cadeia.gamaA)[,1],
                  ic.sup=HPDinterval(cadeia.gamaA)[,2])

dados1A<-data.frame(nomes=estimandos,delta=res.deltaA[1]$statistics[,1],
                  ic.inf=HPDinterval(cadeia.deltaA)[,1],
                  ic.sup=HPDinterval(cadeia.deltaA)[,2])


HPDinterval(cadeia.gamaB)
dadosB<-data.frame(nomes=estimandos,gama=res.gamaB[1]$statistics[,1],
                   ic.inf=HPDinterval(cadeia.gamaB)[,1],
                   ic.sup=HPDinterval(cadeia.gamaB)[,2])

dados1B<-data.frame(nomes=estimandos,delta=res.deltaB[1]$statistics[,1],
                    ic.inf=HPDinterval(cadeia.deltaB)[,1],
                    ic.sup=HPDinterval(cadeia.deltaB)[,2])


HPDinterval(cadeia.gamaC)
dadosC<-data.frame(nomes=estimandos,gama=res.gamaC[1]$statistics[,1],
                   ic.inf=HPDinterval(cadeia.gamaC)[,1],
                   ic.sup=HPDinterval(cadeia.gamaC)[,2])

dados1C<-data.frame(nomes=estimandos,delta=res.deltaC[1]$statistics[,1],
                    ic.inf=HPDinterval(cadeia.deltaC)[,1],
                    ic.sup=HPDinterval(cadeia.deltaC)[,2])

# organizando o gama
dadosA$RAN <- rank(dadosA$gama,ties.method= "first")
dadosA$nomes<-ordered(dadosA$nomes, levels=dadosA$nomes[order(dadosA$gama)])
aux<-dadosA
for(i in 1:length(levels(dadosA$nomes))){
  aux[i,]<-dadosA[which(dadosA$RAN==i),]
}
dadosA<-aux;rm(aux)

dadosB$RAN <- rank(dadosB$gama,ties.method= "first")
dadosB$nomes<-ordered(dadosB$nomes, levels=dadosB$nomes[order(dadosB$gama)])
aux<-dadosB
for(i in 1:length(levels(dadosB$nomes))){
  aux[i,]<-dadosB[which(dadosB$RAN==i),]
}
dadosB<-aux;rm(aux)


dadosC$RAN <- rank(dadosC$gama,ties.method= "first")
dadosC$nomes<-ordered(dadosC$nomes, levels=dadosC$nomes[order(dadosC$gama)])
aux<-dadosC
for(i in 1:length(levels(dadosC$nomes))){
  aux[i,]<-dadosC[which(dadosC$RAN==i),]
}
dadosC<-aux;rm(aux)

#organizando o delta
dados1A$RAN <- rank(dados1A$delta,ties.method= "first")
dados1A$nomes<-ordered(dados1A$nomes, levels=dados1A$nomes[order(dados1A$delta)])
aux<-dados1A
for(i in 1:length(levels(dados1A$nomes))){
  aux[i,]<-dados1A[which(dados1A$RAN==i),]
}
dados1A<-aux;rm(aux)

dados1B$RAN <- rank(dados1B$delta,ties.method= "first")
dados1B$nomes<-ordered(dados1B$nomes, levels=dados1B$nomes[order(dados1B$delta)])
aux<-dados1B
for(i in 1:length(levels(dados1B$nomes))){
  aux[i,]<-dados1B[which(dados1B$RAN==i),]
}
dados1B<-aux;rm(aux)

dados1C$RAN <- rank(dados1C$delta,ties.method= "first")
dados1C$nomes<-ordered(dados1C$nomes, levels=dados1C$nomes[order(dados1C$delta)])
aux<-dados1C
for(i in 1:length(levels(dados1C$nomes))){
  aux[i,]<-dados1C[which(dados1C$RAN==i),]
}
dados1C<-aux;rm(aux)


save(dadosA,file="dadosA.rda");save(dados1A,file="dados1A.rda")
save(dadosB,file="dadosB.rda");save(dados1B,file="dados1B.rda")
save(dadosC,file="dadosC.rda");save(dados1C,file="dados1C.rda")

#gama
attach(dadosA)
LI <- ic.inf
LS <-ic.sup 
Trat <- factor(dadosA$nomes)
segplot (Trat ~ LI + LS, center= gama, col="gray50", 
         xlab="IC", ylab="Tratamentos",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

detach(dadosA,pos=dadosA)

attach(dadosB)
LI <- ic.inf
LS <-ic.sup 
Trat <- factor(dadosB$nomes)
segplot (Trat ~ LI + LS, center= gama, col="gray50", 
         xlab="IC", ylab="Tratamentos",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

detach(dadosB,pos=dadosB)

attach(dadosC)
LI <- ic.inf
LS <-ic.sup 
Trat <- factor(dadosC$nomes)
segplot (Trat ~ LI + LS, center= gama, col="gray50", 
         xlab="IC", ylab="Tratamentos",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

detach(dadosC,pos=dadosC)


#delta
attach(dados1A)
LI <- ic.inf
LS <-ic.sup 
Trat <- factor(dados1A$nomes)
segplot (Trat ~ LI + LS, center= delta, col="gray50", 
         xlab="IC", ylab="Tratamentos",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)
detach(dados1A,pos=dados1A)

attach(dados1B)
LI <- ic.inf
LS <-ic.sup 
Trat <- factor(dados1B$nomes)
segplot (Trat ~ LI + LS, center= delta, col="gray50", 
         xlab="IC", ylab="Tratamentos",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)
detach(dados1B,pos=dados1B)

attach(dados1C)
LI <- ic.inf
LS <-ic.sup 
Trat <- factor(dados1C$nomes)
segplot (Trat ~ LI + LS, center= delta, col="gray50", 
         xlab="IC", ylab="Tratamentos",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)
detach(dados1C,pos=dados1C)

#lambda
round(cbind(HPDinterval(cadeia.lambdaA)[1],res.lambdaA[1]$statistic[1],HPDinterval(cadeia.lambdaA)[2]),4)
round(cbind(HPDinterval(cadeia.lambdaB)[1],res.lambdaB[1]$statistic[1],HPDinterval(cadeia.lambdaB)[2]),4)
round(cbind(HPDinterval(cadeia.lambdaC)[1],res.lambdaC[1]$statistic[1],HPDinterval(cadeia.lambdaC)[2]),4)
