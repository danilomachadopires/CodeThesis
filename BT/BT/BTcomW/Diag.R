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


cadeia.gamaA   <- mcmc(read.table("cadeia.gamaA.txt"))
cadeia.gamaB   <- mcmc(read.table("cadeia.gamaB.txt"))
cadeia.gamaC   <- mcmc(read.table("cadeia.gamaC.txt"))

rd.gamaA    <- raftery.diag(cadeia.gamaA)$resmatrix
res.gamaA   <- summary(cadeia.gamaA)
rd.gamaB    <- raftery.diag(cadeia.gamaB)$resmatrix
res.gamaB   <- summary(cadeia.gamaB)
rd.gamaC    <- raftery.diag(cadeia.gamaC)$resmatrix
res.gamaC   <- summary(cadeia.gamaC)



HPDinterval(cadeia.gamaA)
dadosA<-data.frame(nomes=estimandos,gama=res.gamaA[1]$statistics[,1],
                  ic.inf=HPDinterval(cadeia.gamaA)[,1],
                  ic.sup=HPDinterval(cadeia.gamaA)[,2])

HPDinterval(cadeia.gamaB)
dadosB<-data.frame(nomes=estimandos,gama=res.gamaB[1]$statistics[,1],
                  ic.inf=HPDinterval(cadeia.gamaB)[,1],
                  ic.sup=HPDinterval(cadeia.gamaB)[,2])


HPDinterval(cadeia.gamaC)
dadosC<-data.frame(nomes=estimandos,gama=res.gamaC[1]$statistics[,1],
                  ic.inf=HPDinterval(cadeia.gamaC)[,1],
                  ic.sup=HPDinterval(cadeia.gamaC)[,2])


# organizes the data
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




save(dadosA,file="dadosA.rda");save(dadosB,file="dadosB.rda");save(dadosC,file="dadosC.rda")

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

