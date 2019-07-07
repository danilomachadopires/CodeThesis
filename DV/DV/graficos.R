rm(list=ls(all=T))
require(latticeExtra)
load("dados7.rda")
LI <- dados7$ic.inf
LS <-dados7$ic.sup 
Trat <- factor(dados7$nomes)
segplot (Trat ~ LI + LS, center= dados7$gama, col="gray50", 
         main="DV simples com W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados8.rda")
LI <- dados8$ic.inf
LS <-dados8$ic.sup 
Trat <- factor(dados8$nomes)
segplot (Trat ~ LI + LS, center= dados8$gama, col="gray50", 
         main="DV simples sem W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados9.rda")
LI <- dados9$ic.inf
LS <-dados9$ic.sup 
Trat <- factor(dados9$nomes)
segplot (Trat ~ LI + LS, center= dados9$gama, col="gray50", 
         main="DV 1 delta com W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados10.rda")
LI <- dados10$ic.inf
LS <- dados10$ic.sup 
Trat <- factor(dados10$nomes)
segplot (Trat ~ LI + LS, center= dados10$gama, col="gray50", 
         main="DV 1 delta sem W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

#load("dados11.rda")
#LI <- dados11$ic.inf
#LS <- dados11$ic.sup 
#Trat <- factor(dados11$nomes)
#segplot (Trat ~ LI + LS, center= dados11$gama, col="gray50", 
 #        main="DV  deltas com W",xlab="IC", ylab="Jogadores",
  #       draw.bands=F, segments.fun=panel.arrows, ends="both",
   #      angle=90, length=1, unit="mm", las=2)

load("dados11.rda")
LI <- dados11$ic.inf
LS <- dados11$ic.sup 
Trat <- factor(dados11$nomes)
segplot (Trat ~ LI + LS, center= dados11$gama, col="gray50", 
          xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)


#load("dados11a.rda")
#LI <- dados11a$ic.inf
#LS <- dados11a$ic.sup 
#Trat <- factor(dados11a$nomes)
#segplot (Trat ~ LI + LS, center= dados11a$delta, col="gray50", 
 #        main="DV deltas com W",xlab="IC-delta", ylab="Jogadores",
  #       draw.bands=F, segments.fun=panel.arrows, ends="both",
   #      angle=90, length=1, unit="mm", las=2)


load("dados11a.rda")
LI <- dados11a$ic.inf
LS <- dados11a$ic.sup 
Trat <- factor(dados11a$nomes)
segplot (Trat ~ LI + LS, center= dados11a$delta, col="gray50", 
    xlab="IC-delta", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)



#load("dados12.rda")
#LI <- dados12$ic.inf
#LS <- dados12$ic.sup 
#Trat <- factor(dados12$nomes)
#segplot (Trat ~ LI + LS, center= dados12$gama, col="gray50", 
 #        main="DV delta sem W",xlab="IC", ylab="Jogadores",
  #       draw.bands=F, segments.fun=panel.arrows, ends="both",
   #      angle=90, length=1, unit="mm", las=2)

load("dados12.rda")
LI <- dados12$ic.inf
LS <- dados12$ic.sup 
Trat <- factor(dados12$nomes)
segplot (Trat ~ LI + LS, center= dados12$gama, col="gray50", 
         xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)


#load("dados12d.rda")
#LI <- dados12d$ic.inf
#LS <- dados12d$ic.sup 
#Trat <- factor(dados12d$nomes)
#segplot (Trat ~ LI + LS, center= dados12d$delta, col="gray50", 
 #        main="DV deltas sem W",xlab="IC-delta", ylab="Jogadores",
  #       draw.bands=F, segments.fun=panel.arrows, ends="both",
   #      angle=90, length=1, unit="mm", las=2)


load("dados12d.rda")
LI <- dados12d$ic.inf
LS <- dados12d$ic.sup 
Trat <- factor(dados12d$nomes)
segplot (Trat ~ LI + LS, center= dados12d$delta, col="gray50", 
        xlab="IC-delta", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)


load("dados7.rda")
load("dados8.rda")
load("dados9.rda")
load("dados10.rda")
load("dados11.rda")
load("dados12.rda")
tudo<-rbind(dados7,dados8,dados9,dados10,dados11,dados12)
tudo<-tudo[,1:2]
nomes<-dados7$nomes
ratings<-matrix(0,46,6)


for(i in 1:46){
  ratings[i,]<-tudo$gama[which(nomes[i]==tudo[,1])]
}

medias<-data.frame(nomes=nomes,BTsw=ratings[,1],BTcw=ratings[,2],Bt1sw=ratings[,3],
                   Bt1cw=ratings[,4],Btvsw=ratings[,5],Btvcw=ratings[,6])