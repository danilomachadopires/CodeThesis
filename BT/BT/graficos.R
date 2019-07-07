rm(list=ls(all=T))
require(latticeExtra)
require(gplots)
load("dados1.rda")
LI <- dados1$ic.inf
LS <-dados1$ic.sup 
Trat <- factor(dados1$nomes)
segplot (Trat ~ LI + LS, center= dados1$gama,   panel.lines(2750),
         col="gray50", 
         main="BT simples com W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)
axis(4)

panel.refline(2700)

load("dados2.rda")
LI <- dados2$ic.inf
LS <-dados2$ic.sup 
Trat <- factor(dados2$nomes)
segplot (Trat ~ LI + LS, center= dados2$gama, col="gray50", 
         main="BT simples sem W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados3.rda")
LI <- dados3$ic.inf
LS <-dados3$ic.sup 
Trat <- factor(dados3$nomes)
segplot (Trat ~ LI + LS, center= dados3$gama, col="gray50", 
         xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

postscript('BT1deltacomW.eps')
barplot2(dados3$gama, # A matriz a ser plotada
         names.arg=dados3$nomes,
         #main = dados4$nomes,
         #legend=legenda,
         beside=T, # Plota as barras lado-a-lado
         col=par("ask"),
         border=par("ask"),# cores das bordas da barras
         axes=T,       
         axisnames = T,
         axis.lty = 25,
         xpd = F,
         plot.ci=T, # TRUE, deve-se plotar os intervalos de confian?a
         ci.l=LI, # A matriz dos limites inferiores dos intervalos de confian?a
         ci.u=LS, # A matriz dos limites superiores dos intervalos de confian?a
         cex.names=0.6,
         ylim=c(min(LI)*0.99,max(LS)*1.01),
         las=2    
)
abline(h=max(LI))
abline(h=min(LS))
abline(h=mean(dados3$gama),lty=2)
par(new=T)
plot(dados3$nomes,dados3$gama,ylim=c(min(LI)*0.99,max(LS)*1.01),ylab='CL',xlab='',axes=F)
dev.off()




#

load("dados4.rda")
LI <- dados4$ic.inf
LS <- dados4$ic.sup 
Trat <- factor(dados4$nomes)
segplot (Trat ~ LI + LS, center= dados4$gama, col="gray50", 
    xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)


postscript('BT1deltasemW.eps')
barplot2(dados4$gama, # A matriz a ser plotada
         names.arg=dados4$nomes,
         #main = dados4$nomes,
          #legend=legenda,
         beside=T, # Plota as barras lado-a-lado
         col=par("ask"),
         border=par("ask"),# cores das bordas da barras
         axes=T,       
         axisnames = T,
         axis.lty = 25,
         xpd = F,
         plot.ci=T, # TRUE, deve-se plotar os intervalos de confian?a
         ci.l=LI, # A matriz dos limites inferiores dos intervalos de confian?a
         ci.u=LS, # A matriz dos limites superiores dos intervalos de confian?a
         cex.names=0.6,
         ylim=c(min(LI)*0.99,max(LS)*1.01),
         las=2    
)
abline(h=max(LI))
abline(h=min(LS))
abline(h=mean(dados4$gama),lty=2)
par(new=T)
plot(dados4$nomes,dados4$gama,ylim=c(min(LI)*0.99,max(LS)*1.01),ylab='CL',xlab='',axes=F)
dev.off()

load("dados5.rda")
LI <- dados5$ic.inf
LS <- dados5$ic.sup 
Trat <- factor(dados5$nomes)
segplot (Trat ~ LI + LS, center= dados5$gama, col="gray50", 
         main="BT  deltas com W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados5d.rda")
LI <- dados5d$ic.inf
LS <- dados5d$ic.sup 
Trat <- factor(dados5d$nomes)
segplot (Trat ~ LI + LS, center= dados5d$delta, col="gray50", 
         main="BT  deltas com W",xlab="IC-delta", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)



load("dados6.rda")
LI <- dados6$ic.inf
LS <- dados6$ic.sup 
Trat <- factor(dados6$nomes)
segplot (Trat ~ LI + LS, center= dados6$gama, col="gray50", 
         main="BT deltas sem W",xlab="IC", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados6d.rda")
LI <- dados6d$ic.inf
LS <- dados6d$ic.sup 
Trat <- factor(dados6d$nomes)
segplot (Trat ~ LI + LS, center= dados6d$delta, col="gray50", 
         main="BT  deltas sem W",xlab="IC-delta", ylab="Jogadores",
         draw.bands=F, segments.fun=panel.arrows, ends="both",
         angle=90, length=1, unit="mm", las=2)

load("dados1.rda")
load("dados2.rda")
load("dados3.rda")
load("dados4.rda")
load("dados5.rda")
load("dados6.rda")
tudo<-rbind(dados1,dados2,dados3,dados4,dados5,dados6)
tudo<-tudo[,1:2]
nomes
ratings<-matrix(0,46,6)

for(i in 1:46){
  ratings[i,]<-tudo$gama[which(nomes[i]==tudo[,1])]
}

medias<-data.frame(nomes=nomes,BTsw=ratings[,1],BTcw=ratings[,2],Bt1sw=ratings[,3],
           Bt1cw=ratings[,4],Btvsw=ratings[,5],Btvcw=ratings[,6])