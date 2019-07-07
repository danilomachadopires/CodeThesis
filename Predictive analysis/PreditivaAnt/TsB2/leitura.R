###############################################################
##### Compara os 12 modelos quanto a sua distribuição preditiva
##### Modelos ajustado com jogos entre 2010 e 2012
##### Preditiva calculada com jogos de 2013

rm(list=ls(all=T))
load("meddistb2.rda")
load("mediasLogb2.rda")
load("vardistb2.rda")
load("varLogb2.rda")
load("banco3.rda")
attach(banco3)




#--------------------------Elo Referencia----------------------------
#--------------------------Elo Referencia----------------------------
#--------------------------Elo Referencia----------------------------
estimados <- sort(unique(c(as.character(W),as.character(B))))
ratingestimados<-c(2710,2775,2723,2815,2703,2848,2782,2702,2734,
                   2751,2720,2764,2766,2741,2762,2775,2702,2795,
                   2732,2705,2764,2710,2748,2760,2707,2732,2793,
                   2705,2747,2725,2771,2711,2694,2694,2737,2696)#12/2012
ratingsElo<-cbind(estimados,ratingestimados);ratingsElo


pie <-rep(0,length(W))

ELO <- function(theta){
  1/(1+10^((-theta[1]+theta[2])/400))
}



for (j in 1:length(banco3$W)){
  branca <- banco3$W[j]
  negra  <- banco3$B[j]
  RBr    <- as.numeric(ratingsElo[which(ratingsElo[,1]==branca),2])
  RNe    <- as.numeric(ratingsElo[which(ratingsElo[,1]==negra),2])
  Theta <-cbind(RBr,RNe)
  pie[j] <- ELO(Theta)
}


AIC<--2*sum(y*log(pie)+(1-y)*log(1-pie))  


DeFineti<-mean(abs(y-pie))


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------

propto    <- (table(y)/length(y));

BVeropropto <- sum(y*log(propto[3])+(1-y)*log(1-propto[3]));
AIC<--2*BVeropropto+2

DeFineti<-mean(abs(y-propto[3]))


BVeropropigual <- sum(y*log(234/(234+154))+(1-y)*log(1-(234/(234+154))));
AIC<--2*BVeropropigual+2

DeFineti<-mean(abs(y-(234/(234+154))))

AIC<--2*sum(y*log(1/2)+(1-y)*log(1/2))+2
DeFineti<-mean(abs(y-1/2))

#------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------
##  Verossimilhança e definet do modelo Davidson que as chances de vitória, empates e derrotas
## são as mesmas ou seja 1/33...

Y <- matrix(0,length(W),3)
Y[y==0,1]  <- 1 
Y[y==.5,2] <- 1
Y[y==1,3]  <- 1

DVeropropto <- sum(Y[,1]*log(propto[1])+Y[,2]*log(propto[2])+Y[,3]*log(propto[3]));
AIC<--2*DVeropropto+4
DeFineti<-mean(sqrt((Y[,1]-propto[1])^2 +(Y[,2]-propto[2])^2  +(Y[,3]-propto[3])^2 ))

AIC<--2*sum(Y[,1]*log(1/3)+Y[,2]*log(1/3)+Y[,3]*log(1/3))+4
DeFineti<-mean(sqrt((Y[,1]-1/3)^2 +(Y[,2]-1/3)^2  +(Y[,3]-1/3)^2 ))

#------------------------------------------------------------------------------
# medias e variancias do log da verossimilhança e medias e variancias da medida
# definet para os 6 modelos de Bradley-Terry e Davidosn

resultados <- cbind(apply(meddist,2,mean), 
                    apply(vardist,2,mean),
                    apply(mediasLog,2,sum),
                    apply(varLog,2,mean) )

# Bradley-Terry simples com W (tempo)
# Bradley-Terry simples sem W (tempo)
# Bradley-Terry  com 1 dela e W (tempo)
# Bradley-Terry com 1 dela sem W (tempo)
# Bradley-Terry  com  delta com W (tempo)
# Bradley-Terry  com  delta sem W (tempo)
          # Modelos tipo Bradley-Terry
BT    <- resultados[1:6,];BT
BT[,4]   # variancia log
BT[,2]   # variancia distancia
BT[,1]   # distancia


# Davidson simples com W (tempo)
# Davidson simples sem W (tempo)
# Davidson com 1 delta e W (tempo)
# Davidson com 1 delta sem W (tempo)
# Davidson com delta e W (tempo)
# Davidson com delta sem W (tempo)
          # Modelos tipo Davidson
DV    <- resultados[7:12,];DV
DV[,4]    # variancia log   
DV[,2]     # variancia distancia
DV[,1]     # distancia



##### Estimativas de AICm (quanto menor o valor melhor o ajuste)
# Modelos tipo Bradley-Terry
# com e sem
AIC<--2*resultados[1:6,3]+2*resultados[1:6,4]


#Bradley-Terry
cbind(c(resultados[1:6,1]),rank(c(resultados[1:6,1]))) # Distancia Definet

# Modelos tipo Davidson
AIC<--2*resultados[7:12,3]+2*resultados[7:12,4]




##### Distância (medida adequação de DeFinetti)
#Bradley-Terry
cbind(c(resultados[1:6,1]),rank(c(resultados[1:6,1]))) 
# Modelos tipo Davidson
cbind(c(resultados[7:12,1]),rank(c(resultados[7:12,1])))


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
