
#PREDICAO DE PARTIDAS ONDE OS DOIS JOGADORES TIVERAM OS RATINGS ESTIMADOS

rm(list=ls(all=T))
source("funcoes.R")

# janeiro de 2010 a novembro de 2012

    banco  <- read.table("granprix2010a2013.txt",h=T)  
    nomes  <- levels(banco$W)

    estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

       m   <- length(estimandos)

#janeiro a agosto de 2013

     banco2 <- read.table("BancoDados2013.txt",h=T) 
attach(banco2)

    nomes2 <- (unique(c(as.character(W),as.character(B))))
         n <- length(W)

# matriz de 0s com linhas igual ao número de partidas e colunas igual a jogadores

        GW <- matrix(0,n,m) 
        GB <- matrix(0,n,m)


#preenche com 1 as linhas referentes a partidas com jogadores de rating estimados

for(i in 1:m){
    GW[,i] <- as.integer(W==estimandos[i])
    GB[,i] <- as.integer(B==estimandos[i])
}

         M <- GW+GB

# Linhas em que não tem os dois jogadores com rating estimados

 foralinhas <- c(which(apply(M,1,sum)==0),which(apply(M,1,sum)==1)) 

detach(banco2,pos=banco2)

# novo banco somente com partidas de jogadores de ratings estimados
     banco4 <- (banco2[-foralinhas,])

rm(banco,banco2,n,nomes,GB,GW,i,nomes2)

attach(banco4)
save(banco4,file="banco4.rda")

   estimados <- sort(unique(c(as.character(W),as.character(B))))
#    ratingestimados<-c(2710,2775,2723,2815,2703,2848,2782,2702,2734,
#                       2751,2720,2764,2766,2741,2762,2775,2702,2795,
#                       2732,2705,2764,2710,2748,2760,2707,2732,2793,
#                       2705,2747,2725,2771,2711,2694,2694,2737,2696)#12/12
# ratingsElo<-cbind(estimados,ratingestimados);ratingsElo
# 
# 
# 
# 
# #--------------------------Elo Referencia----------------------------
# #--------------------------Elo Referencia----------------------------
# #--------------------------Elo Referencia----------------------------
# pie <-rep(0,length(W))
# 
# ELO <- function(theta){
#   1/(1+10^((-theta[1]+theta[2])/400))
# }
# 
# 
# 
# for (j in 1:length(banco4$W)){
#   branca <- banco4$W[j]
#   negra  <- banco4$B[j]
#   RBr    <- as.numeric(ratingsElo[which(ratingsElo[,1]==branca),2])
#   RNe    <- as.numeric(ratingsElo[which(ratingsElo[,1]==negra),2])
#   Theta <-cbind(RBr,RNe)
#   pie[j] <- BTt.Dif1(Theta)
# }
# 
# 
# -2*sum(y*log(pie)+(1-y)*log(1-pie))  
# 
# 
# mean(abs(y-pie))
# 
# 
# sum(table(y))
# prop<-114/411
# propa<-114/(114+62)
# 
# mean(abs(y-prop))
# mean(abs(y-propa))
# #----------------------------------------------------------------------
# #----------------------------------------------------------------------
# #----------------------------------------------------------------------


#ACHAR O LOG(VER) DAS ESTIMATIVAS USANDO OS RATINS ELO COMO PARAMETROS
#USAR ISSO COMO REFERENCIA
#
# colunas referentes ao jogadores que estão presentes na banco do preditor.
    posJoga <- rep(0,length(estimados))

for( i in 1:length(estimados)){
 posJoga[i] <- (which(estimandos==estimados[i]))  
}

# jogadores e números (indices) referentes a coluna na cadeia
        aux <- cbind(estimados,posJoga) 

  mediasLog <- matrix(0,length(W),12)
     varLog <- matrix(0,length(W),12)
  meddist   <- matrix(0,length(W),12)
  vardist   <- matrix(0,length(W),12)









# banco somente com jogadores de rating estimados
####################################################################### BT ######################################################################
#--------------Bradley-Terry simples com W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTcomW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
# Para cada uma das 402 partidas (colunas), existe 12000 estimativas de resultados.
     pi <- matrix(0,dim(gama)[1],length(banco4$W))

for (j in 1:length(banco4$W)){
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # pega as estimativas de rating na coluna do jagador "branca"
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])]
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  Theta <-cbind(RBr,RNe)
  pi[,j] <- BTt.Dif1(Theta)
}
# ao final deste trecho tem todos os pis calculados para as 402 partidas.

# todas as verossilhanças, uma para cada estimativa de pi 
# para 
 logveros   <- matrix(0, length(W),dim(gama)[1])

# todas as distancias, uma para cada estimativa de pi
 distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
           for(j in 1:dim(logveros)[2]){
        
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    distancia[i,j] <- abs(y[i]-pi[j,i]) #abs(y[i]-pi[j,i])
                                        }
           
                              }
# library("geiger")

mediasLog[,1] <- apply(logveros,1,mean)
varLog[,1]    <- apply(logveros,1,var)
meddist[,1]   <- apply(distancia,1,mean)
vardist[,1]   <- apply(distancia,1,var)

rm(distancia,logveros,pi,gama,RBr,RNe,branca,estimandos,estimados,negra,posJoga,Theta,M,foralinhas,i,j,m)

#--------------Bradley-Terry simples sem W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTsemW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco4$W))
for (j in 1:length(banco4$W)){

  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])]
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  Theta <-cbind(RBr,RNe)
  pi[,j] <- BTt.Dif1(Theta)
  
}


# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    distancia[i,j] <- abs(y[i]-pi[j,i])
  }
}

mediasLog[,2] <- apply(logveros,1,mean)
varLog[,2]    <- apply(logveros,1,var)
meddist[,2]   <- apply(distancia,1,mean)
vardist[,2]   <- apply(distancia,1,var)

rm(distancia,logveros,pi,i,j,gama,RBr,RNe,branca,negra,Theta)


#--------------Bradley-Terry  com 1 dela e W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemcomW1delta")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco4$W))

for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])]
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  Theta <-cbind(RBr,RNe,delta)
  pi[,j] <- BTt.Dif2(Theta)
  
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    distancia[i,j] <- abs(y[i]-pi[j,i])
  }
}

mediasLog[,3] <- apply(logveros,1,mean)
varLog[,3]    <- apply(logveros,1,var)
meddist[,3]   <- apply(distancia,1,mean)
vardist[,3]   <- apply(distancia,1,var)

rm(distancia,logveros,pi,gama,RBr,RNe,branca,negra,Theta,i,j,delta)

#--------------Bradley-Terry com 1 dela sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemsemW1delta")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco4$W))

for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])]
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  Theta <-cbind(RBr,RNe,delta)
  pi[,j] <- BTt.Dif2(Theta)
  
}


# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    distancia[i,j] <- abs(y[i]-pi[j,i])
  }
}

mediasLog[,4] <- apply(logveros,1,mean)
varLog[,4]    <- apply(logveros,1,var)
meddist[,4]   <- apply(distancia,1,mean)
vardist[,4]   <- apply(distancia,1,var)

rm(distancia,logveros,pi,gama,RBr,RNe,branca,negra,Theta,i,j,delta)

#--------------Bradley-Terry  com  delta com W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemComW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco4$W))

for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])]
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2])]
  DRw <-delta[,as.integer(aux[which(branca==aux[,1]),2])]
  DRb <-delta[,as.integer(aux[which(negra==aux[,1]),2])]
  Theta <-cbind(RBr,RNe,DRw,DRb)
  pi[,j] <- BT.Dif3(Theta)
  
}


# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    distancia[i,j] <- abs(y[i]-pi[j,i])
  }
}

mediasLog[,5] <- apply(logveros,1,mean)
varLog[,5]    <- apply(logveros,1,var)
meddist[,5]   <- apply(distancia,1,mean)
vardist[,5]   <- apply(distancia,1,var)

rm(logveros,distancia,pi,gama,RBr,RNe,branca,negra,Theta,i,j,delta)

#--------------Bradley-Terry  com  delta sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemsemW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco4$W))

for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])]
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2])]
  DRw <-delta[,as.integer(aux[which(branca==aux[,1]),2])]
  DRb <-delta[,as.integer(aux[which(negra==aux[,1]),2])]
  Theta <-cbind(RBr,RNe,DRw,DRb)
  
  pi[,j] <- BT.Dif3(Theta)
  
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    distancia[i,j] <- abs(y[i]-pi[j,i])
  }
}

mediasLog[,6] <- apply(logveros,1,mean)
varLog[,6]    <- apply(logveros,1,var)
meddist[,6]   <- apply(distancia,1,mean)
vardist[,6]   <- apply(distancia,1,var)

rm(logveros,distancia,pi,gama,RBr,RNe,branca,negra,Theta,i,j,delta,DRb,DRw)

##################################################################### fim BT ####################################################################

#####################################################################  DV ####################################################################

#--------------Davidson simples com W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVcomW")
gama     <- rbind(read.table("cadeia.gamaA.txt"),
                read.table("cadeia.gamaB.txt"),
                read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                  read.table("cadeia.lambdaB.txt"),
                  read.table("cadeia.lambdaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco4$W))
a<-1

for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # posição do jogador na cadeia, vincula cada jogador a uma coluna especificia
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])] 
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  pi[,a]<- DV.Dif1(RBr,RNe,lambda$V1)[,1]
  pi[,a+1] <-DV.Dif1(RBr,RNe,lambda$V1)[,2]
  pi[,a+2] <-DV.Dif1(RBr,RNe,lambda$V1)[,3]
  a<-a+3
  
 }

Y <- matrix(0,length(W),3)
Y[y==0,1]  <- 1 
Y[y==.5,2] <- 1
Y[y==1,3]  <- 1

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

a<-1
for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
  
    logveros[i,j]<-(Y[i,1]*log(pi[j,a])+Y[i,2]*log(pi[j,a+1])+Y[i,3]*log(pi[j,a+2]))
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2)     
     
  }
  a<-a+3
}

mediasLog[,7] <- apply(logveros,1,mean)
varLog[,7]    <- apply(logveros,1,var)
meddist[,7]   <- apply(distancia,1,mean)
vardist[,7]   <- apply(distancia,1,var)

rm(logveros,distancia,pi,gama,RBr,RNe,branca,negra,i,j,lambda,DRb,DRw,a)


  

#--------------Davidson simples sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVsemW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco4$W))
a<-1
for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # posição do jogador na cadeia, vincula cada jogador a uma coluna especificia
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])] 
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  pi[,a]   <- DV.Dif1(RBr,RNe,lambda$V1)[,1]
  pi[,a+1] <-DV.Dif1(RBr,RNe,lambda$V1)[,2]
  pi[,a+2] <-DV.Dif1(RBr,RNe,lambda$V1)[,3]
  a<-a+3
  
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])


a<-1
for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]<-(Y[i,1]*log(pi[j,a])+Y[i,2]*log(pi[j,a+1])+Y[i,3]*log(pi[j,a+2]))
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2)     
    
  }
  a<-a+3
}

mediasLog[,8] <- apply(logveros,1,mean)
varLog[,8]    <- apply(logveros,1,var)
meddist[,8]   <- apply(distancia,1,mean)
vardist[,8]   <- apply(distancia,1,var)


rm(distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)

#--------------Davidson com 1 delta e W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagem1deltaW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                    read.table("cadeia.deltaB.txt"),
                    read.table("cadeia.deltaC.txt"))
# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco4$W))
a<-1
for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # posição do jogador na cadeia, vincula cada jogador a uma coluna especificia
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])] 
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  pi[,a]<- DV.Dif2(RBr,RNe,delta$V1,lambda$V1)[,1]
  pi[,a+1] <-DV.Dif2(RBr,RNe,delta$V1,lambda$V1)[,2]
  pi[,a+2] <-DV.Dif2(RBr,RNe,delta$V1,lambda$V1)[,3]
  a<-a+3
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

a<-1
for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]<-(Y[i,1]*log(pi[j,a])+Y[i,2]*log(pi[j,a+1])+Y[i,3]*log(pi[j,a+2]))
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2 )    
    
  }
  a<-a+3
}

mediasLog[,9] <- apply(logveros,1,mean)
varLog[,9]    <- apply(logveros,1,var)
meddist[,9]   <- apply(distancia,1,mean)
vardist[,9]   <- apply(distancia,1,var)


rm(delta,distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)


#--------------Davidson com 1 delta sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagem1deltasemW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco4$W))
a<-1
for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # posição do jogador na cadeia, vincula cada jogador a uma coluna especificia
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])] 
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  pi[,a]   <- DV.Dif2(RBr,RNe,delta$V1,lambda$V1)[,1]
  pi[,a+1] <- DV.Dif2(RBr,RNe,delta$V1,lambda$V1)[,2]
  pi[,a+2] <- DV.Dif2(RBr,RNe,delta$V1,lambda$V1)[,3]
  a<-a+3
  
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

a<-1
for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]<-(Y[i,1]*log(pi[j,a])+Y[i,2]*log(pi[j,a+1])+Y[i,3]*log(pi[j,a+2]))
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2  )   
    
  }
  a<-a+3
}

mediasLog[,10] <- apply(logveros,1,mean)
varLog[,10]    <- apply(logveros,1,var)
meddist[,10]   <- apply(distancia,1,mean)
vardist[,10]   <- apply(distancia,1,var)


rm(delta,distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)

#--------------Davidson com delta e W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagemComW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco4$W))
a<-1
for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # posição do jogador na cadeia, vincula cada jogador a uma coluna especificia
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])] 
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  DRw <-delta[,as.integer(aux[which(branca==aux[,1]),2])]
  DRb <-delta[,as.integer(aux[which(negra==aux[,1]),2])]
  pi[,a]   <- DV.Dif3(RBr,RNe,DRw,DRb,lambda$V1)[,1]
  pi[,a+1] <- DV.Dif3(RBr,RNe,DRw,DRb,lambda$V1)[,2]
  pi[,a+2] <- DV.Dif3(RBr,RNe,DRw,DRb,lambda$V1)[,3]
  a<-a+3
  
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

a<-1
for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]<-(Y[i,1]*log(pi[j,a])+Y[i,2]*log(pi[j,a+1])+Y[i,3]*log(pi[j,a+2]))
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2 )  
    
  }
  a<-a+3
}

mediasLog[,11] <- apply(logveros,1,mean)
varLog[,11]    <- apply(logveros,1,var)
meddist[,11]   <- apply(distancia,1,mean)
vardist[,11]   <- apply(distancia,1,var)


rm(delta,distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)

#--------------Davidson com delta sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagemSemW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco4$W))
a<-1
for (j in 1:length(banco4$W)){
  
  branca <- banco4$W[j]
  negra  <- banco4$B[j]
  # posição do jogador na cadeia, vincula cada jogador a uma coluna especificia
  RBr <-gama[,as.integer(aux[which(branca==aux[,1]),2])] 
  RNe <-gama[,as.integer(aux[which(negra==aux[,1]),2]) ]
  DRw <-delta[,as.integer(aux[which(branca==aux[,1]),2])]
  DRb <-delta[,as.integer(aux[which(negra==aux[,1]),2])]
  pi[,a]   <- DV.Dif3(RBr,RNe,DRw,DRb,lambda$V1)[,1]
  pi[,a+1] <- DV.Dif3(RBr,RNe,DRw,DRb,lambda$V1)[,2]
  pi[,a+2] <- DV.Dif3(RBr,RNe,DRw,DRb,lambda$V1)[,3]
  a<-a+3
  
}

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

a<-1
for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    
    logveros[i,j]<-(Y[i,1]*log(pi[j,a])+Y[i,2]*log(pi[j,a+1])+Y[i,3]*log(pi[j,a+2]))
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2  )  
    
  }
  a<-a+3
}

mediasLog[,12] <- apply(logveros,1,mean)
varLog[,12]    <- apply(logveros,1,var)
meddist[,12]   <- apply(distancia,1,mean)
vardist[,12]   <- apply(distancia,1,var)


rm(delta,distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo")

save(mediasLog,file="mediasLog.rda")
save(varLog,file="varLog.rda")
save(meddist,file="meddist.rda")
save(vardist,file="vardist.rda")
##################################################################### fim DV ####################################################################








