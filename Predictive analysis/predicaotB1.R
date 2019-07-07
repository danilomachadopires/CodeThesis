rm(list=ls(all=T))
#DELTA DOBRO

source("funcoes.R")

# janeiro de 2010 a novembro de 2012

    banco  <- read.table("granprix2010a2013.txt",h=T)  
    nomes  <- levels(banco$W)

# jogadores que estimei o rating nas estapas iniciais
    estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

       m   <- length(estimandos)

       #-----------Banco utilizado para predi��o janeiro a agosto de 2013  a agosto de 2013


            banco2 <- read.table("BancoDados2013.txt",h=T) 
                     attach(banco2)

            nomes2 <- (unique(c(as.character(W),as.character(B))))
                 n <- length(W)
                 
#------------- Selecionando partidas onde pelo menos um dos jogadores foi analisado
#-----------------Testing set B        GW <- matrix(0,n,m) 

                 GW <- matrix(0,n,m) 
                 GB <- matrix(0,n,m)

for(i in 1:m){
    GW[,i] <- as.integer(W==estimandos[i])
    GB[,i] <- as.integer(B==estimandos[i])
}

         M <- GW+GB

# Linhas em que nem um dos dois jogadores teve o rating estimado pelos modelos

         foralinhas <- c(which(apply(M,1,sum)==0))
         detach(banco2,pos=banco2)

# novo banco somente com partidas de pelo menos um jogador de ratings estimados
     banco3 <- (banco2[-foralinhas,])

rm(banco,banco2,n,nomes,GB,GW,i,nomes2)

attach(banco3)

   estimados <- sort(unique(c(as.character(banco3$W),as.character(banco3$B))))


   mediasLog <- matrix(0,length(W),12)
     varLog <- matrix(0,length(W),12)
  meddist   <- matrix(0,length(W),12)
  vardist   <- matrix(0,length(W),12)

save(banco3,file="banco3.rda")
# banco com pelo menos 1 jogador de rating estimado.

####################################################################### BT ######################################################################
#--------------Bradley-Terry simples com W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTcomW")

#  12000 valores de rating para cada 1 dos 46 jogadores
gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))


names(gama)<-estimandos

k<-mean(c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")]))/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

mean(gama[,"CarlsenMagnus"])

# se tivessemos  uma unica estimativa para cada jogador, teriamos 732 partidas
# Como temos 12000 estimativas para cada jogador, temos 12000X732 resultados esperados.

pi <- matrix(0,dim(gama)[1],length(banco3$W))

for (j in 1:length(banco3$W)){
    branca <- as.character(banco3$W[j])
    negra  <- as.character(banco3$B[j])
    
    if(sum(branca==names(gama))==0) {
      RBr <-rep(banco3$WR[j],dim(pi)[1])
      RNe <-gama[,negra]
    
    }else{
            if(sum(negra==names(gama))==0){
              RBr <-gama[,branca]
              RNe <-rep(banco3$BR[j],dim(pi)[1])
              }else{
            # pega as estimativas de rating na coluna do jogador "branca"
                 RBr <-gama[,branca]
                 RNe <-gama[,negra ]
             }
    }            
    Theta <-cbind(RBr,RNe)
    pi[,j] <- BTt.Dif1(Theta)
 }

# ao final deste trecho tem todos os pis calculados para as 732 partidas.

# todas as verossilhanças, uma para cada estimativa de pi 

 logveros   <- matrix(0, length(W),dim(gama)[1])

# todas as distancias, uma para cada estimativa de pi

distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
           for(j in 1:dim(logveros)[2]){
        
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    # distancia[i,j] <- (y[i]-pi[j,i])^2
    distancia[i,j] <- abs(y[i]-pi[j,i])
                                        }
                              }


# meddist   <- matrix(0,length(W),12)
# vardist   <- matrix(0,length(W),12)




mediasLog[,1] <- apply(logveros,1,mean)
varLog[,1]    <- apply(logveros,1,var)
meddist[,1]   <- apply(distancia,1,mean)
vardist[,1]   <- apply(distancia,1,var)

rm(distancia,logveros,pi,gama,RBr,RNe,branca,negra,Theta,M,foralinhas,i,j,m)

#--------------Bradley-Terry simples sem W (tempo)---------------------------------

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTsemW")
#setwd("/home/danilo/Dropbox/3marco2014/BT/BTsemW")
gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))

names(gama)<-estimandos

k<-mean(c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")]))/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

mean(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
# Para cada uma das 731 partidas (colunas), existe 12000 estimativas de resultados.
pi <- matrix(0,dim(gama)[1],length(banco3$W))

for (j in 1:length(banco3$W)){
  
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j],dim(pi)[1])
    RNe <-gama[,negra]
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j],dim(pi)[1])
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
       
        }
  }            
  Theta <-cbind(RBr,RNe)
  pi[,j] <- BTt.Dif1(Theta)
}

# ao final deste trecho tem todos os pis calculados para as 732 partidas.

# todas as verossilhanças, uma para cada estimativa de pi 

# todas as verossilhanças, uma para cada estimativa de pi 
logveros   <- matrix(0, length(W),dim(gama)[1])
# todas as distancias, uma para cada estimativa de pi
distancia  <- matrix(0, length(W),dim(gama)[1])

for(i in 1:dim(logveros)[1]){
  for(j in 1:dim(logveros)[2]){
    logveros[i,j]  <- (y[i]*log(pi[j,i])+(1-y[i])*log(1-pi[j,i]))
    # distancia[i,j] <- (y[i]-pi[j,i])^2
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
#setwd("/home/danilo/Dropbox/3marco2014/BT/BTvantagemcomW1delta")
gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))
names(gama)<-estimandos

k<-mean(c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")]))/mean(gama[,"CarlsenMagnus"])
gama<-k*gama


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco3$W))


for (j in 1:length(banco3$W)){
  
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j],dim(pi)[1])
    RNe <-gama[,negra]
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j],dim(pi)[1])
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
       }
  }            
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
    # distancia[i,j] <- (y[i]-pi[j,i])^2
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
#setwd("/home/danilo/Dropbox/3marco2014/BT/BTvantagemsemW1delta")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))
names(gama)<-estimandos

k<-mean(c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")]))/mean(gama[,"CarlsenMagnus"])
gama<-k*gama


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco3$W))


for (j in 1:length(banco3$W)){
  
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j],dim(pi)[1])
    RNe <-gama[,negra]
    
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j],dim(pi)[1])
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        }
  }            
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
    # distancia[i,j] <- (y[i]-pi[j,i])^2
    distancia[i,j] <- abs(y[i]-pi[j,i])
  }
}

mediasLog[,4] <- apply(logveros,1,mean)
varLog[,4]    <- apply(logveros,1,var)
meddist[,4]   <- apply(distancia,1,mean)
vardist[,4]   <- apply(distancia,1,var)

rm(distancia,logveros,pi,gama,RBr,RNe,branca,negra,Theta,i,j,delta)

#--------------Bradley-Terry  com  delta com W (tempo)---------------------------------
#######################################################duvida

setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/BT/BTvantagemComW")
#setwd("/home/danilo/Dropbox/3marco2014/BT/BTvantagemComW")
gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))

names(gama)<-estimandos
names(delta)<-estimandos

k<-mean(c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")]))/mean(gama[,"CarlsenMagnus"])
gama<-k*gama


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco3$W))


for (j in 1:length(banco3$W)){
  
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j],dim(pi)[1])
    RNe <-gama[,negra]
    DRb <-delta[,negra]
    DRw<-rep(0,dim(pi)[1])
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j],dim(pi)[1])
      DRw<-delta[,branca]
      DRb <-rep(0,dim(pi)[1])
     
      }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        DRw<-delta[,branca]
        DRb <-delta[,negra]
      }
  }            
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
    # distancia[i,j] <- (y[i]-pi[j,i])^2
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
names(gama)<-estimandos
names(delta)<-estimandos

k<-mean(c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")]))/mean(gama[,"CarlsenMagnus"])
gama<-k*gama


# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],length(banco3$W))


for (j in 1:length(banco3$W)){
  
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j],dim(pi)[1])
    RNe <-gama[,negra]
    DRb <-delta[,negra]
    DRw<-rep(0,dim(pi)[1])
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j],dim(pi)[1])
      DRw<-delta[,branca]
      DRb <-rep(0,dim(pi)[1])
      
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        DRw<-delta[,branca]
        DRb <-delta[,negra]
      }
  }            
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
    # distancia[i,j] <- (y[i]-pi[j,i])^2
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

names(gama)<-estimandos

k<-mean((c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")])*log(10))/400)/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

min(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco3$W))
a<-1

for (j in 1:length(banco3$W)){
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j]*log(10)/400,dim(pi)[1])
    RNe <-gama[,negra]
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j]*log(10)/400,dim(pi)[1])
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        }
  }  
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
     distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2 )    
    }
  a<-a+3
}

mediasLog[,7] <- apply(logveros,1,mean)
varLog[,7]    <- apply(logveros,1,var)
meddist[,7]   <- apply(distancia,1,mean)
vardist[,7]   <- apply(distancia,1,var)

rm(logveros,distancia,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)

#--------------Davidson simples sem W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVsemW")

gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))
names(gama)<-estimandos

k<-mean((c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")])*log(10))/400)/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

min(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco3$W))
a<-1

for (j in 1:length(banco3$W)){
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j]*log(10)/400,dim(pi)[1])
    RNe <-gama[,negra]
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j]*log(10)/400,dim(pi)[1])
      }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
       }
  }  
  pi[,a]<- DV.Dif1(RBr,RNe,lambda$V1)[,1]
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
    distancia[i,j] <-sqrt((Y[i,1]-pi[j,a])^2 +(Y[i,2]-pi[j,a+1])^2  +(Y[i,3]-pi[j,a+2])^2 )    
    
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
names(gama)<-estimandos


k<-mean((c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")])*log(10))/400)/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

min(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco3$W))
a<-1

for (j in 1:length(banco3$W)){
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j]*log(10)/400,dim(pi)[1])
    RNe <-gama[,negra]
    
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j]*log(10)/400,dim(pi)[1])
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
       }
  }  
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
names(gama)<-estimandos

k<-mean((c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")])*log(10))/400)/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

min(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco3$W))
a<-1

for (j in 1:length(banco3$W)){
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j]*log(10)/400,dim(pi)[1])
    RNe <-gama[,negra]
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j]*log(10)/400,dim(pi)[1])
      }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        }
  }  
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

mediasLog[,10] <- apply(logveros,1,mean)
varLog[,10]    <- apply(logveros,1,var)
meddist[,10]   <- apply(distancia,1,mean)
vardist[,10]   <- apply(distancia,1,var)


rm(delta,distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)

#--------------Davidson com delta e W (tempo)---------------------------------
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo/DV/DVvantagemComW")
#setwd("/home/danilo/Dropbox/3marco2014/DV/DVvantagemComW")
gama     <- rbind(read.table("cadeia.gamaA.txt"),
                  read.table("cadeia.gamaB.txt"),
                  read.table("cadeia.gamaC.txt"))
lambda     <- rbind(read.table("cadeia.lambdaA.txt"),
                    read.table("cadeia.lambdaB.txt"),
                    read.table("cadeia.lambdaC.txt"))
delta     <- rbind(read.table("cadeia.deltaA.txt"),
                   read.table("cadeia.deltaB.txt"),
                   read.table("cadeia.deltaC.txt"))

names(gama)<-estimandos
names(delta)<-estimandos

k<-mean((c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")])*log(10))/400)/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

min(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco3$W))
a<-1

for (j in 1:length(banco3$W)){
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j]*log(10)/400,dim(pi)[1])
    RNe <-gama[,negra]
    DRb <-delta[,negra]
    DRw <- rep(0,dim(pi)[1])
    
    
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j]*log(10)/400,dim(pi)[1])
      DRw <-delta[,branca]
      DRb <-rep(0,dim(pi)[1])
     }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        DRw <-delta[,branca]
        DRb <-delta[,negra]
       }
  }  
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


names(gama)<-estimandos
names(delta)<-estimandos

k<-mean((c(WR[which(W=="CarlsenMagnus")],BR[which(B=="CarlsenMagnus")])*log(10))/400)/mean(gama[,"CarlsenMagnus"])
gama<-gama*k

min(gama[,"CarlsenMagnus"])

# percorre todo o banco e identifica os ratings na coluna da cadeia 
pi <-matrix(0,dim(gama)[1],3*length(banco3$W))
a<-1

for (j in 1:length(banco3$W)){
  branca <- as.character(banco3$W[j])
  negra  <- as.character(banco3$B[j])
  
  if(sum(branca==names(gama))==0) {
    RBr <-rep(banco3$WR[j]*log(10)/400,dim(pi)[1])
    RNe <-gama[,negra]
    DRb <-delta[,negra]
    DRw <- rep(0,dim(pi)[1])
    
   
  }else{
    if(sum(negra==names(gama))==0){
      RBr <-gama[,branca]
      RNe <-rep(banco3$BR[j]*log(10)/400,dim(pi)[1])
      DRw <-delta[,branca]
      DRb <-rep(0,dim(pi)[1])
      }else{
        # pega as estimativas de rating na coluna do jogador "branca"
        RBr <-gama[,branca]
        RNe <-gama[,negra ]
        DRw <-delta[,branca]
        DRb <-delta[,negra]
       }
  }  
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

mediasLog[,12] <- apply(logveros,1,mean)
varLog[,12]    <- apply(logveros,1,var)
meddist[,12]   <- apply(distancia,1,mean)
vardist[,12]   <- apply(distancia,1,var)


rm(delta,distancia,logveros,pi,gama,RBr,RNe,branca,negra,i,j,lambda,a)
setwd("C:/Users/Danilomp/Desktop/arquivos Artigo/3marco2014refazendo")

save(mediasLog,file="mediasLogb1.rda")
save(varLog,file="varLogb1.rda")
save(meddist,file="meddistb1.rda")
save(vardist,file="vardistb1.rda")
##################################################################### fim DV ####################################################################








