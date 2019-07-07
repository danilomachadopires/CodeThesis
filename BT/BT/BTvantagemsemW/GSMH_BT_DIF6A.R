#-----------------------------------------------Modelo com um delta para cada jogador sem omega--------------
# Entra com o rating e a dif de cada jogador
# theta = c(gama1,gama2,delta1,delta2)


rm(list=ls(all=T))
source("FGSMH.R")

dados <- read.table("granprix2010a2013.txt",h=T)
attach(dados)

nomes <- levels(W)



## Jogadores de interesse TRabalho antigo (46 jogadores do live rating 15 de maio de 2013)


estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

m  <- length(estimandos)  # número de jogadores analisados
n  <- length(W)           # número de jogos


DRW <- rep(0,n)           # 
DRB <-DRW                 #

#-----------------------------------------------------
#     Distribuições a priori
#     Ajustadas a partir de dados históricos

#    Gama ~ Normal(mu=2705,sigma=400)
mu0    <- 2705
sigma0 <- 400

# delta ~ Normal(mu=0,sigma=10)
mud    <- 50
sigmad <- 40

#-----------------------------------------------------
# Distribuições geradoras de candidatos
# 

## Gama ~ Normal(mu=2705,sigma=400)
mugC    <- rep(2705,m) 
sigmagC <- rep(100,m)

## delta ~ Normal(mu=0,sigma=10)
mudC    <- rep(0,m)
sigmadC <- rep(40,m)


################### Veriricar o banco de dados para partidas onde os participantes não serão analisados
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


DRB <- DRB[-fora]
DRW <-DRW[-fora]

detach(dados,pos=dados)

### chute inicial

gama  <- mugC 
delta <- mudC

cgr <- cbind(c(mugC,mudC),c(sigmagC,sigmadC))
# Substituir no vetor WR e BR os gama
# Substituir no vetor DR os delta  , Aqui é mantido o rating do jogador que não será analisado como fixo, e os ratings
# dos jogadores de interesse como a média do torneio.
for(i in 1:m){
  WR[which(GW[,i]==1)]  <- gama[i] 
  BR[which(GB[,i]==1)]  <- gama[i]
  DRW[which(GW[,i]==1)] <-delta[i]
  DRB[which(GB[,i]==1)] <-delta[i]
}

## Tamanho da cadeia MCCM
B    <- 50000    #burn-in
J    <- 20       #jump
nef  <- 4000     #tamanho amostral efetivo
nsMC <- B+nef*J; #tamanho total da amostra (total da cadeia)
tcgc <- 1000     # tamanho da amostra para atualizar a geradora de candidatos

###########################################################
Mtcgc <- matrix(0,92,tcgc)
cont <- 0

#### Laço de atualização dos parâmetros (Gibbs Sampling)
while (cont <= nsMC) {
  # Atualizar o gama
  for(i in sample(1:m)){
    indice <- which((GW[,i]+GB[,i])!=0)
    # Construir a matriz Theta para o jogador
    Theta  <- NULL
    Theta  <- matrix(0,length(indice),4)
    Theta  <- cbind(WR[indice],
                    BR[indice],
                    DRW[indice],
                    DRB[indice])
    
    pi <- BT.Dif(Theta)
    # Valor corrente da log-Verossimilhança
    Y<-y[indice]
    lVa <- lLpi(pi)
    # Amostra candidato
    cand <- rnorm(1,mugC[i],sigmagC[i])
    # Valor da log-Verossimilhança para o candidato
    WR[which(GW[,i]==1)]  <- cand  
    BR[which(GB[,i]==1)]  <- cand
    Thetac  <- NULL
    Thetac  <- matrix(0,length(indice),4)
    Thetac  <- cbind(WR[indice],
                     BR[indice],
                     DRW[indice],
                     DRB[indice])
    pi  <- BT.Dif(Thetac)
    lVc <- lLpi(pi)
    
    lgC <- log(dnorm(cand   ,mugC[i],sigmagC[i]))
    lgA <- log(dnorm(gama[i],mugC[i],sigmagC[i]))
    
    lpC <- log(dnorm(cand   ,mu0,sigma0))
    lpA <- log(dnorm(gama[i],mu0,sigma0))
    
    gama[i] <- aceita(gama[i],cand,(lVc+lpC+lgA),(lVa+lpA+lgC))
    WR[which(GW[,i]==1)]  <- gama[i]
    BR[which(GB[,i]==1)]  <- gama[i]
  }
  
  # Atualizar o delta
  for(i in sample(1:m)){
    indice <- which((GW[,i]+GB[,i])!=0)
    # Construir a matriz Theta para o jogador
    Theta  <- NULL
    Theta  <- matrix(0,length(indice),4)
    Theta  <- cbind(WR[indice],
                    BR[indice],
                    DRW[indice],
                    DRB[indice])
    pi <- BT.Dif(Theta)
    # Atualizar o gama
    # Valor corrente da log-Verossimilhança
    Y<-y[indice]
    lVa <- lLpi(pi)
    # Amostra candidato
    cand <- rnorm(1,mudC[i],sigmadC[i])
    # Valor da log-Verossimilhança para o candidato
    DRW[which(GW[,i]==1)]  <- cand  
    DRB[which(GB[,i]==1)]  <- cand
    
    Thetac  <- Theta
    Thetac[,c(3,4)] <- cbind(DRW[indice],
                             DRB[indice])
    pi  <- BT.Dif(Thetac)
    lVc <- lLpi(pi)
    
    lgC <- log(dnorm(cand   ,mudC[i],sigmadC[i]))
    lgA <- log(dnorm(delta[i],mudC[i],sigmadC[i]))
    
    lpC <- log(dnorm(cand   ,mud,sigmad))
    lpA <- log(dnorm(delta[i],mud,sigmad))
    
    delta[i] <- aceita(delta[i],cand,(lVc+lpC+lgA),(lVa+lpA+lgC))
    DRW[which(GW[,i]==1)] <- delta[i]
    DRB[which(GB[,i]==1)] <- delta[i]
      }
  
  
  
  # atualizar e imprimir matriz de parâmetros
  if(cont%%J==0 && cont>B)
  {
    write(t(gama),"cadeia.gamaA.txt",ncol=length(gama),append=TRUE)
    write(t(delta),"cadeia.deltaA.txt",ncol=length(delta),append=TRUE)
    Theta  <- cbind(WR,BR,DRW,DRB)
    pi  <- BT.Dif(Theta)
    Y<-y
    lVm <- lLpi(pi)
   
    write(lVm,"cadeia.lVmA.txt",ncol=1,append=TRUE)
  }
    
  # atualizar geradora de candidatos
  Mtcgc[,cont%%tcgc] <- c(gama,delta)
  if(cont%%tcgc==0){
    cgr[,1] <- apply(Mtcgc,1,mean)
    cgr[,2] <- apply(Mtcgc,1,sd)+0.01
    mugC    <- cgr[1:46,1]
    sigmagC <- cgr[1:46,2]
    mudC    <- cgr[47:92,1]
    sigmadC <- cgr[47:92,2]
  }
  cont <- cont + 1
}
