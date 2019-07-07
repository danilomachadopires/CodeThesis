#-----------------------------------------------Modelo com um delta para cada jogador--------------
# Entra com o rating e a dif de cada jogador
# theta = c(gama1,gama2,delta1,delta2)

rm(list=ls(all=T))
source("/home/danilomp/Qualifica/DV/DVvantagemComW/FGSMH.R")

dados <- read.table("/home/danilomp/Qualifica/DV/DVvantagemComW/granprix2010a2013.txt",h=T)
attach(dados)

WR <- WR*log(10)/400
BR <- BR*log(10)/400
nomes <- levels(W)

## ------------------------tempo---------------------
tmin <- min(Data)
tmax <- max(Data)
omega <- 1/sqrt((1+Data-tmin)/(1+tmax-tmin))



## Jogadores de interesse TRabalho antigo (46 jogadores do live rating 15 de maio de 2013)


estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

m  <- length(estimandos)  # número de jogadores analisados
n  <- length(W)           # número de jogos


### Chute inicial para DRW e DRB
DRW <- rep(0.1,n)         # A vantagem das brancas foi calculada externamente
DRB <- DRW                #

#-----------------------------------------------------
#     Distribuições a priori
#     Ajustadas a partir de dados históricos

#    Gama ~ Normal(mu=2705,sigma=400)
mu0    <- 2705*log(10)/400
sigma0 <- 400

# delta ~ Normal(mu=0,sigma=10)
mud    <- 1
sigmad <- 10

# lambda ~ Beta(mu=1,sd=5)
mul <- 1
sdl <- 5

#-----------------------------------------------------
# Distribuições geradoras de candidatos
# Lendo inicialmente a saida original
load("/home/danilomp/Qualifica/DV/DVvantagemComW/cgr.rda")

## Gama ~ Normal(mugC,sigmagC)
mugC    <- cgr[1:46,1]
sigmagC <- cgr[1:46,2]

## delta ~ Normal(mugC,sigmagC)
mudC    <- cgr[47:92,1]
sigmadC <- cgr[47:92,2]

# lambda ~ Normal(mulgC,sdlgC)
mulgC <- cgr[93,1]
sdlgC <- cgr[93,1]

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

DRB <- DRB[-fora]
DRW <-DRW[-fora]
omegag <- omega[-fora]
detach(dados,pos=dados)

Yg <- matrix(0,n,3)
Yg[y==0,1]  <- 1 
Yg[y==.5,2] <- 1
Yg[y==1,3]  <- 1

### chute inicial

gama   <- mugC 
delta  <- mudC
lambda <- mulgC

# Substituir no vetor WR e BR os gama
# Substituir no vetor DR os delta  , Aqui é mantido o rating do jogador que não será analisado como fixo, e os ratings
# dos jogadores de interesse como a média do torneio.
for(i in 1:m){
  WR[which(GW[,i]==1)]  <- gama[i] 
  BR[which(GB[,i]==1)]  <- gama[i]
  DRW[which(GW[,i]==1)] <- delta[i]
  DRB[which(GB[,i]==1)] <- delta[i]
}

## Tamanho da cadeia MCCM
B    <- 50000     #burn-in
J    <- 10       #jump
nef  <- 4000     #tamanho amostral efetivo
nsMC <- B+nef*J  #tamanho total da amostra (total da cadeia)
tcgc <- 100      # tamanho da amostra para atualizar a geradora de candidatos
###########################################################
Mtcgc <- matrix(0,93,tcgc)
cont <- 0

#### Laço de atualização dos parâmetros (Gibbs Sampling)
while (cont <= nsMC) {
  # Atualizar o gama
  ordem <- sample(1:m)
  for(i in ordem){
    indice <- which((GW[,i]+GB[,i])!=0)
    # Construir a matriz Theta para o jogador
    # Construir a matriz Theta para o jogador
    gamai   <- NULL
    gamaj   <- NULL
    deltai  <- NULL
    deltaj  <- NULL
    gamai   <- WR[indice]
    gamaj   <- BR[indice]
    deltai  <- DRW[indice]
    deltaj  <- DRB[indice]
    omega   <- omegag[indice]
    pi <- DV.Dif(gamai,gamaj,deltai,deltaj,lambda)
    
    # Valor corrente da log-Verossimilhança
    Y <- Yg[indice,]
    lVa <- lLpi3(pi)
    # Amostra candidato
    cand <- rnorm(1,mugC[i],sigmagC[i])
    # Valor da log-Verossimilhança para o candidato
    WR[which(GW[,i]==1)]  <- cand  
    BR[which(GB[,i]==1)]  <- cand
    gamaic   <- NULL
    gamajc   <- NULL
    gamaic   <- WR[indice]
    gamajc   <- BR[indice]
    pi <- DV.Dif(gamaic,gamajc,deltai,deltaj,lambda)
    # Valor da log-Verossimilhança do candidato
    lVc <- lLpi3(pi)
    
    lgC <- log(dnorm(cand   ,mugC[i],sigmagC[i]))
    lgA <- log(dnorm(gama[i],mugC[i],sigmagC[i]))
    
    lpC <- log(dnorm(cand   ,mu0,sigma0))
    lpA <- log(dnorm(gama[i],mu0,sigma0))
    
    gama[i] <- aceita(gama[i],cand,(lVc+lpC+lgA),(lVa+lpA+lgC))
    WR[which(GW[,i]==1)]  <- gama[i]
    BR[which(GB[,i]==1)]  <- gama[i]
  }
  
  # Atualizar o delta
  ordem <- sample(1:m)
  for(i in ordem){
    indice <- which((GW[,i]+GB[,i])!=0)
    # Construir a matriz Theta para o jogador
    gamai   <- NULL
    gamaj   <- NULL
    deltai  <- NULL
    deltaj  <- NULL
    gamai   <- WR[indice]
    gamaj   <- BR[indice]
    deltai  <- DRW[indice]
    deltaj  <- DRB[indice]
    omega   <- omegag[indice]
    pi <- DV.Dif(gamai,gamaj,deltai,deltaj,lambda)
    # Atualizar o gama
    # Valor corrente da log-Verossimilhança
    Y <- Yg[indice,]
    lVa <- lLpi3(pi)
    # Amostra candidato
    cand <- rnorm(1,mudC[i],sigmadC[i])
    # Valor da log-Verossimilhança para o candidato
    DRW[which(GW[,i]==1)]  <- cand  
    DRB[which(GB[,i]==1)]  <- cand
   
    deltaic   <- NULL
    deltajc   <- NULL
    deltaic   <- DRW[indice]
    deltajc   <- DRB[indice]
    pi <- DV.Dif(gamai,gamaj,deltaic,deltajc,lambda)
    lVc <- lLpi3(pi)
    
    lgC <- log(dnorm(cand   ,mudC[i],sigmadC[i]))
    lgA <- log(dnorm(delta[i],mudC[i],sigmadC[i]))
    
    lpC <- log(dnorm(cand   ,mud,sigmad))
    lpA <- log(dnorm(delta[i],mud,sigmad))
    
    delta[i] <- aceita(delta[i],cand,(lVc+lpC+lgA),(lVa+lpA+lgC))
    DRW[which(GW[,i]==1)] <- delta[i]
    DRB[which(GB[,i]==1)] <- delta[i]
  }
  
  # Atualizar o lambda
  pi  <- DV.Dif(WR,BR,DRW,DRB,lambda)
  omega <- omegag
  Y   <- Yg
  lVa <- lLpi3(pi)
  # Amostra candidato
  cand <- rnorm(1,mulgC,sdlgC)
  
  pi  <- DV.Dif(WR,BR,DRW,DRB,cand)
  lVc <- lLpi3(pi)
  
  lgC <- log(dnorm(cand  ,mulgC,sdlgC))
  lgA <- log(dnorm(lambda,mulgC,sdlgC))

  lpC <- log(dnorm(cand  ,mul,sdl))
  lpA <- log(dnorm(lambda,mul,sdl))
  
  lambda <- aceita(lambda,cand,(lVc+lpC+lgA),(lVa+lpA+lgC))
  
  
  # atualizar e imprimir matriz de parâmetros
  if(cont%%J==0 && cont>B){
    write(t(gama),"/home/danilomp/Qualifica/DV/DVvantagemComW/cadeia.gamaA.txt",ncol=length(gama),append=TRUE)
    write(t(delta),"/home/danilomp/Qualifica/DV/DVvantagemComW/cadeia.deltaA.txt",ncol=length(delta),append=TRUE)
    write(lambda,"/home/danilomp/Qualifica/DV/DVvantagemComW/cadeia.lambdaA.txt",ncol=1,append=TRUE)
    
    # Valor corrente da logVerossimilhança
    # para o cálculo do fator de Bayes
    pi  <- DV.Dif(WR,BR,DRW,DRB,lambda)
    Y   <- Yg
    pi  <- DV.Dif(WR,BR,DRW,DRB,lambda)
    omega <- omegag
    lVm <- lLpi3(pi)
    write(lVm,"/home/danilomp/Qualifica/DV/DVvantagemComW/cadeia.lVmA.txt",ncol=1,append=TRUE) 
    
  }
  
  # atualizar geradora de candidatos
  Mtcgc[,cont%%tcgc] <- c(gama,delta,lambda)
  if(cont%%tcgc==0){
    cgr[,1] <- apply(Mtcgc,1,mean)
    cgr[,2] <- apply(Mtcgc,1,sd)+0.01
    mugC    <- cgr[1:46,1]
    sigmagC <- cgr[1:46,2]
    mudC    <- cgr[47:92,1]
    sigmadC <- cgr[47:92,2]
    mulgC <- cgr[93,1]
    sdlgC <- cgr[93,1]
  }
  cont <- cont + 1
}
