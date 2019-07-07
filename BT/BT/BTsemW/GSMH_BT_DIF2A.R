#     Bradley-Terry model with time-weighted likelihood function


rm(list=ls(all=T))
source("FGSMH.R")

dados <- read.table("granprix2010a2013.txt",h=T)
attach(dados)

nomes <- levels(W)


# Players in the ***Training set (46 out of the 100 world's best that would play Grand Prix series 2013).


estimandos <- nomes[c(7,19,20,32,35,47,51,63,143,145,
                      204,208,232,284,292,325,401,407,433,437,
                      472,494,531,536,570,576,627,633,644,646,
                      653,658,738,757,773,843,877,908,929,931,
                      942,948,959,972,973,979)]

m  <- length(estimandos)  # number of players
n  <- length(W)            # number of games


# Average and variance (hyper-parameters) for the Normal prior distributions 
# for ratings
mu0    <- 2705
sigma0 <- 400

## Gama ~ Norm(mu=2705,sigma=400)  / Candidate
mugC    <- rep(2705,m)
sigmagC <- rep(400,m)


#The following code was used to select the games in the training set.
#Where (`m`) is the number the select players  and `n`is the number games in origin data base.

# Matrix to store results: White
GW <- matrix(0,n,m) 
# Matrix to store results: black
GB <- matrix(0,n,m)


for(i in 1:m){
  GW[,i] <- as.integer(W==estimandos[i])
  GB[,i] <- as.integer(B==estimandos[i])
}

M <- GW+GB
#Positions in the database for games in which chosen players do not participate. This will be used to identify the correct games to analyse or not.

fora <- which(apply(M,1,sum)==0)
n-length(fora)

GW <- GW[-fora,]
GB <- GB[-fora,]
y <- y[-fora]

WR <- WR[-fora]
BR <- BR[-fora]

W <- W[-fora]
n <- length(y)


detach(dados,pos=dados)
rm(dados)

#  Candidate for rating (m, is the players number)
gama  <- mugC  

cgr <-cbind(gama,sigmagC)

for(i in 1:m){
  WR[which(GW[,i]==1)]  <- gama[i] 
  BR[which(GB[,i]==1)]  <- gama[i]
}

# MCMC - Parameters
B    <- 50000     #burn-in
J    <- 20        #jump
nef  <- 4000      # effective sample size
nsMC <- B+nef*J;  # total size
tcgc  <- 1000     # sample size to reacess 
                  # generating distribution hyperparameters



Mtcgc <- matrix(0,(m),tcgc) # samples used to reacess
                            # generating distribution

cont <- 0

# Gibbs Sampling Loop
# for actual use of the algorithm use 
# replace 2 by nsMC 
# (uncoment next line and coment the other)


while (cont <= nsMC) {
  
  # Updating ratings
  for(i in sample(1:m)){
    indice <- which((GW[,i]+GB[,i])!=0)
    
    Theta  <- NULL
    Theta  <- matrix(0,length(indice),2)
    Theta  <- cbind(WR[indice],
                    BR[indice])
    # Probabilities for each result with new candidate value                
    pi <- BTt.Dif(Theta)
    
    Y<-y[indice]
   
    lVa <- lLpi(pi)
    # candidate  sample
    cand <- rnorm(1,mugC[i],sigmagC[i])
    WR[which(GW[,i]==1)]  <- cand  
    BR[which(GB[,i]==1)]  <- cand
    Thetac  <- NULL
    Thetac  <- matrix(0,length(indice),2)
    Thetac  <- cbind(WR[indice],
                     BR[indice])
    pi  <- BTt.Dif(Thetac)
    lVc <- lLpi(pi)
    
    lgC <- log(dnorm(cand   ,mugC[i],sigmagC[i]))
    lgA <- log(dnorm(gama[i],mugC[i],sigmagC[i]))
    
    lpC <- log(dnorm(cand   ,mu0,sigma0))
    lpA <- log(dnorm(gama[i],mu0,sigma0))
    
    gama[i] <- aceita(gama[i],cand,(lVc+lpC+lgA),(lVa+lpA+lgC))
    WR[which(GW[,i]==1)]  <- gama[i]
    BR[which(GB[,i]==1)]  <- gama[i]
    
  }
  

  # Printing parameter values and logLikelihood
  if(cont%%J==0 && cont>B)
  {
    write(t(gama),"cadeia.gamaA.txt",ncol=length(gama),append=TRUE)
    Theta  <- cbind(WR,BR)
    # evaluate and print logLikelihood
    pi  <- BTt.Dif(Theta)
    Y<-y
    
    lVm <- lLpi(pi)
    
    write(lVm,"cadeia.lVmA.txt",ncol=1,append=TRUE)  
    
  }
  # updating candidate generating densities
  Mtcgc[,cont%%tcgc] <- c(gama)
  if(cont%%tcgc==0){
    cgr[,1] <- apply(Mtcgc,1,mean)
    cgr[,2] <- apply(Mtcgc,1,sd)+0.01
    mugC    <- cgr[1:46,1]
    sigmagC <- cgr[1:46,2]
    
  }
  cont <- cont + 1
}
#This is the algorithm is used three times, then are obtained 3 chains for each parameter.

# cadeia.gamaA , cadeia.gamaB and cadeia.gamaC for each 46 Ratings (gammas)
# cadeia.lVmA, cadeia.lVmB and cadeia.lVmC for loglikelihood

