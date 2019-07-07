BTt.Dif1 <- function(theta){
  1/(1+10^((-theta[,1]+theta[,2])/400))
}


BTt.Dif2 <- function(theta){
  1/(1+10^((-theta[,1]+theta[,2]-theta[,3])/400))
}

BT.Dif3 <- function(theta){
  1/(1+10^((-theta[,1]+theta[,2]-theta[,3]-theta[,4])/400) )  # não é o bradley!!
}

#lLpi1 <- function(pi){
#  q <- which((pi!=0)*(pi!=1)==1)       # quarda as posições em que pi são diferentes de pi é diferente de 0 e de 1
#  sum(y[q]*log(pi[q])+(1-y[q])*log(1-pi[q]))  
#}



# theta corresponde a gamai,gamaj,deltai,deltaj,lambda
DV.Dif1 <- function(gamai,gamaj,lambda){
  wv   <- exp(gamai)                             # white
  bv   <- exp(gamaj)                             # black
  draw <- exp(lambda+(gamai+gamaj)/2)     # empate 
  pi   <- cbind(bv,draw,wv)/(bv+draw+wv)
  return(pi=pi)   # não é o bradley!!
}

DV.Dif2 <- function(gamai,gamaj,deltai,lambda){
  wv   <- exp(gamai+deltai)                             # white
  bv   <- exp(gamaj)                             # black
  draw <- exp(lambda+(gamai+gamaj+deltai)/2)     # empate 
  pi   <- cbind(bv,draw,wv)/(bv+draw+wv)
  return(pi=pi)   # não é o bradley!!
}


DV.Dif3 <- function(gamai,gamaj,deltai,deltaj,lambda){
  wv   <- exp(gamai+deltai)                             # white
  bv   <- exp(gamaj-deltaj)                             # black
  draw <- exp(lambda+(gamai+gamaj+deltai-deltaj)/2)     # empate 
  pi   <- cbind(bv,draw,wv)/(bv+draw+wv)
  return(pi=pi)   # não é o bradley!!
}

#lLpi4 <- function(pi){
#  lVero <- sum((Y[,1]*log(pi[,1])+
#                        Y[,2]*log(pi[,2])+
#                        Y[,3]*log(pi[,3])))  
#  return(lVero=lVero)
#}
