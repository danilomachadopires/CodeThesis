#----# The Bradley-Terry model.


BTt.Dif <- function(theta){
  1/(1+10^((-theta[,1]+theta[,2])/400))
}

#---# Logarithm of the likelihood function for the Bradley-Terry model
lLpi <- function(pi){
  q <- which((pi!=0)*(pi!=1)==1)       # quarda as posições em que pi são diferentes de pi é diferente de 0 e de 1
  sum(Y[q]*log(pi[q])+(1-Y[q])*log(1-pi[q]))  
 

}


#---Acceptance function used in the MH algorithm.
aceita <- function(atual,candidato,lnum,lden){
  if(lnum > lden){
    atual <- candidato
  }else{ 
    if(runif(1)<exp(lnum - lden)){
      atual <- candidato
    }
  }
  return(atual)
}





 