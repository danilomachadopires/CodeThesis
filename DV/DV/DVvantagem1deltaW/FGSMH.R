# Funcao Davidian modificada, como parametro de vantagem para as brancas 
#  e prob de empate geral

# theta corresponde a gamai,gamaj,deltai,deltaj,lambda
DV.Dif <- function(gamai,gamaj,deltai,lambda){
  wv   <- exp(gamai+deltai)                             # white
  bv   <- exp(gamaj)                             # black
  draw <- exp(lambda+(gamai+gamaj+deltai)/2)     # empate 
  pi   <- cbind(bv,draw,wv)/(bv+draw+wv)
  return(pi=pi)   # não é o bradley!!
}



## funcao que calcula a Log Verossimilhança
## dados os valores do vetor theta
#  Para cada theta são gerados n (pi)s pois cada pi é calculado para o rating de brancas
#  e o rating de pretas (O theta) no jogo em questão, apos é feita a operação de 
# somátorio levando em consideração esses (pi)s e os resultados
# assim é gerado a verossimilhança para o dado theta.

# Y é uma matriz indicadora de colunas para cada resultado (bv,draw,wv) 



lLpi3 <- function(pi){
 
    lVero <- sum(omega*(Y[,1]*log(pi[,1])+
                      Y[,2]*log(pi[,2])+
                      Y[,3]*log(pi[,3])))  
  return(lVero=lVero)
}


# Probabilidade de aceitação no algoritmo MH
# função que devolve se
# o ponto amostral candidato foi aceito

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










 