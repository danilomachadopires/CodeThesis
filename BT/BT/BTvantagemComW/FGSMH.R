# Funcao Bradley-Terry modificada, como parametro de vantagem para as brancas 
#  e para as pretas

BT.Dif <- function(theta){
  1/(1+10^((-theta[,1]+theta[,2]-theta[,3]-theta[,4])/400) )  # não é o bradley!!
}




## funcao que calcula a Log Verossimilhança
## dados os valores do vetor theta
#  Para cada theta são gerados n (pi)s pois cada pi é calculado para o rating de brancas
#  e o rating de pretas (O theta) no jogo em questão, apos é feita a operação de 
# somátorio levando em consideração esses (pi)s e os resultados
# assim é gerado a verossimilhança para o dado theta.


lLpi <- function(pi){
  q <- which((pi!=0)*(pi!=1)==1)
  sum(Y[q]*omega[q]*log(pi[q])+(1-Y[q])*omega[q]*log(1-pi[q]))  
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










 