#Método ruim, horrível, quem fizer isso será convidado a se retirar da sala pelo lupércio
covariancias <- function(x,y)
{
  #O as.vector faz virar uma lista (?!?!?!?)
  x <- as.vector(x)[[1]]
  y <- as.vector(y)[[1]]
  xbarra <- mean(x)
  ybarra <- mean(y)
  return(sum((x-xbarra)*(y-ybarra))/(length(x)-1))
}

matriz <- matrix(0,nrow=4,ncol=4)

for(l in 1:4)
{
  for(c in 1:4)
  {
    matriz[l,c] <- covariancias(iris[l],iris[c])
  }
}
matriz
cov(iris[-5])
variancia_total <- sum(diag(matriz))
variancia_generalizada <- det(matriz)

#----------Outro método-----------------

covariancia_matricial <- function(x,y)
{
  x <- as.vector(x)[[1]]
  y <- as.vector(y)[[1]]
  xbarra <- mean(x)
  ybarra <- mean(y)
  xcentrado <- x-xbarra
  ycentrado <- y-ybarra
  return(t(xcentrado)%*%(ycentrado)/(length(x)-1))
}

matriz <- matrix(0,nrow=4,ncol=4)

for(l in 1:4)
{
  for(c in 1:4)
  {
    matriz[l,c] <- covariancia_matricial(iris[l],iris[c])
  }
}

matriz
cov(iris[-5])
variancia_total <- sum(diag(matriz))
variancia_generalizada <- det(matriz)

#----------Outro método-----------------
#Objetivamente melhor!
x <- scale(iris[-5],center=TRUE,scale=FALSE)

matriz <- t(x)%*%x/(ncol(x)-1)
matriz
cov(iris[-5])
variancia_total <- sum(diag(matriz))
variancia_generalizada <- det(matriz)
