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
