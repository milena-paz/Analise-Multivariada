library("gifski")
xc <- scale(iris[,c(3,4)])

plot(xc)

cria_rota <- function(rad)
{
  return(matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),nrow=2,ncol=2,byrow=T))
}

xc

x_rotacionado <- t(cria_rota(-pi/4) %*% t(xc))

plot(x_rotacionado,asp=1)

abline(h=0,col="red")

desenha <- function(dados=xc,rotacao=0)
{
  result <- dados%*%t(cria_rota(rotacao))
  plot(result,asp=1,col=iris$Species,pch=19)
  abline(h=c(-2:2),v=c(-3:3),col="#00000088",lty=2)
  if(rotacao +pi/4 == pi/2|| rotacao + pi/2 + pi/4 == pi/2)
    abline(h=0,v=0,col="red")
  else
  {
    abline(a=0,b=tan(c(rotacao+pi/4)),col="red")
    abline(a=0,b=tan(c(rotacao+pi/4+pi/2)),col="red")
  }
}

ease_sine <- function(x)
{
  return(-(cos(pi*x)-1)/2)
}

desenha_apos_rotacao <- function(dados=xc,rotacao=-pi/4,t)
{
  result <- dados%*%t(cria_rota(rotacao))
  lerp <- 1-t
  result[,2] <- result[,2]*lerp
  plot(result,asp=1,col=iris$Species,pch=19)
  abline(h=c(-2:2),v=c(-3:3),col="#00000088",lty=2)
  if(rotacao +pi/4 == pi/2|| rotacao + pi/2 + pi/4 == pi/2)
    abline(h=0,v=0,col="red")
  else
  {
    abline(a=0,b=tan(c(rotacao+pi/4)),col="red")
    abline(a=0,b=tan(c(rotacao+pi/4+pi/2)),col="red")
  }
  for(i in 1:length(dados[,1]))
  {
    lines(c(result[i,1],result[i,1]),c(0,result[i,2]),lty=2,col="blue")
  }
}

desenha()

desenha_apos_rotacao(t=0)

animacao <- function(fps,segundos=c(2,2,2))
{
  for(i in (1:(fps*segundos[1]))/(fps*segundos[1]))
  {
    desenha(rotacao = -ease_sine(i)*pi/4)
  }
  for(k in (1:(fps*segundos[2]))/(fps*segundos[2]))
  {
    desenha_apos_rotacao(t=ease_sine(k))
  }
  for(j in (1:(fps*segundos[3]))/(fps*segundos[3]))
  {
    hist((xc%*%t(cria_rota(-pi/4)))[,1],freq=F,ylim=c(0,1))
  }
}

#Usando a matriz mde covariancias

desenha()
desenha_apos_rotacao(t=0)

S <- cov(xc)
W <- eigen(S)$vectors

desenha2 <- function(dados=xc)
{
  result <- dados
  plot(result,asp=1,col=iris$Species,pch=19)
  arrows(c(0,0),c(0,0),c(W[1,1],W[2,1]),c(W[1,2],W[2,2]))
  abline(h=c(-2:2),v=c(-3:3),col="#00000088",lty=2)
}

desenha_apos_rotacao2 <- function(dados=xc,rotacao=-pi/4,t)
{
  result <- dados%*%W
  lerp <- 1-t
  result[,2] <- result[,2]*lerp
  plot(result,asp=1,col=iris$Species,pch=19)
  abline(h=c(-2:2),v=c(-3:3),col="#00000088",lty=2)
  for(i in 1:length(dados[,1]))
  {
    lines(c(result[i,1],result[i,1]),c(0,result[i,2]),lty=2,col="blue")
  }
}

desenha2()

desenha_apos_rotacao2(t=0)

var(xc%*%W)
eigen(W)

eigen(S)
