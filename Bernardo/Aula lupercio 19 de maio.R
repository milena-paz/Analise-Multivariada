medidas <- read.table("T4-3.DAT",header=F)
dim(medidas)
head(medidas)
str(medidas)
names(medidas) <- c("X1","X2","X3","X4","dj2")

str(cjto)
tam <- dim(medidas)[1]
variaveis <- dim(medidas)[2]
medidas <- medidas[,1:4]
library(nortest)

par(mfrow=c(2,2))
for(i in 1:4)
{
  qqnorm(medidas[,i])
  qqline(medidas[,i])
}

shapiro <- apply(medidas,2,shapiro.test)

anderson <- apply(medidas,2,ad.test)

lillie <- apply(medidas,2,lillie.test)

medias <- apply(medidas,2,mean)
S <- cov(medidas)
medidas_c <- scale(medidas,T,F)
dj2 <- apply(medidas_c,1, function(x) t(x)%*%solve(S)%*%(x))

parametric.draw <- function(range,funct,precision=100,...)
{
  #faz "precision" linhas para aproximar a curva
  points <- seq(from=range[1],to=range[2],length.out=precision)
  coords <- list(x = funct$x(points), y = funct$y(points))
  lines(coords$x,coords$y,...)
}

rotacao <- angulo(c(1,0),autov$vectors[1,])

angulo <- function(a,b) acos(sum(a*b)/(sqrt(sum(a*a))*sqrt(sum(b*b))))

elipse <- function(a,b,rot=0,x0=0,y0=0) list(x=function(x) a*cos(x)*cos(rot)+b*sin(x)*sin(rot) + x0,y= function(x) a*cos(x)*sin(rot) - b*sin(x)*cos(rot)+y0)
#o parametro a b da elipse, autovalores deve ser os autovalores da matriz de covariancia
elipse.a <- function(a,autovalores) sqrt(qchisq(a,2)*(autovalores[1]))
elipse.b <- function(a,autovalores) sqrt(qchisq(a,2)*(autovalores[2]))

desenha_elipse <- function(i,j,dados,alfa,cor= "red",espessura = 3)
{
  sigma <- cov(dados[,c(i,j)])
  medias <- c(mean(dados[,i]), mean(dados[,j]))
  autov <- eigen(sigma)
  print(autov)
  rotacao <- -angulo(c(1,0),autov$vectors[1,])
  parametric.draw(c(0,2*pi),elipse(elipse.a(alfa,autov$values),elipse.b(alfa,autov$values),
                                   rotacao,medias[1],medias[2]),col=cor,lwd=espessura)
}

par(mfrow=c(1,1))
plot(medidas[,c(1,2)])
desenha_elipse(1,2,as.matrix(medidas),0.5,cor = "blue", espessura= 3)
desenha_elipse(1,2,as.matrix(medidas),0.9 , cor = "purple", espessura =2 )
desenha_elipse(1,2,as.matrix(medidas),0.95 , cor = "red" , espessura =1)
