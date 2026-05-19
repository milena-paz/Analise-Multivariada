medidas <- read.table("~/Documentos/MILENA-ANALISE.MULTIVARIADA/T4-3.DAT",header=F)
names(medidas) <- c(paste0("X",1:4),"dj2")
medidas <- medidas[-5]
tam <- dim(medidas)[1]
vars <- dim(medidas)[2]
library(nortest)
#### VERIFICAR NORMALIDADE UNIVARIADA: ####
# - QQ-PLOT
# - TESTES DE NORMALIDADE:
#   H0: A variável é normalmente distribuida
#   H1: A variável não é normalmente distribuida
# - OBS. ATÍPICAS

apply(medidas,2,function(x){
  qqnorm(scale(x),pch=c(rep(1,8),19,rep(1,20)))
  ##linha 9 destacada
  abline(a=0,b=1,col="red")
  pvalores <- c(ad.test(x)$p.value,  shapiro.test(x)$p.value,  lillie.test(x)$p.value) |> round(4)
  mtext(paste0("AD: ",pvalores[1],"\n",
               "Shap: ",pvalores[2],"\n",
               "Lill: ",pvalores[3],"\n"),line=-5,at=-1.15,cex=0.85)
})

#### VERIFICAR NORMALIDADE BIVARIADA: ####
# - GRÁFICOS DE DISPERSÃO 2 A 2
# - VERIFICAR ELIPSES DE:
#   50% -> Quantidade
#   95% -> Outliers
pares <- list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4))
pairs(medidas,pch=c(rep(1,8),17,rep(1,6),15,rep(1,13)))
Xc <- scale(medidas,scale=F)

grafico <- function(i,j){ #GRAFICO DE DISPERSAO
  plot(medidas[[paste0("X",i)]],medidas[[paste0("X",j)]],
       xlab=paste0("X",i),ylab=paste0("X",j))
}
Sij <- function(i,j){#COVARIANCIAS
  cov(Xc[,c(i,j)])
}
dj2 <- function(ij){
  diag(Xc[,ij]%*%solve(Sij(ij[1],ij[2]))%*%t(Xc[,ij]))
}
dj2.1 <- dj2(pares[[1]])
which(dj2.1 > qchisq(0.95,df=2))

sapply(pares,function(ij) #qui-quadrado p/ achar outliers
  which(dj2(ij) > qchisq(0.95,df=2)))
library(ellipse)
#GRAFICO CENTRADO
graficoC <- function(i,j){
  plot(Xc[,i],Xc[,j],
       xlab=paste0("X",i),ylab=paste0("X",j))
}
## ELIPSES 95%
sapply(pares,function(ij){
  graficoC(ij[1],ij[2])
  disp <- Xc[,ij]
  elip <- ellipse(cor(disp)[1,2],level=0.95,scale=apply(disp,2,sd),
                  centre=apply(disp,2,mean))
  lines(elip,col="red",lwd=2)
  })
#### VERIFICAR NORMALIDADE TETRAVARIADA: ####
# - QQ-PLOT CHISQ(4)
# - dj^2= (X_i- X.barra)'S^(-1)(X_i - X.barra)
# - TESTE QUI-QUADRADO
S <- cov(medidas)
dj2 <- diag(Xc%*%solve(S)%*%t(Xc))
#dj2 <- mahalanobis(medidas,center=colMeans(medidas),cov=S)
q.teorico <- qchisq(ppoints(500),df=4)
qqplot(q.teorico,y=dj2)
qqline(dj2,distribution=function(p)qchisq(p,df=4),col="red",lty=2)
which(dj2 > qchisq(0.95,df=2))
