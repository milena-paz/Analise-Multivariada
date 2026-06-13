# Temos que \Sigma=LL'+\Psi.
# Digamos que haja uma L*=LT, onde T é uma matriz ortogonal(!).
# Essa matriz L* é uma matriz de loadings rotacionados.
# (!)Assim: L*L*'=LT(LT)'=LTT'L=LL
# Trabalhando com dados centrados, obtemos:
# Sn=LmLm'+Psi
# Trabalhando com dados padronizados, obtemos:
# Pn=LmLm'+Psi

acoes <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/T8-4.DAT")
af.acoes <- factanal(acoes,2,rotation="none")
L <- af.acoes$loadings[,1:2]
afvari.acoes <- factanal(acoes,2)
afvari.acoes$loadings[,1:2]
Lrot <- varimax(L)$loadings
plot(Lrot,pch=2)
text(x=Lrot[,1],y=Lrot[,2],labels=c("JP Morgan","Citibank",
                                    "Wells Fargo","Royal Dutch Shell","Exxon Mobil"),
     cex=0.75,pos=c(3,1,3,3,3))
library(GPArotation)
quartimax(L)