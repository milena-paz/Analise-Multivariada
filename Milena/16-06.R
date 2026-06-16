source("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/chap4druguse.dat")
R <- druguse.cor
corrplot::corrplot(R)
psych::cortest.bartlett(R,n=1634)
psych::KMO(R)

sapply(1:6,function(m)
  factanal(covmat=R,factors=m,n.obs=1634)$PVAL)
ft6 <- factanal(covmat=R,factors=6,n.obs=1634)
ft6
#omitindo loadings menores que 0.25
print(ft6$loadings,cutoff=0.25)
library(GPArotation)
#SEM ROTACAO
L <- factanal(covmat=R,factors=6,n.obs=1634,rotation="none")$loadings
#QUARTIMAX
L.qt <- factanal(covmat=R,factors=6,n.obs=1634,rotation="quartimax")$loadings
print(L.qt,cutoff=0.25)
#EQUAMAX
L.eq <- factanal(covmat=R,factors=6,n.obs=1634,rotation="equamax")$loadings
print(L.eq,cutoff=0.25)
#OBLIMIN
L.ob <- factanal(covmat=R,factors=6,n.obs=1634,rotation="oblimin")$loadings
print(L.ob,cutoff=.25)
#covariancia dos fatores
attr(L.ob,"covariance")
# como essa rotação é oblíqua temos que considerar a covariancia
### NUMERO DE FATORES PODE SER FEITO COM COMPONENTES PRINCIPAIS -> SCREEPLOT
screeplot(prcomp(R),type="l")
ft3 <- factanal(covmat=R,factors=3,n.obs=1634)
print(ft3$loadings,cutoff=0.25)
