### 05/05/2026 ###
#### Número de componentes principais ####
# Não há resposta definitiva para quantas componentes devem ser retidas, mas
# temos alguns fatores a considerar antes de definir essa quantidade.
# - Quantidade explicada de variância total
# - Tamanho relativo dos autovalores (variancia das componentes amostrais)
# - Interpretação das componentes
# - Scree-plot (lambda_i vs i)
#   ex.
    x <- iris[-5]
    xval <- eigen(S<-cov(x))$values
    plot(xval,ylab="Autovalor",type="b")
#   busca-se um "cotovelo" no gráfico; São consideradas as componentes até o ponto em que
#  os autovalores remanescentes são relativamente
#  pequenos e todos aproximadamente do mesmo valor

#### Componentes Principais de Variáveis Padronizadas ####
# Um vetor aleatório X pode ser padronizado como segue:
v.meioinv <- diag(1/diag(sqrt(S)))
z <-scale(x,scale=F)%*%v.meioinv
(P<- cov(z)) #matriz de correlacao de x
S
