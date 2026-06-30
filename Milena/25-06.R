x <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/T11-8.DAT",
                col.names=c("grupo","anticorpo","antigeno"))
x$grupo <- as.factor(x$grupo)
str(x)
cov.l <- lapply(unique(x$grupo), function(y)
    cov(x[x$grupo==y,-1],use="na.or.complete")
  )
cov.l
(tabela <- table(x$grupo)) #tamanhos de amostra
n1 <- tabela[1]
n2 <- tabela[2]

S1 <- cov.l[[1]]
S2 <- cov.l[[2]]
rm(cov.l,tabela)
gc()
(S.p <- ((n1-1)*S1+(n2-1)*S2)/(n1+n2-2))

medias <- tapply(x[-1],x$grupo, colMeans)
Sp.inv <- solve(S.p)

b<- ((medias[[1]]-medias[[2]])%*%Sp.inv)

xmat <- as.matrix(x[-1])
y <- tcrossprod(b,xmat)

centro <- sapply(medias, function(x) b%*%x)

(m <- sum(centro)/2)
#REGRA DE CLASSIFICACAO
# xi É ALOCADO NA POP. 1 SE SEU ESCORE y >=m
# xi É ALOCADO NA POP. 2 SE SEU ESCORE y <m

(D2 <- b%*%(medias[[1]]-medias[[2]]))

#predicao: 
#  digamos que haja uma mulher com anticorpo -0.210 e antigeno -0.044
#ESCORE  DE CLASSIFICACAO:
x0 <- c(-0.210,-0.044)
b%*%x0
b%*%x0<m
#entao alocamos na população 1

#distancia de Mahalanobis dessa obs ate  os centroides
sapply(medias,function(x)t(x0-x)%*%Sp.inv%*%(x0-x))

#TESTE DA DIFERENCA ENTRE CENTROIDES
p=2
f <- (n1+n2-p-1)/((n1+n2-2)*p)*n1*n2/(n1+n2)*D2
1-pf(f, df1=p,df2=n1+n2-p-1)
#conclusao: ha diferença significativa entre os centroides
##########30/06/2026##########
## E RECOMENDADO ESCALONAR(NORMALIZAR) OS COMPONENTES DE b
#escalonamento tipo 1
b.st <- b/norm(b,type="e")
#escalonamento tipo 2
b/b[1]  #recomendavel somente se preditoras forem padronizadas 

ajustead=MASS::lda(grupo~antigeno+anticorpo,data=x,prior=c(.5,.5))
ajustead
summary(ajustead)

#o escalonamento feito pelo pacote e diferente, do tipo:
diferenca=apply(rbind(medias[[1]],medias[[2]]),2,function(x) x[1]-x[2])
# COEFICIENTES DE DISCRIMINACAO DE FISHER
diferenca%*%Sp.inv #=b
#escalonamento
b/c(sqrt(diferenca%*%Sp.inv%*%diferenca))

medias=cbind(medias[[1]],medias[[2]])
## ESCORES DISCRIMINANTES
head(predict(ajustead)$posterior,5)
####
#manualmente pelo mesmo metodo
x0=x[c(1:5),-1]
dist=apply(medias,2,function(m)mahalanobis(x0,center=m,cov=Sp.inv,inverted=T))
f=exp(-1/2*dist)
t(apply(f,1,function(f)f/sum(f)))

#predicao: 
#  digamos que haja uma mulher com anticorpo -0.210 e antigeno -0.044
#ESCORE  DE CLASSIFICACAO:
x0 <- data.frame(anticorpo=-0.210,antigeno=-0.044)
predict(ajustead,newdata=x0)
m
#aloca ao grupo 1

klaR::partimat(grupo~.,data=x,method="lda",main="Gráfico da partição")
############## TABELA DE CONFUSAO ##############
contagem=table(predict(ajustead)$class,x$grupo)
###porcentagem de classificacoes corretas por categoria
diag(prop.table(contagem,1))
### acuracia do modelo: porcentagem total de classificacoes corretas
sum(diag(prop.table(contagem)))

### ERROS DE CLASSIFICACAO
# * Erro de tipo 1:
#   – Elemento amostral pertence a π1, mas é
# classificado em π2.
# * Erro de tipo 2:
#   – Elemento amostral pertence a π2, mas é
# classificado em π1.
#  A função de discriminação será melhor
# quanto menores as probabilidades desses
# erros.

ajuste.ad2 <- MASS::lda(grupo ~ anticorpo + antigeno, data = x,
                         prior = c(0.75, 0.25))
# Predição de observação
x0 <- data.frame(anticorpo = -0.210, antigeno = -0.044)
predict(ajuste.ad2, newdata = x0)

