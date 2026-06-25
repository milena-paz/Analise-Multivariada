x <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/T11-8.DAT",
                col.names=c("grupo","anticorpo","antigeno"))
x$grupo <- as.factor(x$grupo)
str(x)
cov.l <- lapply(unique(x$grupo), function(y)
  cov(x[x$grupo==y,-1],use="na.or.complete"))
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