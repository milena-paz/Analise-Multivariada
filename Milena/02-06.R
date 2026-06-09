
acoes <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/T8-4.DAT")

# TESTE DE DECISAO DO NUMERO DE FATORES:
# H0: m fatores sao suficientes
# H1: sao necessarios mais que m fatores
# T= (n-1-(2p+4m+5)/6)*ln(det(L.z%*%t(L)+Psi_z)/det(R))
# sob H0 T\sim chisq(gl); onde gl=((p-m)^2-p-m)/2

#### Conjunto de dados: Ações ####

m=1
obj <- factanal(acoes,factors = m,rotation="none")
L <- obj$loadings
Psi <- diag(obj$uniquenesses)

(R.1 <- tcrossprod(L,L)+Psi)
(R <- cor(acoes))
det(R.1)
det(R)
t <- (n-1-(2*p+4*m+5)/6)*log(det(R.2)/det(R))
((p-m)^2-p-m)/2 #grau de liberdade

qchisq(0.95,df=5)
pchisq(t,df=5,lower.tail = F)

obj$criteria[1]#AKAIKE
# Se dois métodos tem a mesma verossimilhança, o
# procedimento vai privilegiar o modelo com menor
# número de fatores

#CRITERIO DE SCHWARZ
obj$criteria[1]+log(n)*m/2
residuos <- sum((R-R.1)^2)

###

obj <- factanal(acoes,factors = 2,rotation="none")
L <- obj$loadings
Psi <- diag(obj$uniquenesses)

(R.2 <- tcrossprod(L,L)+Psi)
det(R.2)
det(R)

m <- 2
n <- nrow(acoes)
p <- ncol(acoes)
t <- (n-1-(2*p+4*m+5)/6)*log(det(R.2)/det(R))
((p-m)^2-p-m)/2 #grau de liberdade

qchisq(0.95,df=1)
pchisq(t,df=1,lower.tail = F)
#CONCLUSAO: 2 FATORES SAO SUFICIENTES

aic=obj$criteria[1]
## SUPOSICOES:
# dados provenientes de normal multivariada;
# envolve metodo de maxima verossimilhança

residuos.2 <- det((R-R.2)^2)

#CRITERIO KMO
psych::KMO(R)

# TESTE DE BARLETT
#testa esfericidade da matriz R
#supoe distribuicao multivariada
#modelo de analise fatorial pressupoe variaveis respota correlacionadas entre si
#Verifica se P=\rho é proxima da identidade
#H0: P=I; H1: P!= I
#lambda= autovalores de R
#ESTATISTICA
# t= -(n-(2*p+1)/6)*sum(log(lambda))
#com gl=p(p-1)/2
lambda <- eigen(R)$values
t2 <- -(n-(2*p+1)/6)*sum(log(lambda))
p(p-1)/2
qchisq(0.95,df=10)
pchisq(t2,df=10,lower.tail = F)

psych::cortest.bartlett(R,n=n)

#### MINIMOS QUADRADOS PONDERADOS ####
nomes <- c("JP Morgan","Citibank","Wells Fargo","Royal Dutch Shell","Exxon Mobil")
xbar <- colMeans(acoes)
names(acoes) <- nomes
sapply(acoes,acf)

## estimacao de L por componentes principais 
R <- cor(acoes)
acoes.av <- eigen(R)
E <- acoes.av$vectors
lambda <- acoes.av$values

L <- E%*%diag(sqrt(lambda))
rownames(L) <- names(acoes)
colnames(L) <- paste0("F",1:5)
L2 <- L[,1:2]
#comunalidade
comum <- rowSums(L2**2)
sum(comum)/5  #77% da variância é explicada por esses fatores
unica <- 1-comum

(tab.cp <- cbind(L2,comum,unica))
#residuos
res.cp <- R-tcrossprod(L2)-diag(unica)
sum(res.cp**2)

## estimacao por max verossimilhanca

obj <- factanal(acoes,factors = 2,rotation="none")
L.mv2 <- obj$loadings
rownames(L.mv2) <- nomes
colnames(L.mv2) <- paste0("F",1:2)
L.mv2
Psi <- diag(obj$uniquenesses)

obj
comum <- rowSums(L.mv2^2)
sum(comum)/5
(unica <- 1-comum) #UNICA = PSI
(tab.mv <- cbind(L.mv2,comum,unica))
#residuos
res.mv <- R-tcrossprod(L.mv2)-diag(unica)
sum(res.mv**2)


## ESCORES POR MINIMOS QUADRADOS PONDERADOS
obj <- factanal(acoes,factors = 2)
L.Rot <- obj$l
L.Rot

## matriz W p/ mv
solve(t(L.mv2)%*%solve(Psi)%*%L.mv2)%*%t(L.mv2)%*%solve(Psi)
W.mqp <- solve(t(L.mv2)%*%solve(Psi)%*%L.mv2)%*%t(L.mv2)%*%solve(Psi)
round(W.mqp,3)
Z <- scale(acoes)
F2 <- Z%*%t(W.mqp)
plot(F2)
colMeans(F2)
psych::cortest.bartlett(cov(F2))
#Fjk=(t(L)%*%Psi.inv%*%L)t(L)%*%Psi.inv%*%Zk=W%*%Zk

#usando agora L por componentes principais
Psi <- diag(nrow=5,ncol=5)
W.cp <- solve(t(L2)%*%solve(Psi)%*%L2)%*%t(L2)%*%solve(Psi)
round(W.cp,3)
Z <- scale(acoes)
F2.cp <- Z%*%t(W.cp)
plot(F2.cp)
colMeans(F2.cp)
psych::cortest.bartlett(cov(F2))


plot(F2[,1],F2.cp[,1])
plot(F2[,2],F2.cp[,2])
cor(F2[,1],F2.cp[,1])

###########################

R <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/E9-6.DAT")|>as.matrix()
n <- 280
for(i in 3:5){
  print(factanal(covmat=R,factors=i,n.obs=n)$PVAL)
}
psych::KMO(R)

