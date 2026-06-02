decatlo <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/E9-6.DAT")
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
