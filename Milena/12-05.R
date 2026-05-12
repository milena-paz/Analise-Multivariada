x <- HSAUR2::USairpollution |>as.matrix()
dim(x)
names(x)
colSums(is.na(x))
cidades <- rownames(x)
abreviatura <- abbreviate(cidades,4)
x$negtemp <- -1*x$temp
x$temp <- NULL

R <- cor(x[-1])

#### ANALISE DE COMPONENTES PRINCIPAIS ####
(poluicao.av <- eigen(R))
lambda <- poluicao.av$values
E <- poluicao.av$vectors
poluicao.acp <- princomp(x[-1],cor=T)
str(poluicao.acp)

percentuais <- lambda/sum(lambda)*100
percentuais |> round(1)
round(cumsum(percentuais),1)

(poluicao.acp$sdev)**2 # == lambda
summary(poluicao.acp)
# poluicao.acp$loadings é equivalente a E

plot(poluicao.acp,type="l")
round(E**2*100,1)
# MATRIZ DE LOADINGS: MATRIZ DE CORRELACOES ENTRE VARIAVEL E COMPONENTE
Lambda <- diag(sqrt(lambda)) #PARA VARIAVEIS PADRONIZADAS
(L <- E%*%Lambda) |> round(3)
round(L**2,3)
rowSums(L**2)
colSums(L**2)

#COMUNALIDADE PARA 3 COMPONENTES
rowSums((L**2)[,1:3])
# sum(rowSums((L**2)[,1:3])) = sum(lambda[1:3])
#ESPECIFICIDADE PARA 3 COMPONENTES
1 - rowSums((L**2)[,1:3]) |>round(3)
###------------------------------------------###
#COMUNALIDADE PARA 4 COMPONENTES
rowSums((L**2)[,1:4]) |>round(3)
# sum(rowSums((L**2)[,1:4])) = sum(lambda[1:4])
#ESPECIFICIDADE PARA 4 COMPONENTES
(1 - rowSums((L**2)[,1:4])) |>round()


R.3 <- tcrossprod(L[,1:3]) # ?tcrossprod
R.3
sum(diag(R.3)) #== sum((L**2)[,1:4])
R.4 <- tcrossprod(L[,1:4])
## SOMA DE ERROS AO QUADRADO ##
(R.3 - R)**2 |>sum()

### ESCORES ###
Y <-  x[,-1]%*%E
plot(abs(Y),abs(x[,-1]%*%poluicao.acp$loadings))
