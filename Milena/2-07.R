###### TESTE M DE BOX ######
#JOHNSON, WICHERN PG 311 SECAO 6.6
# Para g populações diferentes, testamos:
# H0: \Sigma_1=\Sigma_2=...=\Sigma_g
#   \Lambda=\prod_l\frac{|S_l|}{|S_pool|}^{(n_l-1)/2}, onde
# nl=tamanho do grupo l;
# S_pool=\frac{\sum_l^g{(n_l-1)S_l}}{\sum_l^g(n_l-1)}
# M=\sum_l(n_l-1)\ln(S_pool)-\sum_l[(n_l-1)ln(S_l)]
# m=\frac{1}{\sum_l(n_l-1)}-


x <- read.table("/home/sala/Documentos/MILENA-ANALISE.MULTIVARIADA/dados/T11-8.DAT",
                col.names=c("grupo","anticorpo","antigeno"))
x$grupo <- as.factor(x$grupo)
cov.l <- lapply(unique(x$grupo), function(y)
  cov(x[x$grupo==y,-1],use="na.or.complete")
)
cov.l
(tabela <- table(x$grupo)) #tamanhos de amostra
n1 <- tabela[1]
n2 <- tabela[2]

S1 <- cov.l[[1]]
S2 <- cov.l[[2]]
(res<- (biotools::boxM(x[-1],grouping=x$grupo)))
res$pooled
Spooled <- function(covs,grupos){
  #---------------------------------#
  # covs: lista contendo as matrizes
  #   de covariancia de cada grupo
  # grupos: fator indicador do grupo
  #   no conjunto de dados
  #---------------------------------#
  g <- length(covs)
  n <- tapply(grupos,grupos,length)
  aux <- sapply(1:g, function(l) (n[l]-1)*covs[[l]],simplify=F)
  return(Reduce("+",aux)/(sum(n)-g))
}

(S.p <- sapply(1:2,function(l) (tabela[l]-1)*cov.l[[l]],simplify=F))
S.p=Reduce("+",S.p)/(sum(tabela)-2)
## VAMOS AJUSTAR AO MODELO QUADRATICO DE CLASSIFICACAO
ajusteqd=MASS::qda(grupo~antigeno+anticorpo,data=x,prior=c(.5,.5))
ajusteqd
table(predict(ajusteqd)$class,x$grupo)
klaR::partimat(grupo~.,data=x,method="qda",main="Gráfico da partição")
