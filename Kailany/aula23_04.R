X <- as.matrix(iris[-5])
S <- cov(X)
iris.av <- eigen(S)
lambda <- iris.av$values
E <- iris.av$vectors
sum(diag(S))

# ORDENAR AS 4 VARIAVEIS E DAR AS PORCENTAGENS ORDENADAS, DA MAIOR VARIÃNCIA PARA A MENOR
# variâncias (diagonal)
variancias <- diag(S)

# ordenar da maior para a menor
ordem <- order(variancias, decreasing = TRUE)

variancias_ord <- variancias[ordem]

# porcentagem relativa
porcentagens <- variancias_ord / sum(variancias_ord) * 100

# mostrar resultados
variancias_ord
porcentagens

prop <- lambda/sum(lambda)
prop
tabela <- data.frame(
  Componente= paste0("Y", 1:4),
  Autovalor= round( lambda, 3),
  Proporcao= round(prop, 3), 
  Percentual= paste(round(100* prop, 1), "%")
)
tabela

plot(lambda, type= "b", col= "black")

E

## FAZER GRAFICO DOS COEFICIENTES DAS 2 PRIMEIRAS VARIAVEIS

plot( E[,1:2], pch=21, bg="black",
      xlim= c(-0.2,1), ylim= c(-0.8, 0.3),
      xlabs="CP1", ylabs= "CP2")
abline(h=0, v=0, lty= 1)
text(E[, 1:2], labels= names(iris[-5]), pos= c(1,1,3,3), cex=0.85)
arrows(x0=0, y0=0, x1=E[,1], y1= E[,2], angle=15, lty=2, col="blue")

# calcular as componentes para cada uma das 150 observações, sem usar comandos de pca

Y <- X %*% E
Y

# Fazer o graficos das 2 componetes em x e em Y e pinte as cores de cada especie

cores<- RColorBrewer::brewer.pal(3, "Set2")
plot(Y[,1:2], pch=21, bg=cores[as.numeric(iris$Species)])

#Calcular a correlação entre a y1 e X3

cor(Y[,1], X[,3])
cor(Y[,2], X[,2])

Lambda <- diag(lambda)
V <- diag(diag(S))
Ns <- diag(1/sqrt(diag(V))) %*% E %*% diag(sqrt(diag(Lambda)))
Lambda
V
Ns

0.15*0.15
(Ns)^2
# Se somar cada linha dá 1

############## AULA 30 DE ABRIL ####################
perc <- lambda/sum(lambda)*100
acum <- cumsum(perc)
tabela.val <- cbind(round(lambda, 3), round(perc, 2), round(acum, 1))

rownames(tabela.val) <- paste0("CP.", 1:4)
colnames(tabela.val) <- c("Autovalor", "% variancia", "%acumulada")
tabela.val

E <- iris.av$vectors
colnames(E) <- paste("Comp.", 1:4)
rownames(E) <- names(iris[-5])
E

Lambda <- diag(iris.av$values)
Vsqrt.inv <- diag(1/sqrt(diag(S)))

cor.YX <- Vsqrt.inv %*% E %*% sqrt(Lambda)
colnames(cor.YX) <- paste("Comp.", 1:4)
rownames(cor.YX) <- names(iris[-5])
round(cor.YX, 3)

# Correlação tá relacionada com o cos do angulo entre 2 vetores

colSums(cor.YX**2)

corrplot::corrplot(cor.YX, is.corr= T)

Lambda.meio<- diag(sqrt(iris.av$values))
L <- E %*% Lambda.meio
colnames(L) <- colnames(cor.YX) <- paste("Comp.", 1:4)
rownames(L) <- names(iris[-5])
round(L, 3)

Vsqrt.inv %*% L
colSums(L**2, 3)
rowSums(L**2, 3)

diag(S)
corrplot::corrplot(L**2, is.corr= F)

rbind(contribuicao <- E**2, Total= apply((E^2), 2, sum))
contribuicao

par(mfrow= 1,2)
      
cos2 <- cor.YX**2
cos2

rowSums(cos2)
colSums(cos2)
sum(cos2)

cos2_duas <- rowSums(cos2[, 1:2])
cos2_duas

ordem <- order(cos2_duas, decreasing = T)
bar2 <-  barplot(cos2_duas[ordem], names.arg= names(cos2_duas),
            angle=45, cex.names=0.85, xaxt= "n",
            ylab= "Cos2 - Qualidade de repre...",
            main= "Cos2 das variaveis para Cp1 e cp2")
bar2

# ACP - Gráfico das variáveis (círculo de correlações)

plot(0,
     type = "n",
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     asp = 1,
     main = "ACP - Variáveis",
     xlab = paste0("CP1 (", tabela.val[1, 2], "%)"),
     ylab = paste0("CP2 (", tabela.val[2, 2], "%)")
)

abline(h = 0, v = 0, lty = 2)

plotrix::draw.circle(x = 0, y = 0, radius = 1)

arrows(x0 = 0, y0 = 0,
       x1 = cos2[, 1],
       y1 = cos2[, 2],
       length = 0.15,
       angle = 20)

text(x = cos2[, 1],
     y = cos2[, 2],
     labels = names(iris[-5]),
     cex = 0.7,
     pos = 4)

# Para cada uma das 150 observações eu quero calcular o valor de cada componente
X <- as.matrix(iris[-5])
escores <- X %*% iris.av$vector
dim(escores)
head(escores)

apply(escores, 2, function(x) c(media=mean(X), 
                                desvio= sd(X),
                                variancia = var(X)
      )
)


#escores.media <- as.vector(t(iris.av$vectors) %*% )
#escores.media

S.rec <- E %*% Lambda %*% t(E)
S.rec - S
sum((S.rec - S)**2)
 
S1<- L[,1] %*% t(L[,1])
S1
sum((S1- S)**2)

