# matriz de dados
X <- iris[c("Petal.Length", "Petal.Width")]
# Matriz de covariâncias
S <- cov(X)
S

Xc <- scale(x = X, center = TRUE, scale = FALSE)

# limites simétricos
lim.x <- c(-1, 1) * 3.2
lim.y <- c(-1, 1) * 1.3

cores <- c("yellow", "blue", "green") 

# diagrama de dispersão
plot(x = Xc, pch = 19, xlim = lim.x, ylim = lim.y, 
     col = cores[as.numeric(iris$Species)])

# eixos coordenados
abline(h = 0, v = 0, lty = 2)

# pontos escolhidos
pontos <- c(44, 62, 123, 135)

# Xc - dados como vetores
plot(x = Xc, xlim = lim.x, ylim = lim.y, pch = '.',
     col = cores[as.numeric(iris$Species)])
arrows(x0 = 0, y0 = 0, x1 = Xc[, 1], y1 = Xc[, 2], lty = 1, length = 0.15, 
       angle = 15, col = cores[as.numeric(iris$Species)])

# Localização dos pontos escolhidos
points(x = Xc[pontos, 1], y = Xc[pontos, 2], pch = 3, cex = 1)
text(x = Xc[pontos, 1], y = Xc[pontos, 2], label = pontos, cex = .85, 
     pos = 3)

# pontos escolhidos
pontos <- c(44, 62, 123, 135)

# Xc - dados como vetores
plot(x = Xc, xlim = lim.x, ylim = lim.y, pch = '.',
     col = cores[as.numeric(iris$Species)])
arrows(x0 = 0, y0 = 0, x1 = Xc[, 1], y1 = Xc[, 2], lty = 1, length = 0.15, 
       angle = 15, col = cores[as.numeric(iris$Species)])

# Localização dos pontos escolhidos
points(x = Xc[pontos, 1], y = Xc[pontos, 2], pch = 3, cex = 1)
text(x = Xc[pontos, 1], y = Xc[pontos, 2], label = pontos, cex = .85, 
     pos = 3)

# plot apenas com os 4 pontos
plot(Xc[pontos, ], xlim = lim.x, ylim = lim.y, type = 'n',
     col = cores[as.numeric(iris$Species)], asp = 1)

# setas dos vetores
arrows(x0 = 0, y0 = 0, x1 = Xc[pontos, 1], y1 = Xc[pontos, 2], lty = 1, 
       length = 0.15, angle = 15, col = cores[as.numeric(iris$Species)]
)

# identificação dos pontos
text(x = Xc[pontos, 1], y = Xc[pontos, 2], label = pontos, cex = .85, 
     pos = 3)

# eixo original
abline(h = 0, v = 0)

########## Geometria Vetorial


# vetor associado ao ponto 44
x44 <- Xc[44, ]

# Intensidade do vetor (em relação ao eixo x: (1, 0))
sqrt(t(x44) %*% x44)

# Comprimento dos 4 vetores
Xp <- as.matrix(Xc[pontos, ])
sqrt(diag(Xp %*% t(Xp)))     

################## Projeção dos Vetores

plot(Xc[pontos, ], xlim = lim.x, ylim = lim.y, type = 'n',
     col = cores[as.numeric(iris$Species)], asp = 1)
arrows(x0 = 0, y0 = 0, x1 = Xc[pontos, 1], y1 = Xc[pontos, 2], lty = 1, 
       length = 0.15, angle = 15, col = cores[as.numeric(iris$Species)]
)
text(x = Xc[pontos, 1], y = Xc[pontos, 2], label = pontos, cex = .85, 
     pos = 3)
abline(h = 0, v = 0)

# vetor e1
e1 <- c(1, 1)*sqrt(2)/2
e1

# vetor e2
e2 <- c(-1, 1)*sqrt(2)/2
e2

# Gráfico dos vetores do novo sistema
k <- 0.5
arrows(x0 = 0, y0 = 0, 
       x1 = c(e1[1], e2[1])*k, y1 = c(e1[2], e2[2])*k, 
       col = 'black', angle = 20, length = .25)

text(x = c(e1[1], e2[1])*k, y = c(e1[2], e2[2])*k, pos = 4,
     label = c('e1', 'e2'), cex = 0.85, font = 2)

# Refazendo o Gráfico anterior
plot(Xc[pontos, ], xlim = lim.x, ylim = lim.y, type = 'n',
     col = cores[as.numeric(iris$Species)], asp = 1)
arrows(x0 = 0, y0 = 0, x1 = Xc[pontos, 1], y1 = Xc[pontos, 2], lty = 1, 
       length = 0.15, angle = 15, col = cores[as.numeric(iris$Species)]
)
text(x = Xc[pontos, 1], y = Xc[pontos, 2], label = pontos, cex = .85, 
     pos = 3)
abline(h = 0, v = 0)

# vetor e1
e1 <- c(1, 1)*sqrt(2)/2
e1

# vetor e2
e2 <- c(-1, 1)*sqrt(2)/2
e2

# Gráfico dos vetores do novo sistema
k <- 0.5
arrows(x0 = 0, y0 = 0, 
       x1 = c(e1[1], e2[1])*k, y1 = c(e1[2], e2[2])*k, 
       col = 'black', angle = 20, length = .25)

text(x = c(e1[1], e2[1])*k, y = c(e1[2], e2[2])*k, pos = 4,
     label = c('e1', 'e2'), cex = 0.85, font = 2)

# ----------------  Projeções  ---------------

# Projeção do vetor X44 em e1
prj.e1 <- as.vector((t(x44)%*%e1))*e1
prj.e1

# intensidade do vetor prj.e1
mag.e1 <- as.vector((t(x44)%*%e1))

# projeção em e1
segments(x0 = 0, y0 = 0, x1 = prj.e1[1], y1 = prj.e1[2], 
         col = 'red', lwd = 2)

segments(x0 = x44[1], y0 = x44[2], x1 = prj.e1[1], y1 = prj.e1[2],
         lty = 2)

# Projeção do vetor X44 em e2
prj.e2 <- as.vector((t(x44)%*%e2))*e2
prj.e2

# intensidade do vetor prj.e2
mag.e2 <- as.vector((t(x44)%*%e2))

segments(x0 = 0, y0 = 0, x1 = prj.e2[1], y1 = prj.e2[2], 
         col = 'red', lwd = 2)

segments(x0 = x44[1], y0 = x44[2], x1 = prj.e2[1], y1 = prj.e2[2],
         lty = 2)

# Coordenadas dos pontos no novo sistema e1 x e2
W <- matrix(c(1, 1, -1, 1)* sqrt(2)/2, ncol = 2, byrow = F) 
#matriz com as coordenadas dos pontos no novo sistema
Yp <- Xp %*% W
rownames(Yp) <- pontos
Yp

############ Repetir a projeção agora para 123 #######

x123 <- Xc[123, ]
x123

# projeção vetorial
prj.e1_123 <- as.vector((t(x123) %*% e1)) * e1
prj.e1_123

# intensidade (coordenada no eixo e1)
mag.e1_123 <- as.vector(t(x123) %*% e1)
mag.e1_123

# projeção vetorial
prj.e2_123 <- as.vector((t(x123) %*% e2)) * e2
prj.e2_123

# intensidade (coordenada no eixo e2)
mag.e2_123 <- as.vector(t(x123) %*% e2)
mag.e2_123

# projeção em e1
segments(x0 = 0, y0 = 0, x1 = prj.e1_123[1], y1 = prj.e1_123[2],
         col = 'red', lwd = 2)

segments(x0 = x123[1], y0 = x123[2],
         x1 = prj.e1_123[1], y1 = prj.e1_123[2],
         lty = 2)

# projeção em e2
segments(x0 = 0, y0 = 0, x1 = prj.e2_123[1], y1 = prj.e2_123[2],
         col = 'red', lwd = 2)

segments(x0 = x123[1], y0 = x123[2],
         x1 = prj.e2_123[1], y1 = prj.e2_123[2],
         lty = 2)

c(mag.e1_123, mag.e2_123)

############### Desenhar o iris todo para esse sistema de cooordenadas ##############
####################### Transformar os dados usando a matriz W #############################
###########  Projetar em 45° #################

# 1. dados completos
X <- iris[, 1:4]

# 2. centralizar
Xc <- scale(X, center = TRUE, scale = FALSE)

# 3. matriz de rotação 2D (45°)
R2 <- matrix(c(1, 1,
               -1, 1)*sqrt(2)/2, ncol = 2, byrow = FALSE)

# 4. matriz de rotação 4D (bloco diagonal)
W <- matrix(0, nrow = 4, ncol = 4)

W[1:2, 1:2] <- R2   # sepal
W[3:4, 3:4] <- R2   # petal

# 5. transformação
Y <- Xc %*% W

# 6. visualizar
head(Y)

cores <- c("yellow", "blue", "green") 

plot(Y[,3], Y[,4],
     col = cores[as.numeric(iris$Species)],
     pch = 19,
     asp = 1,
     xlab = "Petal - eixo 1 (45°)",
     ylab = "Petal - eixo 2 (45°)")

abline(h = 0, v = 0, lty = 2)


############# FAZER GRÁFICO USANDO E1 E E2 ##################

###### w[ E1~ E2~]
###### S <- cov(XP)
###### W <- eig(S)$vectors


# matriz de dados
X <- iris[c("Petal.Length", "Petal.Width")]
# Matriz de covariâncias
S <- cov(X)
S

Xc <- scale(x = X, center = TRUE, scale = FALSE)

# limites simétricos
lim.x <- c(-1, 1) * 3.2
lim.y <- c(-1, 1) * 1.3

cores <- c("yellow", "blue", "green") 

# diagrama de dispersão
plot(x = Xc, pch = 19, xlim = lim.x, ylim = lim.y, 
     col = cores[as.numeric(iris$Species)])

# eixos coordenados
abline(h = 0, v = 0, lty = 2)

# pontos escolhidos
pontos <- c(44, 62, 123, 135)

# destacar pontos
points(Xc[pontos,1], Xc[pontos,2], pch = 3, cex = 1)
text(Xc[pontos,1], Xc[pontos,2],
     labels = pontos, pos = 3)

# eixos
abline(h = 0, v = 0, lty = 2)

# matriz de covariância
S <- cov(Xc)

# autovetores
eig <- eigen(S)
e1 <- eig$vectors[,1]
e2 <- eig$vectors[,2]

# desenhar retas (direções dos eixos)
abline(a = 0, b = e1[2]/e1[1], col = "red", lwd = 2)
abline(a = 0, b = e2[2]/e2[1], col = "blue", lwd = 2)

# rótulos (opcional)
text(2, 2*(e1[2]/e1[1]), "e1", col = "red", pos = 4)
text(2, 2*(e2[2]/e2[1]), "e2", col = "blue", pos = 4)

#################### Calcular a variancia da matriz Y ##########################
var(Petal.Length)
var(X, Petal.Width)
