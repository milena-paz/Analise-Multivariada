# Dados sem a coluna Species
X <- iris[, -5]

# Matrizes básicas
S <- cov(X)
P <- cor(X)
Z <- scale(X)

cov(Z)

# Visualização
X
S
P
Z

# Número de observações
n <- nrow(X)

# Centralização
xc <- scale(X, center = TRUE, scale = FALSE)
xc

# Gráficos
plot(Petal.Width ~ Petal.Length, data = X)
plot(Petal.Width ~ Petal.Length, data = xc)

# Conferindo a matriz de covariância via fórmula
a <- t(xc) %*% xc / (n - 1)
a

# Matriz diagonal dos desvios padrão
d.meio <- diag(sqrt(diag(S)))
d.meio

# Inversa
d.meio.inv <- diag(1 / diag(d.meio))
d.meio.inv

# Outra forma equivalente
diag(1 / sqrt(diag(S)))

# ---------------------------------------------------
# Apenas versicolor
Y <- iris[iris$Species == "versicolor", -5]

n2 <- nrow(Y)

# Centralização correta (antes estava usando X por engano)
yc <- scale(Y, center = TRUE, scale = FALSE)
yc

# Gráficos
plot(Petal.Width ~ Petal.Length, data = Y)
plot(Petal.Width ~ Petal.Length, data = yc)

# Vetores (setas)
arrows(x0 = 0, y0 = 0,
       x1 = yc[, 3], y1 = yc[, 4],
       angle = 15, length = 0.15,
       col = "#00000070")

#########################################################
############  AULA 09/04/2026 #######################

# Seleção de variáveis (Petal.Length e Petal.Width)
X <- iris[, c(3, 4)]

# Covariância
S <- cov(X)

# Centralização e padronização
Xc <- scale(X, scale = FALSE)
Xs <- scale(X)

# Matriz diagonal dos desvios padrão
V <- diag(sqrt(diag(S)))
V.inv <- diag(1 / sqrt(diag(S)))

# Gráfico padronizado
plot(Xs)

# Pontos selecionados
pontos <- c(44, 62, 123, 135)
limites <- c(-3, 3)

plot(Xs[pontos, ],
     col = as.numeric(iris$Species[pontos]),
     xlim = limites, ylim = limites, asp = 1)

abline(h = 0, v = 0, lty = 2)
abline(a = 0, b = 1, col = "red")
abline(a = 0, b = -1, col = "red")

arrows(x0 = 0, y0 = 0,
       x1 = Xs[pontos, 1],
       y1 = Xs[pontos, 2],
       angle = 15)

text(Xs[pontos, ], labels = pontos)

# Matriz de rotação (45 graus)
W <- matrix(c(1, 1, -1, 1), ncol = 2, byrow = FALSE) * (sqrt(2) / 2)

# Transformação de um ponto
Xc[44, ] %*% W

# Norma do vetor
sqrt(sum(Xc[44, ]^2))

# Projeção em e1
-0.3084222 * c(0.7071068, -0.7071068)


