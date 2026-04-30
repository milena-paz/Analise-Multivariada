# ==============================
# DADOS
# ==============================
X <- iris[, c(3, 4)] # Petal.Length e Petal.Width
Xs <- scale(X) # dados padronizados

# ponto que você quer ilustrar
i <- 44
x <- Xs[i, ]

# matriz de rotação de 45 graus
W <- matrix(c(1, 1, -1, 1), ncol = 2, byrow = FALSE) * (sqrt(2) / 2)

# coordenadas do ponto no sistema rotacionado
z <- x %*% W

# vetores-base rotacionados
e1 <- c(1, 1) / sqrt(2)
e2 <- c(-1, 1) / sqrt(2)

# projeções do ponto nos novos eixos
proj1 <- as.numeric(z[1]) * e1
proj2 <- as.numeric(z[2]) * e2

# ==============================
# GRÁFICO
# ==============================
lim <- 3

plot(0, 0,
     xlim = c(-lim, lim), ylim = c(-lim, lim),
     asp = 1, pch = 16,
     xlab = "Petal.Length padronizado",
     ylab = "Petal.Width padronizado",
     main = paste("Rotação do ponto", i))

# eixos usuais
abline(h = 0, v = 0, lty = 2, col = "gray50")

# novos eixos (diagonais)
abline(a = 0, b = 1, lty = 2, col = "gray40")
abline(a = 0, b = -1, lty = 2, col = "gray40")

# ponto original
points(x[1], x[2], pch = 16, cex = 1.2, col = "blue")

# vetor original da origem até o ponto
arrows(0, 0, x[1], x[2],
       col = "blue", lwd = 2, length = 0.12)

# projeção no eixo e1
arrows(0, 0, proj1[1], proj1[2],
       col = "red", lwd = 2, length = 0.12)

# projeção complementar: de proj1 até o ponto original
arrows(proj1[1], proj1[2], x[1], x[2],
       col = "purple", lwd = 2, length = 0.12)

# origem
text(0.08, 0.12, "(0,0)", cex = 0.9)

# rótulos do ponto original
text(x[1], x[2] - 0.18,
     labels = paste0("x = (", round(x[1], 3), ", ", round(x[2], 3), ")"),
     col = "blue")

# rótulo da projeção em e1
text(proj1[1] - 0.2, proj1[2] - 0.15,
     labels = round(z[1], 3), col = "red")

# rótulo da projeção em e2
meio2 <- (proj1 + x) / 2
text(meio2[1] + 0.15, meio2[2],
     labels = round(z[2], 3), col = "purple")

# marcar base rotacionada
points(e1[1]*2.2, e1[2]*2.2, pch = 4, col = "red", cex = 1.5, lwd = 2)
points(e2[1]*2.2, e2[2]*2.2, pch = 4, col = "red", cex = 1.5, lwd = 2)

text(e1[1]*2.35, e1[2]*2.35, "e1*", col = "red")
text(e2[1]*2.35, e2[2]*2.35, "e2*", col = "red")

