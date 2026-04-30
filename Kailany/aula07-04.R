X <- iris[-5]
S <- cov(X)
P <- cor(X)
Z <- scale (X)
cov(Z)
X
S
P
Z
n <-  nrow(X)
xc <- scale(X, center= T, scale= F)
xc
plot(Petal.Width ~ Petal.Length, data= X)
plot(Petal.Width ~ Petal.Length, data= xc)
a <- t(xc) %*% xc / (n - 1)
a
d.meio <-  diag(sqrt(diag(S)))
d.meio

d.meio.inv = diag(1/diag(d.meio))
d.meio.inv
diag(1/sqrt(diag(S)))

# versicolor
Y <- iris[iris$Species == "versicolor", -5]
Y

n2 <- nrow(Y)
yc <- scale(X, center= T, scale= F)
yc

plot(Petal.Width ~ Petal.Length, data= Y)
plot(Petal.Width ~ Petal.Length, data= yc)
arrows (x0= 0, y0=0, x1=yc[,3], y1 = yc[,4], angle= 15, length = 0.15, col= "#00000070")


#########################################################
############  AULA 09/04/2026 #######################

X <- iris[c(3,4)]

S <- cov(x)
Xc <- scale(X, scale=F)
Xs <- scale(x)

# matriz diagonal dos desvios padrao

V <- diag(sqrt(diag(S)))
V.inv <- diag(1/sqrt(diag(V)))
plot(Xs)

#identify(x= Xs[,1], y= Xs[,2])

pontos<- c(44,62,123,135)
limites <- c(-3,3)

plot(Xs[pontos,], col= as.numeric(iris$Species), xlim= limites, ylim= limites, asp = (1))
abline(h=0, v=0, lty=2)
abline(a=0, b=1, lty=1, col= "red")
abline(a=0, b=-1, lty=1, col= "red")
arrows(x0= 0, y0= 0, x1= Xs[pontos, 1], y1= Xs[pontos, 2], angle=15)
text(Xs[pontos], labels= pontos)

W <- matrix(c(1,1,-1,1), ncol=2, byrom=F)*(sqrt(2)/2)
Xc[44,]%*%W
sqrt(sum(Xc[44, ]**2))

#projeção em e1
-0.3084222*c(0.7071068, -0.7071068)

