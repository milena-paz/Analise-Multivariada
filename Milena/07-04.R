#07/04/26
### EXEMPLO IRIS ###

x <- iris[-5] #matriz de dados
S <- cov(x) # covariancia amostral
rho <- cor(x) # correlacao amostral

z <- scale(x) # x padronizada 
cov(z) # temos q cov(z) = rho
sum(diag(rho))

## Matriz centrada de X:
Xc <- scale(x,center=T,scale=F)
par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(Petal.Width ~ Petal.Length,data=x)
plot(Petal.Width ~ Petal.Length,data=Xc)

t(Xc)%*%Xc/(149) # S

# Seja A uma matriz retangular nxp
# A %*% t(A) é nxn simétrica e positiva definida
# t(A) %*% A é pxp, simétrica e positiva definida

### Matriz V^1/2 amostral
D.meio <-diag(sqrt(diag(S)))
D.meio.inv <- diag(1/diag(D.meio))

#entao:
rho <- D.meio.inv%*%S%*%D.meio.inv #é equivalente a cor(x) ou cov(z)

####

x<- iris[iris$Species=="versicolor",-5]

Xc <- scale(x,scale=F)

par(mfrow=c(1,1))
plot(Petal.Width ~ Petal.Length,data=Xc)
arrows(x0=0,y0=0,x1=Xc[,3],y1=Xc[,4],angle=15,length=0.15,col="#0000B060")
