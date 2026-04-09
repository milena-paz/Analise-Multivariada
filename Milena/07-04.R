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

##

x<- iris[,c(3,4)]
S<- cov(x)
Xc<- scale(x,scale=F) #centrados
Xs <- scale(x) #padronizados
# Matriz diagonal dos desvios padrão
V <- diag(sqrt(diag(S)))
V.inv <- diag(1/sqrt(diag(S)))

par(mar=c(4,4,2,1))
plot(Petal.Width ~ Petal.Length,data=Xs)

identify(x=Xs[,1],y=Xs[,2])

pontos<- c(44,62,135,123)

plot(Xs[pontos,1],Xs[pontos,2],col="hotpink",pch=19)

#rotacionando os pontos em 45°
#matriz de rotacao
r<-1/4
W <- matrix(c(cospi(r),sinpi(r),-sinpi(r),cospi(r)),nrow=2)
par(mar=c(2,2,2,1),mfrow=c(2,1))
#rotacionado
Xs.rot <- (Xs%*%W)
plot(Xs.rot[pontos,1],Xs.rot[pontos,2],col="hotpink",pch=19,asp=1,xlim=c(-3,3),ylim=c(-3,3),main="Rotacionado")
abline(a=0,b=1,col="red")
abline(a=0,b=-1,col="red")
#regular
plot(Xs[pontos,1],Xs[pontos,2],col="hotpink",pch=19,asp=1,xlim=c(-3,3),ylim=c(-3,3),main="Regular")
abline(a=0,b=1,col="red")
abline(a=0,b=-1,col="red")

#Rotacionando com os dados centrados
Xc.rot <- Xc%*%W

norm(as.matrix(Xc[44,]),type="e")

#projecao de Xc[44,] em e1 e e2
proj<-as.vector(Xc[44,]%*%W[,1])*W[,1]
proj2<-as.vector(Xc[44,]%*%W[,2])*W[,2]

par(mfrow=c(1,1))
plot(Xc[44,1],Xc[44,2],asp=1,xlim=c(-3,1),ylim=c(-1,1),pch=19,col="hotpink",bty="n")
arrows(0,0,Xc[44,1],Xc[44,2], length=.1)
abline(a=0,b=1,lty=2,col="blue")
abline(a=0,b=-1,lty=2,col="blue")
arrows(0,0,proj[1],proj[2],col="red",length=.1)
arrows(0,0,proj2[1],proj2[2],col="red",length=.1)
abline(h=0)
abline(v=0)
