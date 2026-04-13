x<- iris[,c(3,4)]
S<- cov(x)
Xc<- scale(x,scale=F) #centrados
Xs <- scale(x) #padronizados
# Matriz diagonal dos desvios padrão
V <- diag(sqrt(diag(S)))
V.inv <- diag(1/sqrt(diag(S)))

par(mar=c(4,4,2,1))
plot(Petal.Width ~ Petal.Length,data=Xs)

# identify(x=Xs[,1],y=Xs[,2])

pontos<- c(44,62,135,123)

plot(Xs[pontos,1],Xs[pontos,2],col="hotpink",pch=19)

### Rotacionando os pontos em 45° ###
#matriz de rotacao
W <- matrix(c(1,1,-1,1)/sqrt(2),nrow=2)
par(mar=c(2,2,2,1))
#rotacionado
Xs.rot <- Xs%*%t(W)
plot(Xs.rot[pontos,1],Xs.rot[pontos,2],col="hotpink",pch=19,asp=1,xlim=c(-3,3),ylim=c(-3,3),main="Pontos rotacionados 45° (anti-horário)")
abline(a=0,b=1,col="red")
abline(a=0,b=-1,col="red")
#regular
points(Xs[pontos,1],Xs[pontos,2],pch=19)
legend("topleft",col=c("black","hotpink"), legend=c("Original","Rotacionado"),pch=19)

#Rotacionando com os dados centrados
Xc.rot <- Xc%*%W

norm(as.matrix(Xc[44,]),type="e")

#projecao de Xc[44,] em e1 e e2
proj<-as.vector(Xc[44,]%*%W[,1])*W[,1]
proj2<-as.vector(Xc[44,]%*%W[,2])*W[,2]

plot(Xc[44,1],Xc[44,2],asp=1,xlim=c(-3,1),ylim=c(-1,1),pch=19,cex=2,col="hotpink",bty="n")
arrows(0,0,Xc[44,1],Xc[44,2], length=.1,lwd=2)
abline(a=0,b=1,lty=2,col="blue")
abline(a=0,b=-1,lty=2,col="blue")
arrows(0,0,proj[1],proj[2],col="red",length=.1,lwd=2)
arrows(0,0,proj2[1],proj2[2],col="red",length=.1,lwd=2)
abline(h=0)
abline(v=0)
segments(c(proj[1],proj2[1]),c(proj[2],proj2[2]),Xc[44,1],Xc[44,2],lty=3,col="grey30")
