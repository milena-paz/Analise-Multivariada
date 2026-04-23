##### PCA - analise de componentes principais #####
x<- as.matrix(iris[-5])
S= cov(x)
iris.av<-eigen(S)
iris.av$values |> sum() -> vtot #variancia total
sort(diag(S),decreasing=T)/vtot

#daqui temos que a primeira componente concentra 92% da variância do conjunto de dados
lambda <-iris.av$values
lambda/vtot 

E <- iris.av$vectors

plot(lambda, type="b",bg="black",pch=21)

plot(E[,1:2],pch=21,bg="#0033FF",xlim=c(-.8,1),ylim=c(-0.8,1),xlab="Componente 1",ylab="Componente 2",bty="n")
abline(h=0,v=0)
text(E[,1:2],labels=names(iris[-5]),pos=c(1,1,3,3),cex=0.85)

arrows(0,0,E[,1],E[,2],length=.1,angle=15,col="#FF1A00")
#aqui podemos ver quais variaveis mais participam de determinadas componentes

###### COMPONENTES PRINCIPAIS APLICADAS AOS DADOS ######
Y<- x%*%E
cores<- RColorBrewer::brewer.pal(3,"Set2")
plot(Y[,1:2],pch=21,bg=cores[as.numeric(iris$Species)],xlab="Componente 1",ylab="Componente 2",bty="n")
centroide <- apply(Y,2,mean)
abline(h=centroide[2],v=centroide[1],lty=2)
arrows(centroide[1],centroide[2],E[,1]+centroide[1],E[,2]+centroide[2],length=.1,angle=15,col="#FF1A00")

#correlacao entre a compontente Y1 e X3
E[3,1]*sqrt(lambda[1]/S[3,3])
E[2,2]*sqrt(lambda[2]/S[2,2])
#MATRICIALMENTE
V<- diag(diag(S))
V.meio <- sqrt(V)
V.meioinv <- diag(1/sqrt(diag(S)))
Lambda <- diag(lambda)
Lambda.meio <-diag(sqrt(lambda))
# matriz de correlacao entre Componente(i) e Variavel(k)
A<-V.meioinv%*%E%*%Lambda.meio

#quantos % da variável está em cada componente?
A^2*100
