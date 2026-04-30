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
plot(Y[,1:2],pch=21,bg=paste0(cores[as.numeric(iris$Species)],"B0"),
     col="#000000B0",xlab="Componente 1",ylab="Componente 2",bty="n")
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
cor.YX<-V.meioinv%*%E%*%Lambda.meio #no quadro: R

#quantos % da variável está em cada componente?
cor.YX^2*100

###30/04###
perc <- lambda/sum(lambda)*100
acum <- cumsum(perc)
tabela.val <- cbind(round(lambda,3),round(perc,2),round(acum,1))
rownames(tabela.val) <- paste0("CP",1:4)
colnames(tabela.val)  <- c("Autovalor","% Variância","% Acumulada")
tabela.val

## coeficiente das componentes
#E <- iris.av$vectors
colnames(E) = paste("Comp.",1:4)
rownames(E) = names(iris[-5])
E
E2 <- E[,1:2]

colSums(cor.YX**2)
corrplot::corrplot(cor.YX,is.corr=T)

L<- E%*%Lambda.meio #matriz de loadings
#S=L%*%t(L)
colnames(L) = paste("Comp.",1:4)
rownames(L) = names(iris[-5])
L
#correlacao entre componentes e variaveis em termos de matriz de loadings
V.meioinv%*%L
colSums(L**2) # = lambda
rowSums(L**2) # = diag(S) (variancias das variaveis)
corrplot::corrplot(L**2,is.corr=F)

# contribuicao das variaveis em cada componente:%

rbind(contribuicao <- E^2*100,
      Total = apply(E**2,2,sum))
ordem <- order(contribuicao[,1],decreasing=T)
barplot(contribuicao[ordem,1])
#qualidade dos componentes: cos^22
cos2 <- cor.YX**2
cos2
rowSums(cos2)
colSums(cos2)

##
cos2.duas <- rowSums(cos2[,1:2])
cos2.duas
ordem <- order(cos2.duas,decreasing=T)
barplot(cos2.duas[ordem],names.arg=names(iris[-5]),cex.names=0.85,
        ylab="Cos - Qualidade de representação",
        main="Cos2 das variáveis para CP1 e CP2",
        col=RColorBrewer::brewer.pal(4,"Dark2"))
## ACP - Variáveis
plot(0,type="n",xlim=c(-1,1),ylim=c(-1,1),asp=1,
     main="ACP - Variáveis",xlab=paste0("CP1(",tabela.val[1,2],")%"),
     ylab=paste0("CP2(",tabela.val[2,2],")%"))
abline(h=0,v=0,lty=2)
lines(cos(seq(0,2,.01)*pi),sin(seq(0,2,.01)*pi))
arrows(0,0,cos2[,1],cos2[,2],length=.15,angle=20,col="#F20000")
text(cos2[,1],cos2[,2],labels=c("SL","SW","PL","PW"),cex=.7,pos=c(4,4,4,2))

#ESCORES DAS OBSERVACOES
Y
dim(Y)
head(Y) 
apply(Y,2,function(x)
  c(media=mean(x),dp=sd(x),variancia=var(x)))

#Y.media <- t(E)%*%colMeans(x)%*%E

library(RColorBrewer)
cores <- brewer.pal(3,"Set2")
plot(Y[,1:2],pch=21,bg=paste0(cores[as.numeric(iris$Species)],"B0"),
     col="#000000B0",xlab="Componente 1",ylab="Componente 2",bty="n")
abline(h=centroide[2],v=centroide[1],lty=2)
legend("topright",legend=levels(iris$Species),col=cores,pch=19)

###RECONSTITUICAO DE S
S <- cov(x)
S.r <- E%*%Lambda%*%t(E)
S.r - S
sum((S.r-S)**2)
S1 <- L[,1]%*%t(L[,1])
S1 |> diag()|> sum() #variancia da primeira componente
S2 <- L[,1:2]%*%t(L[,1:2])
S2 |> diag()|> sum() #variancia da segunda componente
sum((S2-S)**2)
rowSums(L[,1:2]**2)#comunalidade
rowSums(L**2)
rowSums(L**2) - rowSums(L[,1:2]**2) #especificidade

