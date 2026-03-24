iris<- iris[-5]
S<-cov(iris)
#variancia total
sum(diag(S))
#variancia generalizada
det(S)
#autovalores
iris.av<-eigen(S)

lambda <- iris.av$values
#observe que todos sao positivos: S é positiva definida!
sum(lambda) #igual ao traço de S
prod(lambda) #igual ao determinante de S
#variabilidade em cada lambda
lambda*100/sum(lambda)

E <- iris.av$vectors
E%*%t(E)
t(E)%*%E

Lambda <- diag(lambda)
E%*%Lambda%*%t(E) #é igual a S
#munhecamente
lambda[1]*E[,1]%*%t(E[,1])+lambda[2]*E[,2]%*%t(E[,2])+lambda[3]*E[,3]%*%t(E[,3])+lambda[4]*E[,4]%*%t(E[,4])