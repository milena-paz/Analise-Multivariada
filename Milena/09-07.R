data("Default",package="ISLR")
sum(is.na(Default))
summary(Default)
#taxa de inadimplencia global: 3.33%
(tabela=table(Default[2:1]))
prop.table(tabela,1)
tabe
#NUMERICAS
hist(Default$balance,freq=F)
sum(Default$balance==0)
#parece haver massa em 0!
par(mar=c(4.1,4.1,1,1))
cores=c("#00000088","red")
plot(income~balance,data=Default,cex=0.5,col=cores[as.numeric(Default$default=="Yes")+1])
plot(income~balance,data=Default,cex=0.5,col=cores[as.numeric(Default$student=="Yes")+1])
boxplot(balance~student,data=Default,col=c("orange","#1f1fAF"))
boxplot(income~student,data=Default,col=c("salmon2","lightblue"))
#DEFININDO AMOSTRA DE TREINO E DE VALIDACAO
ene <- dim(Default)[1]
set.seed(666)
ind <- sample(2, ene, replace = TRUE, prob = c(0.6, 0.4))
treino <- Default[ind == 1, ]
teste <- Default[ind == 2, ]

rbind(table(treino[1]),table(teste[1]))
mod.mL0 <- MASS::lda(default ~ student + balance + income, data = treino)
mod.mL0$prior; mod.mL0$scaling
#se o escore for alto o modelo predirá que o indivíduo é inadimplente
predL0 <- predict(mod.mL0, newdata = teste)
(confusao.L0 <- table(Predicao=predL0$class, Referencia=teste$default))
#acuracia
ac=sum(diag(confusao.L0)/sum(confusao.L0))
#taxa de erro global
1-ac
#especificidade
confusao.L0[1,1]/sum(confusao.L0[,1])
#sensibilidade
confusao.L0[2,2]/sum(confusao.L0[,2])
###MODELO2
mod.mL2 <- MASS::lda(default ~ balance + student, data = treino)
mod.mL2$prior; mod.mL2$scaling

predL2 <- predict(mod.mL2, newdata = teste)
(confusao.L2 <- table(Predicao=predL2$class, Referencia=teste$default))
#acuracia
ac=sum(diag(confusao.L2)/sum(confusao.L2))
#taxa de erro global
1-ac
#especificidade
confusao.L2[1,1]/sum(confusao.L2[,1])
#sensibilidade
confusao.L2[2,2]/sum(confusao.L0[,2])
#CENTROIDES
mod.mL2$means
#PACOTE CARET
confusao.carL2 <- caret::confusionMatrix(data = predL2$class, reference = teste$default, positive = "Yes")
confusao.carL2$table
confusao.carL2
#REFAZENDO A PREDIÇÃO
limite <- 0.40
predL2.40 <- predL2$posterior[, "Yes"] > limite
referencia <- teste$default == "Yes"
(confusao.40 <- table(predicao=predL2.40,referencia))
#acuracia
ac=sum(diag(confusao.40)/sum(confusao.40))
#taxa de erro global
1-ac
#especificidade
confusao.40[1,1]/sum(confusao.40[,1])
#sensibilidade
confusao.40[2,2]/sum(confusao.40[,2])
