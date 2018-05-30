#____________________________________________________CODE R 


#_____________________________installation des packages

#install.packages("tree")
#install.packages("useful")
#install.packages("randomForest")
#install.packages("corrplot")
#install.packages("AER")
#install.packages("boot")
#install.packages("MASS")
#install.packages("Metrics")
#install.packages("xgboost")
#install.packages("kknn")
#install.packages('rattle')

library(tree)
library(randomForest)
library(FactoMineR)
library(corrplot)
library(AER)
library(boot)
library(MASS)
library(Metrics)
library(base)
library(xgboost)
library(FNN)
library(kknn)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)


#_____________________________suppression des variables en memoire
rm(list=ls())

#_____________________________importation de la bdd 

BDD<-read.csv("C:/Users/asus/Downloads/data_origine.csv",sep=";",header=T)
#creation des variables day, month, year, hour a partir de la variable datetime
datetime=strptime(as.character(BDD$datetime), format="%d/%m/%Y %H:%M")
day<-format(datetime, format = "%d")
month<-format(datetime, format = "%m")
year<-format(datetime, format = "%Y")
hour<-format(datetime, format = "%H")
bdd<-cbind(day,month,year,hour,BDD[,2:12])


#_____________________________PARTIE 1 statistiques descriptives

#commentaire :  certains graphiques ont été tracés sur tableau software

#-----------------------------plot de la serie temporelle (une semaine)
plot(datetime[1:168],bdd$count[1:168],type="l")
plot(bdd$count[1:168],type="l")
lines(bdd$casual[1:168],col="red")
lines(bdd$registered[1:168],col="green")

#-----------------------------statistiques univariees
names(BDD)
str(BDD)
summary(BDD)
#commentaire : le min pour la variable count est 1, min et médiane differe beaucoup pour casual, registerd et count
table(day)
table(month)
table(year)
#commentaire : en observant les tableaux de contingence des variables dates nous détectons le fait qu il y a des donnees manquantes
table(bdd$season)
table(bdd$holiday)
table(bdd$workingday)
table(bdd$weather) 
#commentaire : nous avons une seule ligne présentant la modalité 'fortes averses ou neige', en majorité 'dégagé à nuageux'

#donnees manquantes
2*12*19*24
#=10944
length(bdd[,1])
10944-length(bdd[,1])
#58 lignes du jeu de donnés sont manquantes
#nous ne mettons pas en place de méthode d'imputation des données car les données manquantes restent marginales et nous avons seulement des informations sur datetime pour ces lignes

#boxplot casual et registered
boxplot(bdd[,13:14], main="boxplot")
outliersC<-boxplot(bdd[,13], main="boxplot")$out
outliersR<-boxplot(bdd[,14], main="boxplot")$out
#histogramme casual et registered
par(mfrow=c(1,2))
hist(bdd$casual,freq=FALSE,col="lightblue",main="casual",breaks=20)
hist(bdd$registered,freq=FALSE,col="lightblue",main="registered",breaks=20)
par(mfrow=c(1,1))
#log-transformation
par(mfrow=c(1,2))
hist(log(bdd$casual),freq=FALSE,col="lightblue",main="log casual",breaks=20)
hist(log(bdd$registered),freq=FALSE,col="lightblue",main="log registered",breaks=20)
par(mfrow=c(1,1))



#commentaire : ressemble à l'histogramme d'une loi de poisson
hist(rpois(10000,1),breaks=10)

#moyenne,variance casual et registered
mean(bdd$casual)
var(bdd$casual)
mean(bdd$registered)
var(bdd$registered)

#commentaire : une regression lineaire semble mal adaptee


#boxplot et histogramme variables explicatives continues

hist(bdd$humidity)

hist(bdd$workingday)
hist(bdd$temp)
hist(bdd$atemp)
hist(data$windspeed)

boxplot(bdd$humidity)
boxplot(bdd$temp)
boxplot(bdd$atemp)
boxplot(data$windspeed)




#-----------------------------statistiques multivariees
#correlation entre toutes les variables
library(corrplot)
bdd_cor<-cbind(as.numeric(day),as.numeric(month),as.numeric(year),as.numeric(hour),BDD[,2:12])
mcor <- cor(as.matrix(bdd_cor))
mcor
corrplot(mcor, tl.cex=0.5 ,type="upper", order="hclust", tl.col="black", tl.srt=45)   #Correlation postive en bleu, negative en rouge
corrplot(mcor, tl.cex=0.3, method = "number", type = "upper")

#scatterplot
#pairs(dat[,8:13])

#boxplot en fonction des modalités des variables factor

boxplot(bdd$count~bdd$year,xlab="year", ylab="count of users")
boxplot(bdd$count~bdd$hour,xlab="hour", ylab="count of users")
boxplot(bdd$count~bdd$day,xlab="day", ylab="count of users")
boxplot(bdd$count~bdd$month,xlab="month", ylab="count of users")
boxplot(bdd$count~bdd$season,xlab="season", ylab="count of users")
boxplot(bdd$count~bdd$weather,xlab="weather", ylab="count of users")
boxplot(bdd$count~bdd$holiday,xlab="holiday", ylab="count of users")
boxplot(bdd$count~bdd$workingday,xlab="workingday", ylab="count of users")

boxplot(bdd$casual~bdd$year,xlab="year", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$hour,xlab="hour", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$day,xlab="day", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$month,xlab="month", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$season,xlab="season", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$weather,xlab="weather", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$holiday,xlab="holiday", ylab="utilisateur non abonné")
boxplot(bdd$casual~bdd$workingday,xlab="workingday", ylab="utilisateur non abonné")

boxplot(bdd$casual~bdd$year,xlab="year", ylab="utilisateur non abonné")
boxplot(bdd$registered~bdd$hour,xlab="hour", ylab="utilisateur abonné")
boxplot(bdd$registered~bdd$day,xlab="day", ylab="utilisateur abonné")
boxplot(bdd$registered~bdd$month,xlab="month", ylab="utilisateur abonné")
boxplot(bdd$registered~bdd$season,xlab="season", ylab="utilisateur abonné")
boxplot(bdd$registered~bdd$weather,xlab="weather", ylab="utilisateur abonné")
boxplot(bdd$registered~bdd$holiday,xlab="holiday", ylab="utilisateur abonné")
boxplot(bdd$registered~bdd$workingday,xlab="workingday", ylab="utilisateur abonné")













#_____________________________PARTIE 2 MACHINE LEARNING

#definition des variables 'factor'
bdd$season=as.factor(bdd$season)
bdd$weather=as.factor(bdd$weather)
bdd$holiday=as.factor(bdd$holiday)
bdd$workingday=as.factor(bdd$workingday)

bdd$casual<-log(bdd$casual+1)
bdd$registered<-log(bdd$registered+1)
bdd$count<-log(bdd$count+1)


#_____________________________definition de l'echantillon d'apprentissage et de test
bdd$day<-as.numeric(bdd$day)
# base d'apprentissage et de test (test du modele pour eviter le sur-apprentissage)
train<-subset(bdd,day<=14)
test<-subset(bdd,day>15)
# seconde base d'apprentissage et de validation (validation du modele pour choisir les parametres)
subtrain<-subset(train,day<=11)
val<-subset(train,day>11)




#_____________________________definition bdd

#definition de la bdd pour le modele casual
datC<-train[,c(2,3,4,5,6,7,8,9,10,11,12,13)]
subtrainC<-subtrain[,c(2,3,4,5,6,7,8,9,10,11,12,13)]
valC<-val[,c(2,3,4,5,6,7,8,9,10,11,12,13)]
testC<-test[,c(2,3,4,5,6,7,8,9,10,11,12,13)]


#_____________________________modele 1 : regression lineaire

#_____________________________modele pour casual

reg1<-lm(casual~month+year+hour+holiday+workingday+weather+temp+atemp+humidity+windspeed+month*year+hour*workingday,datC)

reg2<-step(reg1,k=log(length(datC[1,]))) #procedure de selection de variable par comparaison du critere bic sur reg 1
                                         #aucune variable n'est supprimé

rmse(exp(predict(reg1,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(reg1,testC))-1,exp(testC$casual)-1)

reg11<-lm(casual~year+hour+holiday+workingday+weather+atemp+humidity+windspeed+month*year+hour*workingday,datC)
rmse(exp(predict(reg11,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(reg11,testC))-1,exp(testC$casual)-1)

summary(reg1)
anova(reg1)
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) 
plot(reg1, cex = 1 )
shapiro.test(reg1$residuals)
plot(reg1$fitted.values, reg1$residuals)
abline(h = 0, col = "darkgreen", lwd = 2)

histo <- hist(reg1$residuals, probability = TRUE)
ec_typ <- summary(reg1)$sigma
curve(dnorm(x, 0, ec_typ), from = min(histo$breaks), to = max(histo$breaks), 
      add = TRUE, type = "l", col = "magenta", lwd = 2)


   
#_____________________________modele 2 : knn

# Convertion des variables categorielles en variables binaires 
dmy <- dummyVars(" ~ .", data = subtrainC,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = subtrainC))
train_transformed2 <- data.frame(predict(dmy, newdata = datC))

dmy <- dummyVars(" ~ .", data = valC,fullRank = T)
val_transformed <- data.frame(predict(dmy, newdata = valC))
test_transformed2 <- data.frame(predict(dmy, newdata = testC))


# calcul de l'erreur sur la base de validation pour k=1:50
rmse = rep(0, 50)
for (k in 1:50){
cvpred_hold_out =  kknn(casual ~ ., train_transformed, val_transformed, na.action = na.omit(), 
                               k = k, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
                               contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))
  # misclassification rates for k=1:25
  rmse[k] = rmse(val_transformed$casual, cvpred_hold_out$fitted.values)
}
rmse = rmse/50
rmse

k_star = which.min(rmse)
k_star

plot(rmse, xlab = "k" , col="blue", main="RMSE en fonction de k")

# implémentation du modèle avec k* sur la base d'apprentissage
knn = kknn(casual ~ ., train_transformed2, test_transformed2, na.action = na.omit(), 
           k = k_star, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
           contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))

rmse(exp(knn$fitted.values)-1,exp(test_transformed2$casual)-1)
rmsle(exp(knn$fitted.values)-1,exp(test_transformed2$casual)-1)


#_____________________________modele 3 : arbre de regression
require(useful)

tree0 = rpart(casual ~., data = datC, method = "anova")
fancyRpartPlot(tree0)
rmse(exp(predict(tree0,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(tree0,testC))-1,exp(testC$casual)-1)

plotcp(tree0)
ptree<- prune(tree0, cp= tree0$cptable[which.min(tree0$cptable[,"xerror"]),"CP"])
#il ne semble pas nécessaire d'élager l'arbre
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")



#_____________________________modele 4 algorithme des forets aleatoires

#random forest

require(randomForest)
Forest<-randomForest(casual~., datC, importance= F, ntree=500)

rmse(exp(predict(Forest,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(Forest,testC))-1,exp(testC$casual)-1)


#_____________________________modele 5 : gradient boosting 

dtrain <- xgb.DMatrix(data = data.matrix(datC[,1:11]), label = datC[,12])
dsubtrain <- xgb.DMatrix(data = data.matrix(subtrainC[,1:11]), label = subtrainC[,12])
dval <- xgb.DMatrix(data = data.matrix(valC[,1:11]), label = valC[,12])
dtest<- xgb.DMatrix(data = data.matrix(testC[,1:11]), label = testC[,12])

#choix des paramètres
smallestError <- 100
for (depth in 1:10) { 
  for (rounds in 190:200) { 
      xgb <- xgboost(data = dsubtrain,max.depth=depth,eta = 0.1,nround=rounds,objective = "reg:linear", verbose=0)
      predictions <- predict(xgb,dval)
      err <- rmse(valC[,12], predictions)
    if ( err < smallestError) {
      smallestError = err
      print(paste(depth,rounds,smallestError))
    }  
  }
} 

parametres <- list(objective = "reg:linear", 
                   booster = "gbtree",
                   eval_metric = "rmse",
                   eta              = 0.05,
                   max_depth        = 5,                 
                   subsample        = 0.6,
                   colsample_bytree = 0.3,
                   num_parallel_tree = 10,
                   max_delta_step = 10)

xgb<- xgboost(data = dtrain, nrounds = 700, verbose = 1, params = parametres)
#xgb <- xgboost(data = dtrain,max.depth=depth,eta = 0.1,nround=rounds,objective = "reg:linear", verbose=0)

rmse(exp(predict(xgb,dtest))-1,exp(testC$casual)-1)
rmsle(exp(predict(xgb,dtest))-1,exp(testC$casual)-1)







#_____________________________modele 6 : regression de poisson  /!\ ne pas effectuer la transformation log

#commentaire : nous avons des v.a.r qui dénombrent le nombre d'occurrences dans un laps de temps donne => une regression de poisson semble adaptée a ce type de probleme
#Notons cependant que mean(count) et var(count) ne sont pas egaux => overdispersed poisson regression 

poisreg=glm(casual~month+year+hour+holiday+workingday+weather+atemp+humidity+windspeed+month*year+hour*workingday,datC,family=poisson(link=log))

poisreg2<-step(poisreg,k=log(length(datC[1,]))) #procedure de selection de variables par comparaison du critère aic
#aucune variable n'est supprimée

rmse(predict(poisreg,testC),testC$casual)
f<-function(x) {
  for(i in 1:length(x)) { if (x[i]<=0) x[i]<-0 }
  return(x)
}  
rmsle(f(predict(poisreg,testC)),testC$casual)

#test overdispersion
dispersiontest(poisreg,trafo=1,alternative="greater") #H0 equidispersion rejettee

#_____________________________'quasipoisson regression'
quasipoisreg<-glm(casual~month+year+hour+holiday+workingday+weather+atemp+temp+humidity+windspeed+month*year+hour*workingday,datC, family = quasipoisson)

rmse(predict(quasipoisreg,testC),testC$casual)
rmsle(predict(quasipoisreg,testC),testC$casual)

#_____________________________ regression binomiale negative pour casual
library(MASS)
negbinreg <- glm.nb(casual~month+year+hour+holiday+workingday+weather+temp+humidity+windspeed+month*year+hour*workingday,dat)

rmse(predict(quasipoisreg,testC),testC$casual)
rmsle(predict(quasipoisreg,testC),testC$casual)



#### CONCLUSION :
#mesure performance des différents modèles

rmse(exp(predict(reg1,testC))-1,exp(testC$casual)-1)
rmse(exp(predict(tree0,testC))-1,exp(testC$casual)-1)
rmse(exp(predict(Forest,testC))-1,exp(testC$casual)-1)
rmse(exp(predict(xgb,dtest))-1,exp(testC$casual)-1)

rmsle(exp(predict(reg2,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(tree0,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(Forest,testC))-1,exp(testC$casual)-1)
rmsle(exp(predict(xgb,dtest))-1,exp(testC$casual)-1)














################## CHANGEMENT DE LA VARIABLE D INTERET #######################


#definition de la bdd pour le modele registerd
datC<-train[,c(2,3,4,5,6,7,8,9,10,11,12,14)]
subtrainC<-subtrain[,c(2,3,4,5,6,7,8,9,10,11,12,14)]
valC<-val[,c(2,3,4,5,6,7,8,9,10,11,12,14)]
testC<-test[,c(2,3,4,5,6,7,8,9,10,11,12,14)]


#_____________________________modele 1 : regression lineaire
reg1<-lm(registered~month+year+hour+holiday+workingday+weather+temp+atemp+humidity+windspeed+month*year+hour*workingday,datC)
rmse(exp(predict(reg1,testC))-1,exp(testC$registered)-1)
rmsle(exp(predict(reg1,testC))-1,exp(testC$registered)-1)


#_____________________________modele 2 : knn
# Convertion des variables categorielles en variables binaires 
dmy <- dummyVars(" ~ .", data = subtrainC,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = subtrainC))
train_transformed2 <- data.frame(predict(dmy, newdata = datC))

dmy <- dummyVars(" ~ .", data = valC,fullRank = T)
val_transformed <- data.frame(predict(dmy, newdata = valC))
test_transformed2 <- data.frame(predict(dmy, newdata = testC))


# calcul de l'erreur sur la base de validation pour k=1:50
rmse = rep(0, 50)
for (k in 1:50){
  cvpred_hold_out =  kknn(registered ~ ., train_transformed, val_transformed, na.action = na.omit(), 
                          k = k, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
                          contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))
  # misclassification rates for k=1:25
  rmse[k] = rmse(val_transformed$registered, cvpred_hold_out$fitted.values)
}
rmse = rmse/50
rmse

k_star = which.min(rmse)
k_star

plot(rmse, xlab = "k" , col="blue", main="RMSE en fonction de k")

# implémentation du modèle avec k* sur la base d'apprentissage
knn = kknn(registered ~ ., train_transformed2, test_transformed2, na.action = na.omit(), 
           k = k_star, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
           contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))

rmse(exp(knn$fitted.values)-1,exp(test_transformed2$registered)-1)
rmsle(exp(knn$fitted.values)-1,exp(test_transformed2$registered)-1)


#_____________________________modele 3 : arbre de regression
require(useful)

tree0 = rpart(registered ~., data = datC, method = "anova")
fancyRpartPlot(tree0)
plotcp(tree0)
ptree<- prune(tree0, cp= tree0$cptable[which.min(tree0$cptable[,"xerror"]),"CP"])
#il ne semble pas nécessaire d'élager l'arbre
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")
rmse(exp(predict(tree0,testC))-1,exp(testC$registered)-1)
rmsle(exp(predict(tree0,testC))-1,exp(testC$registered)-1)


#_____________________________modele 4 algorithme des forets aleatoires

#random forest

require(randomForest)
Forest<-randomForest(registered~., datC, importance= F, ntree=500)

rmse(exp(predict(Forest,testC))-1,exp(testC$registered)-1)
rmsle(exp(predict(Forest,testC))-1,exp(testC$registered)-1)


#_____________________________modele 5 : gradient boosting 

dtrain <- xgb.DMatrix(data = data.matrix(datC[,1:11]), label = datC[,12])
dsubtrain <- xgb.DMatrix(data = data.matrix(subtrainC[,1:11]), label = subtrainC[,12])
dval <- xgb.DMatrix(data = data.matrix(valC[,1:11]), label = valC[,12])
dtest<- xgb.DMatrix(data = data.matrix(testC[,1:11]), label = testC[,12])

#choix des paramètres
smallestError <- 100
for (depth in 1:10) { 
  for (rounds in 190:200) { 
    xgb <- xgboost(data = dsubtrain,max.depth=depth,eta = 0.1,nround=rounds,objective = "reg:linear", verbose=0)
    predictions <- predict(xgb,dval)
    err <- rmse(valC[,12], predictions)
    if ( err < smallestError) {
      smallestError = err
      print(paste(depth,rounds,smallestError))
    }  
  }
} 

parametres <- list(objective = "reg:linear", 
                   booster = "gbtree",
                   eval_metric = "rmse",
                   eta              = 0.05,
                   max_depth        = 5,                 
                   subsample        = 0.6,
                   colsample_bytree = 0.3,
                   num_parallel_tree = 10,
                   max_delta_step = 10)

xgb<- xgboost(data = dtrain, nrounds = 700, verbose = 1, params = parametres)
#xgb <- xgboost(data = dtrain,max.depth=depth,eta = 0.1,nround=rounds,objective = "reg:linear", verbose=0)

rmse(exp(predict(xgb,dtest))-1,exp(testC$registered)-1)
rmsle(exp(predict(xgb,dtest))-1,exp(testC$registered)-1)

#_____________________________modele 6 : regression de poisson  /!\ ne pas effectuer la transformation log

#commentaire : nous avons des v.a.r qui dénombrent le nombre d'occurrences dans un laps de temps donne => une regression de poisson semble adaptée a ce type de probleme
#Notons cependant que mean(count) et var(count) ne sont pas egaux => overdispersed poisson regression 

poisreg=glm(registered~month+year+hour+holiday+workingday+weather+atemp+humidity+windspeed+month*year+hour*workingday,datC,family=poisson(link=log))
poisreg2<-step(poisreg,k=log(length(datC[1,]))) #procedure de selection de variables par comparaison du critère aic
#aucune variable n'est supprimée

rmse(predict(poisreg,testC),testC$registered)
f<-function(x) {
  for(i in 1:length(x)) { if (x[i]<=0) x[i]<-0 }
  return(x)
}  
rmsle(f(predict(poisreg,testC)),testC$registered)

#test overdispersion
dispersiontest(poisreg,trafo=1,alternative="greater") #H0 equidispersion rejettee

#_____________________________'quasipoisson regression'
quasipoisreg<-glm(registered~month+year+hour+holiday+workingday+weather+atemp+temp+humidity+windspeed+month*year+hour*workingday,datC, family = quasipoisson)

rmse(predict(quasipoisreg,testC),testC$registered)
rmsle(predict(quasipoisreg,testC),testC$registered)

#_____________________________ regression binomiale negative
library(MASS)
negbinreg <- glm.nb(registered~month+year+hour+holiday+workingday+weather+temp+humidity+windspeed+month*year+hour*workingday,dat)

rmse(predict(quasipoisreg,testC),testC$registered)
rmsle(predict(quasipoisreg,testC),testC$registered)






#### CONCLUSION 2 :
#mesure performance des différents modèles pour la variable registered

rmse(exp(predict(reg1,testC))-1,exp(testC$registered)-1)
rmse(exp(predict(tree0,testC))-1,exp(testC$registered)-1)
rmse(exp(predict(Forest,testC))-1,exp(testC$registered)-1)
rmse(exp(predict(xgb,dtest))-1,exp(testC$registered)-1)
rmse(exp(knn$fitted.values)-1,exp(test_transformed2$registered)-1)

rmsle(exp(predict(reg1,testC))-1,exp(testC$registered)-1)
rmsle(exp(predict(tree0,testC))-1,exp(testC$registered)-1)
rmsle(exp(predict(Forest,testC))-1,exp(testC$registered)-1)
rmsle(exp(predict(xgb,dtest))-1,exp(testC$registered)-1)
rmsle(exp(knn$fitted.values)-1,exp(test_transformed2$registered)-1)











#trash

#sans transformation log
rmse(predict(reg1,testC),testC$casual)
rmse(predict(tree_model,testC),testC$casual)
rmse(predict(Forest,testC),testC$casual)
rmse(predict(xgb,dtest),testC$casual)

rmsle(predict(reg2,testC),testC$casual)
rmsle(predict(tree_model,testC),testC$casual)
rmsle(predict(Forest,testC),testC$casual)
rmsle(predict(xgb,dtest),testC$casual)
