
#Executer le script "requirements.R"

library(disk.frame)
library(parallel)
library(dplyr)
library(ggplot2)
library(xgboost)


#Configuration
nCores <- detectCores()    
setup_disk.frame(workers = nCores)
options(future.globals.maxSize = Inf)

load("train.csv")
set.seed(777)

<<<<<<< HEAD
         #Exploration des donnees--------
=======
#Exploration des donnees--------
>>>>>>> 547a4deb3e74def35e8da42f9aadd8cd08a7f988


head(train)
train$amount <- as.integer(train$amount)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$isFraud <- as.factor(train$isFraud)
train$isFlaggedFraud <- as.factor(train$isFlaggedFraud)
train$type<-as.factor(train$type)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)


object.size(train)

#Afficher les modalités et leurs effectifs

str(train)

#Transtypage de colonnes, afin de réduire l'espace du df et mettre un type correspondant

train$amount <- as.integer(train$amount)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.factor(train$isFraud)
train$isFlaggedFraud <- as.factor(train$isFlaggedFraud)
train$type<-as.factor(train$type)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)


#Verification sur les noms (nameOrig et nameDest)

table(train$nameDest)
table(train$nameOrig)

#-----------------------------------------#


#Nous allons continuer notre exploration des données

sum(is.na(train))
#Le jeu de données n'as pas de valeurs manquantes

#Histogramme Fraudes et Non-Fraudes

plot(train$isFraud, 
     main = "Taux de fraudes et non-fraudes", 
     sub = "0=Non-Fraude   1=Fraude")
<<<<<<< HEAD
  
=======

>>>>>>> 547a4deb3e74def35e8da42f9aadd8cd08a7f988
#Le jeu de données est déséquilibré, il faudra donc utilisé des modèles adaptés (ex: arbres de décision, xgboost...)

#--------------------------------------#

#On va voir comment sont reparties les données en fonction si c'est des fraudes ou pas
#pour cela, nous allons créer des sous jeux de données

fraude_f = train[train$isFraud == 0,] 
fraude_t = train[train$isFraud == 1,] 


#--------------------------------------#

#Temps des transactions

ggplot()+geom_density(data = fraude_t,aes(x = fraude_t$step),color="red",
                      fill="red",alpha=0.5)+
  geom_density(data = fraude_f,aes(x = fraude_f$step),color="blue",fill="blue",
               alpha=0.55)+
  ggtitle("Comparaison temps des transactiosn frauduleuses ou pas")+
  xlab("Step")+
  ylab("Densite")

#-----------------------------#

#Montant des transactions

#Comparaison des 2
ggplot()+geom_density(data = fraude_t,aes(x = fraude_t$amount),color="red",
                      fill="red",alpha=0.2)+
  geom_density(data = fraude_f,aes(x = fraude_f$amount),color="blue",fill="blue",
               alpha=0.5)+
  ggtitle("Comparaison du montant des transactions frauduleuses ou pas")+
  xlab("Amount")+
  ylab("Densité") 

#Montant des transactions frauduleuses
ggplot()+geom_density(data = fraude_t,aes(x = fraude_t$amount),color="red",
                      fill="red",alpha=0.2)

#Montant des transactions normales
ggplot()+geom_density(data = fraude_f,aes(x = fraude_f$amount),color="blue",fill="blue",
                      alpha=0.5)

#------------------------------#

#Matrice de correlation entre les variables

ggcorr(train,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "gray50")

#------------------------------#

#Les types de transaction

table(train$type) 

#Types de transactions 


ggplot(train, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count',
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()


plot(typ_fraude_f, col = "blue",
     main = "Types des transactions normales")

plot(typ_fraude_t, col = "red",
     main = "Types des transactions frauduleuses")

table(typ_fraude_t)


train = subset(train, type == "CASH_OUT" | type == "TRANSFER" )


