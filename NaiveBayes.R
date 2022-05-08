install.packages("e1071")
install.packages("caTools")
install.packages("caret")

library(e1071)
library(caTools)
library(caret)

data(iris)
split <- sample.split(iris,SplitRatio = 0.7)
train <- subset(iris,split==T)
test <- subset(iris,split==F)
train

train_scale <- scale(train[,1:4])
test_scale <- scale(test[,1:4])
train_scale

#Fitting Naive Bayes
set.seed(123)
classifier <- naiveBayes(Species~.,data=train)
classifier

pred <- predict(classifier,newdata=test)
pred

cm <- table(test$Species,pred)
cm

confusionMatrix(cm)

install.packages("caret")
install.packages("caretEnsemble")
install.packages("tidyverse")
install.packages("psych")
install.packages("Amelia")
install.packages("mice",INSTALL_opts = '--no-lock')
install.packages("GGally")
install.packages("randomForest")
install.packages("rpart")
library(caretEnsemble)
library(tidyverse)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(randomForest)
library(rpart)
library(ggplot2)
library(party)
library(caret)

data <- read.csv("/cloud/project/Karthikeyan Workspace/diabetes_csv.csv")

View(data)
str(data)
data$class <- as.factor(data$class)
data$class <- factor(data$class,labels =c("F","T"))


#convert 0 to NA
data[,2:7][data[,2:7]==0] <- NA

missmap(data)
mice_mod <- mice(data[,c("plas","pres","skin","insu","mass")])
mice_complete <- complete(mice_mod)
methods(mice)
data$plas <- mice_complete$plas
data$pres <- mice_complete$pres
data$skin <- mice_complete$skin
data$insu <- mice_complete$insu
data$mass <- mice_complete$mass
missmap(data)

#Age
ggplot(data,aes(age,colour=class))+
  geom_freqpoly(binwidth=1)+labs(title="Age distribution by class")

#Preg
c <- ggplot(data,aes(x=preg,fill=class,colour=class))+
    geom_histogram(binwidth=1)+labs(title="Pregnancy distribution by class")      
c+theme_bw() 

#Mass or BMI
c <- ggplot(data,aes(x=mass,fill=class,colour=class))+
  geom_histogram(binwidth=1)+labs(title="BMI distribution by class")
c+theme_bw() 

#Glucose
ggplot(data,aes(plas,colour=class))+
  geom_freqpoly(binwidth=1)+labs(title="Glucose distribution by class")
colnames(data)

ggpairs(data)

indxtrain <- createDataPartition(y=data$class,list=F,p=0.75)
training <- data[indxtrain,]
testing <- data[-indxtrain,]
prop.table(table(data$class))*100

prop.table(table(training$class))*100
prop.table(table(testing$class))*100

x = training[,-9]
y =training$class

library(e1071)

model = train(x,y,'nb',trcontrol=trainControl(method = 'cv',number=10))
model

pre_1 <- predict(model,newdata = testing)

x= varImp(model)
plot(x)

