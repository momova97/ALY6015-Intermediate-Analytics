print('Mohammad Hossein Movahedi')
print('R practice 3')

#installing packages and loading them

install.packages("magrittr")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggvis")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("psych")
install.packages('caret')
install.packages('ggcorrplot')
install.packages('InformationValue')

library(ggcorrplot)
library(data.table)
library(FSA)
library(magrittr)
library(dplyr)
library(plyr)
library(tidyverse)
library(gmodels)
library(ggvis)
library(ggplot2)
library(psych)
library(corrplot)
library(pROC)
library(ISLR)
library(caret)
library( ggplot2)
library(gridExtra)
library(InformationValue)


# Importing the dataset
data(College)


# Exploratory Data Analysis
str(College) 
summary(College)
data <- as.data.frame(College)
#replacing blank data with null
data <- data %>% mutate_all(na_if,"")
#box plot
dev.off()
boxplot(data)
#creating some plots
p1 <- ggplot(data, aes(Accept,Enroll)) + geom_point() + theme_bw()
ggMarginal(p1)

p2 <- ggplot(data, aes(F.Undergrad, P.Undergrad, colour = Private)) +
  geom_point()
ggMarginal(p2, groupColour = TRUE, groupFill = TRUE)
 
# Hist for Apps
hist(data$Apps, xlim = c(0, 50000),col = "green", xlab="car(year)")
# Box plot
 y<- qplot(x=data$Private,y=data$Top10perc, fill=data$Private,geom='boxplot')+guides(scale= "none")
 z<-qplot(x=data$Private,y=data$Top25perc, fill=data$Private,geom='boxplot')+guides(scale= "none")
  grid.arrange(y,z,nrow=1)
  
# split data to train and test
set.seed(910198135)
trainIndex<- createDataPartition(College$Private,p=0.70,list=FALSE)
train<-College[trainIndex,]
test<-College[-trainIndex,]

  
  
  
#Fit model on train data
model1<-glm(Private~.,data=train,family=binomial(link="logit"))
summary(model1)
modelpfo<-glm(Private~F.Undergrad+Outstate,data=train, family=binomial(link="logit"))
summary(modelpfo)
#creating confusion matrix
#use model to predict probability of train
predicted <- predict(modelpfo, train, type="response")
#convert Private from "Yes" and "No" to 1's and 0's
train$Private <- ifelse(train$Private=="Yes", 1, 0)
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(train$Private, predicted)[1]
#create confusion matrix
confusionMatrix(train$Private, predicted)

#calculate sensitivity
sensitivity(test$Private, predicted)
#calculate specificity
specificity(test$Private, predicted)
#calculate total misclassification error rate
misClassError(test$Private, predicted, threshold=optimal)

#Accuracy=(IN+TP)/(TN+FP+FN+TP)
(131+384)/(545)
#Precision=TP/(FP+TP)
131/(131+12)
#Recall=TP/(TP+FN)
131/(131+18)

#testing
#use model to predict probability of test
predicted <- predict(modelpfo, test, type="response")
#convert Private from "Yes" and "No" to 1's and 0's
test$Private <- ifelse(test$Private=="Yes", 1, 0)
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$Private, predicted)[1]
#create confusion matrix
confusionMatrix(test$Private, predicted)

#calculate sensitivity
sensitivity(test$Private, predicted)
#calculate specificity
specificity(test$Private, predicted)
#calculate total misclassification error rate
misClassError(test$Private, predicted, threshold=optimal)

## Test set predictions
probabilities.test<-predict(model1,newdata=test,type='response')
predicted.classes.min<-as.factor(ifelse(probabilities.test>=optimal, "Yes","No"))

#Plot the receiver operator characteristic curve
ROC1<-roc(test$Private,probabilities.test)

plot(ROC1,col="blue",ylab="Sensitivity-TP Rate", xlab='Specifity-FP Rate')

#Calculate the area under the ROC curve
auc<-auc(ROC1)
auc
