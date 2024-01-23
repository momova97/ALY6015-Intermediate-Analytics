install.packages('FSA')
install.packages('FSAdata')
install.packages('magrittr')
install.packages('dplyr')
install.packages('tidyr')
install.packages('plyr')
install.packages('tidyverse')
install.packages('outliers')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('corrplot')

library(ggplot2)
library(outliers)
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(scales)
library(lubridate)
library(corrplot)
install.packages('caret')
library(caret)
install.packages('ggplot2')
library(ggplot2)
install.packages("glmnet")
install.packages('Metrics')
library(glmnet)
library(Metrics)
 
#load the data
library(ISLR)
data("College")
dataset <- as.data.frame(College)

#split the data
set.seed(96)
trainIndex<-sample(x = nrow(dataset) , size = nrow(dataset)*0.7)
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]

train_x <- model.matrix(Grad.Rate~.,train)[,-1]
test_x <- model.matrix(Grad.Rate~.,test)[,-1]

train_y <- train$Grad.Rate
test_y <- test$Grad.Rate

#finding value of lambda

#finding the best value of lambda
set.seed(96)
cv.lasso <- cv.glmnet(train_x,train_y,nfolds = 10)
plot(cv.lasso)

#object : min perdiction error
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)
#Ridge
#fit model based on lambda.min with Ridge regression
model.1se <- glmnet(train_x,train_y,alpha = 0,lambda = cv.lasso$lambda.1se)
model.1se
#display reg coeff
coef(model.1se)

#display coeff of train model with no regularization 
tm <- lm(Grad.Rate~.,data = train)
coef(tm)

#view RMSE of full model
preds.tm <- predict(tm,new = test)
full.rmse <-rmse(test$Grad.Rate,preds.tm)

#train set prediction
preds.train<- predict(model.1se,newx = train_x)
train.rmse <- rmse(train_y,preds.train)
#test set prediction
preds.test <- predict(model.1se,newx =test_x)
test.rmse <- rmse(test_y,preds.test)

#compering values of rmse
train.rmse
test.rmse
full.rmse
#lasso
#fit model based on lambda.min with lasso regression
model.1se <- glmnet(train_x,train_y,alpha = 1,lambda = cv.lasso$lambda.1se)
model.1se
#display reg coeff
coef(model.1se)

#display coeff of train model with no regularization 
tm <- lm(Grad.Rate~.,data = train)
coef(tm)

#view RMSE of full model
preds.tm <- predict(tm,new = test)
full.rmse <-rmse(test$Grad.Rate,preds.tm)

#train set prediction
preds.train<- predict(model.1se,newx = train_x)
train.rmse <- rmse(train_y,preds.train)
#test set prediction
preds.test <- predict(model.1se,newx =test_x)
test.rmse <- rmse(test_y,preds.test)

#compering values of rmse
train.rmse
test.rmse
full.rmse

# Forward selection method
tm <- lm(Grad.Rate~1.,data = train)
datan <- (train[, unlist(lapply(train, is.numeric))])
m = cor(datan,datan$Grad.Rate)
#first rearrenging cor matrix and remove duplicates
msort <- m %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)

msort$value <- abs(msort$value)
msort<-msort[(msort$value < 0.999),]
msort <-arrange(msort,desc(value))
head(msort,5)
steps <- step(tm, direction = 'forward',scope ~
                Outstate + perc.alumni + Top10perc + Top25perc + Room.Board)
model_forward <- lm(formula = Grad.Rate ~ Outstate + Top25perc + perc.alumni + Room.Board, data = train)
summary(model_forward)
#view RMSE of full model
preds.model_forward <- predict(model_forward,new = test)
full.rmse <-rmse(test$Grad.Rate,preds.model_forward)
full.rmse
