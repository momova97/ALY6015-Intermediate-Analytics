#lasso
library(glmnet)
library(Metrics)
 
#load the data
dataset <- read.csv()

#split the data
set.seed(123)
trainIndex<-sample(x = nrow(dataset) , size(dataset)*0.7)
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]

train_x <- model.matrix(Fertility~.,tain)[,-1]
test_x <- model.matrix(Fertility~.,test)[,-1]

train_y <- train$Fertility
test_y <- test%Fertility

#finding value of lamda

#finding the best value of lamda
set.seed(123)
cv.lasso <- cv.glmnet(train_x,train_y,nfolds = 10)
plot(cv.lasso)

#object : min perdiction error
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.lse)
#fit model based on lambda
model.min <- glmnet(train_x,train_y,alpha = 1,lambda = cv.lasso$lambda.min)
model.min

#display reg coeff
coef(model.min)

#display coeff of ols model with no regul
ols <- lm(Fertility~.,data = train)
coef(ols)

#view RMSE
preds.ols <- predict(ols,new = test)
rmse(test$Fertality,preds.ols)

#train set prediction
preds.train<- predict(ols,newx = train_x)
train.rmse <- rmse(train_y,preds.train)
#test set prediction
preds.test <- predict(model.lse,newx =train_x)
test.rmse <- rmse(test_y,preds.test)

#compering values of rmse
train.rmse
test.rmse
