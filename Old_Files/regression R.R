install.packages('ISLR')
library(ISLR)
install.packages('caret')
library(caret)
install.packages('ggplot2')
library(ggplot2)
install.packages('gridExtra')
library(gridExtra)
install.packages('pROC')
library(pROC)

attach(Default)
train <- Default[1:2999,]
test <- Default[2999:7001,]
head(Default)
model1 <- glm(default~.,data = train,family= binomial(link= 'logit'))
summary(model1)
residuals(model1)
