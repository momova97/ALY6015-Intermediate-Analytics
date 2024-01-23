print('Mohammad Hossein Movhaedi')
print('assignment 2')
install.packages('Hmisc')
library(Hmisc)
#Question 6: Blood Types
#assigning alpha
alpha <- 0.1
#vector of observation
c <- c(12,8,24,6)
#vector of general distribution
p <- c(0.2,0.28,0.36,0.16)

#running chi test
r <- chisq.test(x=c,p=p)
r$statistic
r$parameter
r$p.value

#cheaking the p-value
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#question 8
#assigning alpha
alpha <- 0.05
#vector of observation
c <- c(125,10,25,40)
#vector of general distribution
p <- c(0.708,0.082,0.09,0.12)

#running chi test
r <- chisq.test(x=c,p=p)
r$statistic
r$parameter
r$p.value

#cheaking the p-value
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")

#section 11-2
#question 8
#assigning alpha
alpha <- 0.05
#vector of observation
c2013 <- c(724,335,174,107)
c2014 <- c(370,292,152,140)
#stating n of rows
rows = 2
#creating a matrix of rows
mt <- matrix(c(c2013,c2014),nrow = rows,byrow  = T)
rownames(mt) = c(2013,2014)
colnames(mt) = c("Caucasian","Hispanic","African American","Other")

#doing the test
r <- chisq.test(mt)
r$p.value
#cheaking the p-value
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")

#question 10
#assigning alpha
alpha <- 0.05
#vector of observation
Army<-c(10791,62491)
Navy<-c(7816,42750)
Marine_Corps<-c(932,9525)
Air_Force<-c(11819,54344)
#stating n of rows
rows = 4
#creating a matrix of rows
mt <- matrix(c(Army,Navy,Marine_Corps,Air_Force),nrow = rows,byrow  = T)
rownames(mt) = c("Army","Navy", "Marine_Corps","Air_Force")
colnames(mt) = c("Officers","Enlisted")

#doing the test
r <- chisq.test(mt)
r$p.value
r
#cheaking the p-value
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#section 12-1
#question 8
#assigning alpha
alpha <- 0.05
#vector of observation
Condiments<-c(270,
              130,
              230,
              180,
              80,
              70,
              200)
Cereals<-c(260,
           220,
           290,
           290,
           200,
           320,
           140)
Desserts<-c(100,
            180,
            250,
            250,
            300,
            360,
            300,
            160)
#running t.test for each pairs
r <- t.test(Cereals,Condiments)
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#not rejected
r <- t.test(Cereals,Desserts)
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#not rejected
r <- t.test(Condiments,Desserts)
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#not rejected
#section 12-2
#question  10
#assigning alpha
alpha <- 0.05
#input observations
Cereal<-data.frame('data'=c(578,
                              320,
                              264,
                              249,
                              237),'Food'="Cereal")
Chocolate<-data.frame('data'=c(311,
                                    106,
                                    109,
                                    125,
                                    173),'Food'="Chocolate")
Coffee<-data.frame('data'=c(261,
                              185,
                              302,
                              689),'Food'="Coffee")
data <- rbind(Cereal,Chocolate,Coffee)
data$Food<-as.factor(data$Food)
#running the test
anova<-aov(data~Food,data = data)
summary(anova)
#extracting P-value
p<- summary(anova)
pv<-p[[1]][[1,'Pr(>F)']]
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#question 12
#assigning alpha
alpha <- 0.05
#input observations
inputdata<-data.frame('data'=c(4946,
                               5953,
                               6202,
                               7243,
                               6113,
                               6149,
                               7451,
                               6000,
                               6479,
                               5282,
                               8605,
                               6528,
                               6911),lable =c('Eastern','Eastern','Eastern',
                                              'Eastern','Eastern','Middle',
                                              'Middle','Middle','Middle',
                                              'Western','Western','Western',
                                              'Western'))
inputdata$lable<-as.factor(inputdata$lable)
#running the test
anova<-aov(data~lable,data = inputdata)
summary(anova)
#extracting P-value
p<- summary(anova)
pv<-p[[1]][[1,'Pr(>F)']]
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
#section 12-3
#question 10
m11 <- data.frame('data'=c(9.2, 9.4, 8.9),'grow' = 1,plant='a')
m12 <- data.frame('data'=c(8.5, 9.2, 8.9),'grow' = 2,plant='a')
m21 <- data.frame('data'=c(7.1, 7.2, 8.5),'grow' = 1,plant='b')
m22 <- data.frame('data'=c(5.5, 5.8, 7.6),'grow' = 2,plant='b')
indata<-rbind(m11,m12,m21,m22)
indata$grow <- as.factor(indata$grow)
indata$plant <- as.factor(indata$plant)
anova<-aov(data~grow+plant,data=indata)
summary(anova)

#the baseball part
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
#read the data
bb<-read.csv('baseball.csv')

# Extract decade from year
bb$Decade <- bb$Year - (bb$Year %% 10)
# Create a wins table by summing the wins by decade
wins <- bb %>%
  group_by(Decade) %>%
  summarize(wins = sum(W)) %>%
  as.tibble()
#assigning alpha
alpha <- 0.05
p =rep(1/6,6)
r <-chisq.test(wins$wins,p =p)
r$statistic
r$p.value
#cheaking the p-value
ifelse(r$p.value > alpha,"H0 is not rejected","H0 is rejected")
