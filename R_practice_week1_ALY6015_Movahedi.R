print('Mohammad Hossein Movahedi')
print('R practice week 1')
#importing and instaling libraries
#importing and instaling libraries
install.packages('FSA')
install.packages('magrittr')
install.packages('dplyr')
install.packages('tidyr')
install.packages('plyr')
install.packages('tidyverse')
install.packages('outliers')
install.packages('ggplot2')
install.packages('lubridate')
install.packages("janitor")
install.packages('imputeTS')
install.packages('corrplot')
install.packages('mctest')

install.packages('leaps')

library(FSA)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(outliers)
library(janitor)
library(corrplot)

library(leaps)


#Step 1 Importing dataset
data <- read.csv("AmesHousing.csv")


#step 2 perform Exploratory Data Analysis
str(data)
summary(data)

#step 3 data cleaning
#names cleaning
data<-clean_names(data)
colnames(clean)
#removing empty rows and cols
data<-data %>% remove_empty(whic=c("rows"))
data<-data %>% remove_empty(whic=c("cols"))
#removing duplicate rows
data <- data %>% distinct()
#replacing missing values with mean
library(imputeTS)
data <-na_mean(data)

#step 4 the cor()
datan <- (data[, unlist(lapply(data, is.numeric))])
m = cor(datan)
#step 5 corplot
corrplot(m)
#step 6 scater plot
#finding variables correlation
m2 = cor(datan[-39], datan$sale_price) 
#scatter plot for most correlated
ggplot(datan, aes(x=sale_price, y=overall_qual)) + geom_point()
#scatter plot for least correlated
ggplot(datan, aes(x=sale_price, y=misc_val)) + geom_point()
#scatter plot for 0.5 correlation
ggplot(datan, aes(x=sale_price, y=mas_vnr_area)) + geom_point()


#step 7 fitting regression model
#first rearrenging cor matrix and remove duplicates
msort <- m2 %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)

msort$value <- abs(msort$value)
msort<-msort[!(msort$value==1),]
msort <-arrange(msort,desc(value))
head(msort,3)
#creating regression model
reg <- lm(sale_price ~ overall_qual + gr_liv_area + garage_cars, data = datan)
summary(reg)

#step 9 plotting
ggplot(datan, aes(x=sale_price, y=overall_qual)) + geom_point()
ggplot(datan, aes(x=sale_price, y=gr_liv_area)) + geom_point()
ggplot(datan, aes(x=sale_price, y=garage_cars)) + geom_point()

# step 10 checking multicollinearity
library(mctest)
omcdiag(reg)
imcdiag(reg)
#step 11 dealing with outliesrs
# Create data frame for regression model
rdata <- data.frame(order = datan$order,sale = datan$sale_price,overall = datan$overall_qual,area = datan$gr_liv_area,
                    cars = datan$garage_cars)

# Calculate residuals
rdata$residuals <- as.numeric(residuals(reg))

# Choose a threshhold
outlier_threshold <- 19487

# Print only names of outliers
outliers <- rdata[ abs(rdata$residuals) > outlier_threshold, ]
print(outliers$order)
rodata <-filter(rdata,!order %in% outliers$order,)
#ploting
ggplot(rodata, aes(x=sale, y=overall)) + geom_point()
ggplot(rodata, aes(x=sale, y=area)) + geom_point()
ggplot(rodata, aes(x=sale, y=cars)) + geom_point()

#step 13 leap of faith into submodels
models <- regsubsets(sale_price~., data = datan, nvmax = 5) 
summary(models)
#step 14 the part it ends in compareing
s13model <-lm(sale_price ~ overall_qual + gr_liv_area + bsmt_fin_sf_1, data = datan)
anova(reg,s13model)
