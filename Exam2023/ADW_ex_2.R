#Question a:####
library(dplyr)
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

pumpkin_data <- pumpkin %>%
  select(c(City.Name, Variety, Date, Low.Price, High.Price, Item.Size))

#Question b:
pumpkin_data <- pumpkin_data %>%
  mutate(Price = (Low.Price+High.Price)/2,
         Spread = 5+(High.Price-Low.Price)) %>%
  select(-c(Low.Price, High.Price))

par(mfrow=c(1,2))
hist(pumpkin_data$Price, breaks = 20, col = "red", border = "red") 
hist(pumpkin_data$Spread, breaks = 20, col = "blue", border = "blue") 

library(ggplot2)


# Basic density
#Price
par(mfrow=c(1,2))

# Histogram with density plot (Price)
ggplot(pumpkin_data, aes(x=Price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

# Histogram with density plot (Price)
ggplot(pumpkin_data, aes(x=Spread)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

#Question c:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

#Question d:####
pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)

#Question e:###
AdjPrice <- median(pumpkin_data$Price*pumpkin_data$Spread)/mean(pumpkin_data$Spread)
std_adj <- sd(pumpkin_data$Price * pumpkin_data$Spread) / mean(pumpkin_data$Spread)


#Question f:####
library(boot)
set.seed(123)
AdjPrice.fn<-function(data, index){
  Price<-data$Price[index]
  Spread<-data$Spread[index]
  median(Price*Spread)/mean(Spread)
}

AdjPrice.fn(pumpkin_data, sample(1:1473, 1473, replace = TRUE))

boot.out=boot(data=pumpkin_data, statistic=AdjPrice.fn, R=1000)
boot.out
plot(boot.out)

#95% confidensinterval
se <- sd(boot.out$t)
mu <- mean(boot.out$t0)

c(mu - 2*se, mu + 2*se)

#Question g:####
set.seed(123)
pumpkin_train <- pumpkin_data %>%
  filter(year < 2017)

pumpkin_test <- pumpkin_data %>%
  filter(year == 2017)

#Question h:####
#First we will predict price using OLS:
library(glmnet)

#get data in (x,y) format (without intercept)
set.seed(1)
X <- model.matrix(Price~., pumpkin_train)[,-1]
y <- pumpkin_train$Price

train <- sample(1:nrow(pumpkin_train))
test <- (-train)
y.test <- y[test]

reg.ols<-lm(y, pumpkin_train)
summary(reg.ols)

#Predict with Ridge-reg with 5-fold cv:
#Call the glm function with alpha=0

#decreasing lambda grid from 1000 to 0.01 
set.seed(123)
l.grid <- 10^seq(3,-2,length=100)

fit.ridge <- glmnet(X,y,alpha = 0)
plot(fit.ridge, xvar="lambda", label = TRUE)
cv.ridge <- cv.glmnet(X[train, ], y[train], alpha = 0, nfolds = 5, lambda = l.grid)
plot(cv.ridge)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

#evaluate MSE on test set:
ridge.pred <- predict(cv.ridge, s = bestlam.ridge, newx = X[test, ])
mean((ridge.pred - y.test)^2)

#predict with lasso (LOOCV):
#default alpha value is 1 which gives a lasso model

#LOOCV using cv.glmnet
n <- length(y)  #sample size

cv.lasso <- cv.glmnet(X, y, alpha = 1, nfolds = n, lambda = l.grid)
plot(cv.lasso)
coef(cv.lasso)
bestlam.lasso<-cv.lasso$lambda.min
bestlam.lasso

#Question i:####


