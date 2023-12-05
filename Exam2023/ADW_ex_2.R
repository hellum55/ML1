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
par(mfrow=c(2,2))

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

pumpkin_data$month <- as.factor(pumpkin_data$month)
pumpkin_data$year <- as.factor(pumpkin_data$year)

#Question d:####
pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)

#Question e:###
AdjPrice <- median(pumpkin_data$Price*pumpkin_data$Spread)/mean(pumpkin_data$Spread)

std_adj <- sqrt(sum((pumpkin_data$Price - AdjPrice)^2) / (1473-1))

se <- std_adj/sqrt(1473)

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
pumpkin_train <- pumpkin_data %>%
  filter(year < 2017)

pumpkin_test <- pumpkin_data %>%
  filter(year == 2017)

#get data in (x,y) format (without intercept)
X <- x.train <- model.matrix(Price~., pumpkin_data)[,-1]
y <- pumpkin_data$Price

x.train <- model.matrix(Price~., pumpkin_train)[,-1]
x.test <- model.matrix(Price~., pumpkin_test)[,-1]

y.train <- pumpkin_train$Price
y.test <- pumpkin_test$Price

#Question h:####
#First we will predict price using OLS:
fit.ols <- lm(Price ~ ., data = pumpkin_train)
mean((predict(fit.ols, pumpkin_test) - y.test)^2)

#Predict with Ridge-reg with 5-fold cv:
#Call the glm function with alpha=0

#decreasing lambda grid from 1000 to 0.01 
set.seed(123)
l.grid <- seq(0.1, 100, by=0.1)

cv.ridge <- cv.glmnet(x.train, y.train, alpha = 0,
                      nfolds = 5, 
                      lambda = l.grid,
                      thresh = 1e-12)

plot(cv.ridge)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge


#evaluate MSE on test set:
ridge.pred <- predict(cv.ridge, s = bestlam.ridge, newx = x.test)
mean((ridge.pred - y.test)^2)
rsq_ridge_cv <- cor(y.test, ridge.pred)^2
rsq_ridge_cv

plot(y.test, ridge.pred, main = "Predicted price vs actual price")

#predict with lasso (LOOCV):
#default alpha value is 1 which gives a lasso model

#LOOCV using cv.glmnet
n <- length(y.train)  #sample size
set.seed(123)

cv.lasso <- cv.glmnet(x.train, y.train, alpha = 1, nfolds = n, lambda = l.grid)
plot(cv.lasso)
coef(cv.lasso)
bestlam.lasso<-cv.lasso$lambda.min
bestlam.lasso

lasso.pred <- predict(cv.lasso, s = bestlam.lasso, newx = x.test)
mean((lasso.pred - y.test)^2)
rsq_lasso_cv <- cor(y.test, lasso.pred)^2
rsq_lasso_cv

list.of.fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  list.of.fits[[fit.name]] <-
    cv.glmnet(x.train, y.train, type.measure = "mse", alpha=i/10)
}

results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  predicted <- 
    predict(list.of.fits[[fit.name]],
            s=list.of.fits[[fit.name]]$lambda.min, newx = x.test)
  
  mse <- mean((y.test-predicted)^2)
  
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}
results

#Question i:####
fit.1v <- lm(Price ~ City.Name, data = pumpkin_train)
mean((predict(fit.1v, pumpkin_test) - y.test)^2)

fit.2v <- lm(Price ~ month, data = pumpkin_train)
mean((predict(fit.2v, pumpkin_test) - y.test)^2)

fit.3v <- lm(Price ~ City.Name+month, data = pumpkin_train)
mean((predict(fit.3v, pumpkin_test) - y.test)^2)



library(leaps)
regfit.full <- regsubsets(Price~., pumpkin_data,
                          nvmax = 7)
reg.summary <- summary(regfit.full)
reg.summary$adjr2

par(mfrow = c(2, 2))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")






