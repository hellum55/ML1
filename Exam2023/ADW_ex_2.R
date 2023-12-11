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

pumpkin_data <- pumpkin_data %>%
  mutate(Item.Size = case_when(
    Item.Size == 'sml' ~ 0,
    Item.Size == 'med' ~ 1,
    Item.Size == 'med-lge' ~ 2,
    Item.Size == 'lge' ~ 3,
    Item.Size == 'xlge' ~ 4,
    Item.Size == 'exjbo' ~ 5,
    TRUE ~ as.numeric(Item.Size)  # If there are other values, keep them as is (or handle accordingly)
  ))

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
#get data in (x,y) format (without intercept)
library(rsample)
set.seed(1)
pumpkin_train <- pumpkin_data %>%
  filter(year < 2017)

pumpkin_test <- pumpkin_data %>%
  filter(year == 2017)

x <- model.matrix(Price~., pumpkin_train)[,-4]
y <- pumpkin_train$Price

train <- sample(1:nrow(x), nrow(x) * 0.8)
test <- (-train)
y.test <- y[test]

#Question h:####
#First we will predict price using OLS:
reg.lm<-lm(Price~., data = pumpkin_train)
summary(reg.lm)
lm.pred = predict(reg.lm, pumpkin_test)
(ols_mse <- mean((lm.pred - pumpkin_test$Price)^2))

rsq_reg_lm <- cor(y.test, lm.pred)^2
rsq_reg_lm
RMSE1

#Predict with Ridge-reg with 5-fold cv:
#Call the glm function with alpha=0

#decreasing lambda grid from 1000 to 0.01
#install.packages("glmnet")
library(caret)
library(glmnet)
set.seed(123)
l.grid <- 10^seq(3,-2,length=100)

cv.ridge <- cv.glmnet(x=x[train, ], y=y[train],
                      alpha = 0,
                      nfolds = 5,
                      lambda = l.grid,
                      standardize = TRUE)

plot(cv.ridge)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

model_ridge_best <- glmnet(x[train, ], y[train],
                           alpha = 0, 
                           lambda = bestlam.ridge,
                           standardize = TRUE)

ridge_pred <- predict(model_ridge_best, s = bestlam.ridge, newx = x[test, ])
(ridge_mse <- mean((ridge_pred - y.test)^2))

#predict with lasso (LOOCV):
#default alpha value is 1 which gives a lasso model

#LOOCV using cv.glmnet
n <- length(y.train)  #sample size
set.seed(123)

cv.lasso <- cv.glmnet(x.train, y.train,
                      alpha = 1,
                      nfolds = n,
                      lambda = l.grid,
                      standardize = TRUE)
plot(cv.lasso)
coef(cv.lasso)
bestlam.lasso<-cv.lasso$lambda.min
bestlam.lasso

model_lasso_best <- glmnet(x.train, y.train,
                           alpha = 1,
                           lambda = bestlam.lasso,
                           standardize = TRUE)

lasso.pred <- predict(model_lasso_best, s = bestlam.lasso, newx = x.test)
lasso_mse <- mean((lasso.pred - y.test)^2)
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
fit.0v <- lm(Price ~ City.Name, data = pumpkin_train)
v0_pred <- predict(fit.0v, pumpkin_test)
v0_mse <- mean((v0_pred - y.test)^2)

fit.1v <- lm(Price ~ month, data = pumpkin_train)
v1_pred <- predict(fit.1v, pumpkin_test)
v1_mse <- mean((v1_pred - y.test)^2)

fit.2v <- lm(Price ~ City.Name+month, data = pumpkin_train)
v2_pred <- predict(fit.2v, pumpkin_test)
v2_mse <- mean((v2_pred - pumpkin_test$Price)^2)
v2_mse

SS_tot <- sum((y.test - mean(y.test))^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "v0", "v1", "v2"), 
           test_MSE = c(ols_mse, ridge_mse, lasso_mse, v1_mse, v1_mse, v2_mse), 
           test_R2 = c(cor(y.test, lm.pred)^2,
                       cor(y.test, ridge_pred)^2, 
                       cor(y.test, lasso.pred)^2, 
                       cor(y.test, v0_pred)^2, 
                       cor(y.test, v1_pred)^2,
                       cor(y.test, v2_pred)^2)) %>%
arrange(test_MSE)




