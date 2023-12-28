#Question a:####
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

library(dplyr)
pumpkin_data <- pumpkin %>%
  select(c(City.Name, Variety, Date, Low.Price, High.Price, Item.Size))

#Question b:
pumpkin_data <- pumpkin_data %>%
  mutate(Price = (Low.Price+High.Price)/2,
         Spread = 5+(High.Price-Low.Price)) %>%
  select(-c(Low.Price, High.Price))

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

#The distribution of the price is indeed not normal distributed. The plot shows two peaks around
#the lower values and a peak again at 175-200 price-point. The spread-plot shows one huge peak at the start
#around 5-10 ish. And then small peaks but it is very skewed to the right. The mean is less than the median.
#The one big difference is in the amount of peaks. There is one outlier in the spread plot.

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
library(DataExplorer)
plot_missing(pumpkin_data)
pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)
sum(is.na(pumpkin_data))

#Question e:###
AdjPrice=function(Price,Spread){
  median_ps=median(pumpkin_data$Price*pumpkin_data$Spread)
  mean_s=mean(pumpkin_data$Spread)
  median_ps/mean_s
}
aprice <- AdjPrice(pumpkin_data$Price, pumpkin_data$Spread)

std_adj <- sqrt(sum((pumpkin_data$Price - aprice)^2) / (1473-1))
se_pop <- std_adj/sqrt(1473)
#It is not an accurate estimate without resampling as in a bootstrap. But we are able to
#compute an estimate though, but the accuracy is questionable and we concluded before that
#the distribution of spread and price are not normally distributed, which is a must for this type
#of computation. That's why we can apply bootstrapping so we can make a normal distribution out of the
#sample means. I dont think there is a formula for SD of a median value. This is why we can apply bootstrapping.


#Question f:####
library(boot)
set.seed(123)
AdjPrice.fn<-function(data, index){
  Price<-data$Price[index]
  Spread<-data$Spread[index]
  median(Price*Spread)/mean(Spread)
}

AdjPrice.fn(pumpkin_data,1:1473)

AdjPrice.fn(pumpkin_data, sample(1:1473, 1473, replace = TRUE))

boot.out=boot(data=pumpkin_data, statistic=AdjPrice.fn, R=1000)
boot.out
plot(boot.out)
#The bootsrap is used to tell the uncertainty given an estimator or a method. For example a SE
#of spread or a confidence interval for that. We simulate this process a 1000 times to estimate the SD
#so we can compute SE. 

#These are the results:

#    original    bias    std. error
#t1* 64.04122 0.7116221    2.772582

#95% confidensinterval
se_boot <- sd(boot.out$t)
mu <- mean(boot.out$t0)

c(mu - 2*se, mu + 2*se)

#The point estimate of the adjusted price are 64.041 which is the same as the population i computed before. The standard error of the bootsrap
#is 2.68 which is not very much when the mean is 64. The bootstrap statistics are very accurate and can predict the population parameter
#very precisely. The histogram shows that the distribution is normal we can therefor trust the bootstrap results. 
#The bias isnt very high and is centered t the true value. 

#Question g:####
#Split the data into different years and remove obs. with few observations
library(DataExplorer)
pumpkin_data <- pumpkin_data %>%
  group_by(year, month, City.Name) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup()

plot_bar(pumpkin_data)
  
library(rsample)
x <- model.matrix(Price~.,pumpkin_data)[, -1]
y <- pumpkin_data$Price

#decreasing lambda grid from 1000 to 0.01
l.grid <- 10^seq(10, -2, length = 100)

set.seed (185)
train <- sample(1:nrow(x), nrow(x) * 0.7) 
test <- (-train)
y.test <- y[test]
pumpkin_test <- pumpkin_data[test, ]

#Question h:####
#First we will predict price using OLS:
reg.lm<-lm(Price~., pumpkin_data[train, ])
lm.pred <- predict(reg.lm, newdata = pumpkin_data[test,])
lm.pred
ols.mse <- mean((lm.pred-y.test)^2)
ols.mse

#Predict with Ridge-reg with 5-fold cv:
#install.packages("glmnet")
library(caret)
library(glmnet)
set.seed(123)
mod.ridge <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = l.grid, tresh = 1e-12)
plot(mod.ridge)

cv.ridge <- cv.glmnet(x=x[train, ], y=y[train],
                      alpha = 0,
                      nfolds = 5)
plot(cv.ridge)
#It seems like the full model does a good job
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
#The lamda that results in the smallest CV-error is 6.45. Lets see what the RMSE is, associated with this lambda

ridge_pred <- predict(mod.ridge, newx = x[test, ], s=bestlam.ridge)
(rmse_ridge = sqrt(apply((y.test-ridge_pred)^2,2,mean)))
#Results in a RMSE of 53.53
ridge_mse <- mean((ridge_pred - y.test)^2)
ridge_mse

#predict with lasso (LOOCV):
#default alpha value is 1 which gives a lasso model
#LOOCV using cv.glmnet
n <- length(y[train])  #sample size
set.seed(123)
mod.lasso <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = l.grid, thresh = 1e-12)
plot(mod.lasso)

cv.lasso <- cv.glmnet(x[train, ], y[train],
                      alpha = 1,
                      nfolds = n)

bestlam.lasso<-cv.lasso$lambda.min
coef(cv.lasso)
#The best model with lasso takes around 14 parameters. So it is a simpler model than ridge but,
#does not perform better than ridge, but slightly better than OLS.

lasso.pred <- predict(mod.lasso, s = bestlam.lasso, newx = x[test, ])

(rmse = sqrt(apply((y.test-lasso.pred)^2,2,mean)))
lasso_mse <- mean((lasso.pred - y.test)^2)
lasso_mse

#Question i:####
intercept_only <- lm(formula = Price ~ 1, data = pumpkin_data[train, ])
intercept_pred <- predict(intercept_only, pumpkin_data[test, ])
intercept_mse <- mean((intercept_pred - y.test)^2)

model_city.name <- lm(Price ~ City.Name, data = pumpkin_data[train, ])
city.name_pred <- predict(model_city.name, pumpkin_data[test, ])
city.name_mse <- mean((city.name_pred - y.test)^2)

model.month <- lm(Price ~ month, data = pumpkin_data[train, ])
month_pred <- predict(model.month, pumpkin_data[test, ])
month_mse <- mean((month_pred - y.test)^2)

model_city_month <- lm(Price ~ City.Name+month, data = pumpkin_data[train, ])
city_month_pred <- predict(model_city_month, pumpkin_data[test, ])
city_month_mse <- mean((city_month_pred - y.test)^2)

data.frame(method = c("OLS", "Ridge", "Lasso", "Intercept", "City", "month","City+month"), 
           test_MSE = c(ols.mse, ridge_mse, lasso_mse, intercept_mse, city.name_mse, month_mse, city_month_mse))
arrange(test_MSE)

