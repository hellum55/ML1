#Question a:####
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

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
sd_pop <- std_adj/sqrt(1473)
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

AdjPrice.fn(pumpkin_data, sample(1:1473, 1473, replace = TRUE))

boot.out=boot(data=pumpkin_data, statistic=AdjPrice.fn, R=1000)
boot.out
plot(boot.out)
#The bootstrap is used to tell the uncertainty given an estimator or a method. For example a SE
#of spread or a confidence interval for that. We simulate this process a 1000 times to estimate the SD
#so we can compute SE. The SE is significantly lower than the STD from the population. When computing the SE
#from the bootstrap you compute the STD from the 1000 bootstrap sample medians and that is what gives the
#SE which is lower because it is uncertain that multiple outliers will appear in the same bootstrap sample

#These are the results:

#    original    bias    std. error
#t1* 64.04122 0.7116221    2.772582

#95% confidence interval
se_boot <- sd(boot.out$t)
mu <- mean(boot.out$t0)

c(mu - 2*se_boot, mu + 2*se_boot)

#The point estimate of the adjusted price are 64.041 which is the same as the population i computed before. The standard error of the bootsrap
#is 2.68 which is not very much when the mean is 64. The bootstrap statistics are very accurate and can predict the population parameter
#very precisely. The histogram shows that the distribution is normal we can therefor trust the bootstrap results. 
#The bias isnt very high and is centered t the true value. 

#Question g:####
#Split the data into different years and remove obs. with few observations
library(DataExplorer)
pumpkin_data <- pumpkin_data %>%
  group_by(year, month, City.Name) %>%
  filter(n() / nrow(.) >= 0.005) %>%
  ungroup()

plot_bar(pumpkin_data)
str(pumpkin_data)

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
library(Metrics)
reg.lm<-lm(Price~., pumpkin_data[train, ])
lm.pred <- predict(reg.lm, newdata = pumpkin_data[test,])
lm.pred
ols.mse <- mse(lm.pred, y.test)
ols.mse

#Predict with Ridge-reg with 5-fold cv:
library(caret)
library(glmnet)
set.seed(123)
cv.ridge <- cv.glmnet(x=x[train, ], y=y[train],
                      alpha = 0, lambda = l.grid, nfolds = 5)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
plot(cv.ridge)
#It seems like the full model does a good job

mod.ridge <- glmnet(x[train, ], y[train], alpha = 0, lambda = l.grid,
                  tresh = 1e-12)
plot(mod.ridge, xvar = "lambda")

#The lambda that results in the smallest CV-error is 5.88. Lets see what the RMSE is, associated with this lambda
ridge_pred <- predict(mod.ridge, newx = x[test, ], type = "response", s = bestlam.ridge)
ridge_rmse <- rmse(y.test, ridge_pred)
#Results in a RMSE of 53.53
ridge_mse <- mse(ridge_pred, y.test)
ridge_mse

#predict with lasso (LOOCV):
#default alpha value is 1 which gives a lasso model
#LOOCV using cv.glmnet
n <- length(y[train])  #sample size
set.seed(123)
cv.lasso <- cv.glmnet(x[train, ], y[train],
                      alpha = 1, lambda = l.grid,
                      nfolds = n)
plot(cv.lasso)
bestlam.lasso<-cv.lasso$lambda.min

mod.lasso <- glmnet(x[train, ], y[train], alpha = 1, lambda = l.grid,
                     thresh = 1e-12)
plot(mod.lasso, xvar = "lambda")

cv.lasso <- cv.glmnet(x[train, ], y[train],
                      alpha = 1, lambda = l.grid,
                      nfolds = n, type.measure = "mse")
#The best model with lasso takes around 14 parameters. So it is a simpler model than ridge but,
#does not perform better than ridge, but slightly better than OLS.

lasso.pred <- predict(mod.lasso, s = bestlam.lasso, newx = x[test, ])

rmse(y.test, lasso.pred)
lasso_mse <- mse(lasso.pred, y.test)
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

cor(lm.pred, ridge_pred)
cor(lm.pred, lasso.pred)

#The lasso model does a slightly better job than OLS and Ridge. The number of predictors P are not significantly large compared to sample size.
#The lasso model performs variable reduction and can cut out noisy variables which the OLS model still uses, but the lasso model
#only cut off 10(ish) variables which is not much. Therefor the gain is not huge compared to the two other models.
#In the last part there is a sign of forward stepwise selection. First the intercept and then the best variables are connected to the model.
#We can see that the model with only one variable performs worse than the two variable at the same time.

#Best subset selection with AIC, BIC and R^2
library(leaps)
reg.full<-regsubsets(Price~.-Spread, nvmax = 30, data=pumpkin_data)
summary(reg.full)
names(summary(reg.full))
plot(summary(reg.full)$cp, xlab="Number of Variables", ylab="Cp", type="l") #figure of Cp
#minimize by Cp
which.min(summary(reg.full)$cp) #at which p does Cp attain minimum? This produces p=10
coef(reg.full, 22) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.full)$bic)
coef(reg.full, 12)

##Forward Stepwise Selection
reg.fwd<-regsubsets(Price~.-Spread, nvmax=40, method="forward", data=pumpkin_data[train,])
summary(reg.fwd)
plot(reg.fwd, scale = "Cp")
#minimize by Cp
which.min(summary(reg.fwd)$cp) #at which p does Cp attain minimum? This produces p=36
plot(summary(reg.fwd)$cp, xlab="No of Variables", ylab="Cp", type="l") #figure of Cp
#minimize by Cp
coef(reg.fwd, 21) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.fwd)$bic)
coef(reg.fwd, 13)

#Additional and just for fun:
# loop through models of each size and compute test RMSE for each model
# (clever approach from ISLR page 248)
fwd.errors <- rep(NA, 29)
val.mat <- model.matrix(Price~.-Spread, data = pumpkin_data[test,])
for (i in 1:29) {
  # extract the coefficients from the best model of that size
  coefi <- coef(reg.fwd, id = i)
  # multiply them into the appropriate columns of the test model matrix to
  # predict
  pred <- val.mat[, names(coefi)] %*% coefi
  # compute the test MSE
  fwd.errors[i] <- mean((y.test - pred)^2)
}
which.min(fwd.errors)
plot(fwd.errors,ylab="MSE", pch=19, type = "b")

##Best Subset Selection with 10-fold Cross validation
library(leaps)
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = 25 #ncol(pumpkin_data) - 1
folds = sample(rep(1:k, length = nrow(pumpkin_data)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(Price ~ .-Spread, data = pumpkin_data[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, pumpkin_data[folds == i, ], id = j)
    cv.errors[i, j] = mean((pumpkin_data$Price[folds == i] - pred)^2)
  }
}
mse.cv = (apply(cv.errors, 2, mean))
mse.cv[which.min(mse.cv)]
plot(mse.cv, pch = 25, type = "b")
which.min(mse.cv)

#Unfortunately regsubsets() does not have built-in predict function.
#So we write one for us below. You may suppose this as given.
predict.regsubsets<-function(object, newdata, id){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form, newdata)
  coefi<-coef(object, id=id)
  xvars<-names(coefi)
  mat[, xvars]%*%coefi
}

#Predictions with regsubsets:
val.errors <- rep(NA, 30)
x.test <- model.matrix(Price~.-Spread, data = pumpkin_data[test, ])
for(i in 1:30){
  coefi=coef(reg.fwd, id=1)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.test-pred)^2)
}
val.errors

plot(sqrt(val.errors),ylab="Root MSE", ylim=c(6000,7000), pch=19, type = "b")
points(sqrt(reg.fwd$rss[-1]/100),col="blue",pch=19,type="b")
