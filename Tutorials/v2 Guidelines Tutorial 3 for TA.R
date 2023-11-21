#########################################################
# Course: Machine Learning for BI 1 
# Tutorial 3: General Guiding Solution 
# Ana Alina Tudoran & Bezirgen Veliyev
# TA: Camille Pedersen
# Aarhus University, Fall 2023
#########################################################

library(dplyr)    
library(ggplot2)
library(caret)    
library(recipes)
library(vip)    
library(MASS) #includes the Boston data 
library(DataExplorer)
library (GGally) 
library(rsample)


### PROBLEM 1: DATA ANALYSIS

## 1.1
# Explore data set

?Boston

Boston_df <- Boston
View(Boston_df)
summary(Boston_df) 
str(Boston_df)


# missing
table(is.na(Boston_df)) 
plot_missing(Boston_df)

# data split
set.seed(123)
split <- initial_split(Boston_df, prop = 0.8, strata = "crim") 
Bost_train  <- training(split)
Bost_test   <- testing(split)

# data exploration
boxplot(Bost_train$crim, ylab = "Crime per capita") 
ggplot(Bost_train, mapping = aes (x=factor(0), y=crim)) + geom_boxplot() + xlab("") + ylab("Crime per capita")
plot_density(Bost_train)
plot_histogram(Bost_train)

# bivariate maps 
pairs(Boston_df[, 1:10])
ggpairs(Boston_df, columns = 1:10)



## 1.2

# OLS Regression (without data engineering)
set.seed(123)  
(cv_model1 <- train(
  form = crim ~ ., 
  data = Bost_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))
cv_model1
cv_model1$finalModel 


## 1.3

# OLS Assumptions 
par(mfrow=c(2,2))
plot(cv_model1$finalModel) # interpret the plots

# Linearity (just an example)
p1 <- ggplot(Bost_train, aes(nox, crim)) + 
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE) +
  scale_y_continuous("crim") +
  xlab("nox") +
  ggtitle(paste("Non-transformed variables with a\n",
                "non-linear relationship."))
p1

# log transforming the target 
p2 <- ggplot(Bost_train, aes(nox, crim)) + 
  geom_point(size = 1, alpha = .4) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10("Crime", 
                breaks = seq(0, 100, by = 1)) +
  xlab("Nox") +
  ggtitle(paste("Transforming variables can provide a\n",
                "near-linear relationship."))
p2
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Constant variance
df1 <- broom::augment(cv_model1$finalModel, data = Bost_train) # add model results to each observation (see df1)
p1 <- ggplot(df1, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "OLS")
p1
# variance heterogeneity seems to be problematic

# Independence and uncorrelated errors
df1 <- mutate(df1, id = row_number()) #order data by ID

p1 <- ggplot(df1, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.")
p1

# Multicollinearity
# rad and tax are two variables that have a correlation of 0.9
# fit with two strongly correlated variables
summary(cv_model1) %>%
  broom::tidy() %>%
  filter(term %in% c("tax", "rad"))

# model without rad
set.seed(123)
mod_wo_rad <- train(
  crim ~ ., 
  data = Bost_train[, -9], 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(mod_wo_rad) %>%
  broom::tidy() %>%
  filter(term == "tax")

# model without tax
set.seed(123)
mod_wo_tax <- train(
  crim ~ ., 
  data = Bost_train[, -10], 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(mod_wo_tax) %>%
  broom::tidy() %>%
  filter(term == "rad")

# The above result reflects the instability in the linear regression model 
# caused by between-predictor relationships.


## 1.4

# OLS with feature and target engineering 

Bost_train <- Bost_train[, -9] #delete rad
Bost_test <- Bost_test[, -9]

# OLS-regression
set.seed(123)

Bost_recipe <- recipe(crim ~ ., data = Bost_train) %>%
  step_log(all_outcomes()) %>% 
  step_nzv(all_numeric(), -all_outcomes()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1)

reg_fit2 <- train(
  Bost_recipe,      
  data = Bost_train, 
  method = "lm", 
  trControl = cv, 
  metric = "RMSE"
)
reg_fit2 

# new data (test)
predictions_reg <- predict(reg_fit2, Bost_test)
test_RMSE_reg =sqrt(mean((log(Bost_test$crim) - predictions_reg)^2))
test_RMSE_reg

## 1.5

# Models with feature and target engineering 

# KNN-regression
set.seed(123)
Bost_recipe <- recipe(crim ~ ., data = Bost_train) %>%
  step_log(all_outcomes()) %>% 
  step_nzv(all_numeric(), -all_outcomes()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1)

hyper_grid <- expand.grid(k = seq(1, 25, by = 1))

knn_fit2 <- train(
  Bost_recipe,      
  data = Bost_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)
knn_fit2 
ggplot(knn_fit2)

# new data (test)
predictions_knn <- predict(knn_fit2, Bost_test)
test_RMSE_knn =sqrt(mean((log(Bost_test$crim) - predictions_knn)^2))
test_RMSE_knn


# PCR-regression
set.seed(123)
Bost_recipe <- recipe(crim ~ ., data = Bost_train) %>%
  step_log(all_outcomes()) %>% 
  step_nzv(all_numeric(), -all_outcomes()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1)

pcr_fit2 <- train(
  Bost_recipe,      
  data = Bost_train, 
  method = "pcr", 
  trControl = cv, 
  tuneLength = 13, 
  metric = "RMSE"
)
pcr_fit2 
ggplot(pcr_fit2)

# new data (test)
predictions_pcr <- predict(pcr_fit2, Bost_test)
test_RMSE_pcr =sqrt(mean((log(Bost_test$crim) - predictions_pcr)^2))
test_RMSE_pcr


# PLS-regression
set.seed(123)
Bost_recipe <- recipe(crim ~ ., data = Bost_train) %>%
  step_log(all_outcomes()) %>% 
  step_nzv(all_numeric(), -all_outcomes()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1)

pls_fit2 <- train(
  Bost_recipe,      
  data = Bost_train, 
  method = "pls", 
  trControl = cv, 
  tuneLength = 13, 
  metric = "RMSE"
)
pls_fit2 
ggplot(pls_fit2)

# new data (test)
predictions_pls <- predict(pls_fit2, Bost_test)
test_RMSE_pls =sqrt(mean((log(Bost_test$crim) - predictions_pls)^2))
test_RMSE_pls


## 1.6

#  Summarizing the results using a list
results <- resamples(list(reg  = reg_fit2, 
                          knn = knn_fit2,
                          pcr = pcr_fit2,
                          pls = pls_fit2))
summary(results)
bwplot(results)
dotplot(results)
# Based on the above, the best model based on Mean cv-RMSE 
# is ___knn regression_____.

# calculate the training error 
predictions_knn_train <- predict(knn_fit2, Bost_train)
train_RMSE_knn =sqrt(mean((log(Bost_train$crim) - predictions_knn_train)^2))
train_RMSE_knn # 0.45
# as expected it is lower than test-RMSE, but there is no sign of overfitting 


## 1.7

# Feature importance 
vip::vip(knn_fit2, method = "model")


## 1.8

par(mfrow=c(2,2))
plot(reg_fit2$finalModel)

# We can observe a general improvement in all aspects of the model’s performance, 
# including the residual versus fitted values plot and the Normal Q-Q plot

 # If interested in Inference (sign and size of the effects) like in classical statistics, 
 # display final OLS regression and interpret the output
reg_fit2$finalModel # the variables were pre-processed in this model and therefore it is 
 # difficult to interpret the betas. The alternative is to select the most important predictors
 # and re-run the model with those variables in the unprocessed format.


# 1.9

# With the current knowledge, there are possibilities of improvement by:
#  - deleting some outliers.
#  - evaluating other transformation function for Y;
#  - changing the order of feature engineering steps;
#  - prepare the blueprint outside the k-fold process (see lecture - option 2);
#  - creating PC before applying any model  
#  - tuning the number of principal components better (in the guideline we use tune Length=13 – a number that was randomly selected)
#  - tuning the % of variance explained instead of the number of principal components


### PROBLEM 2: LINEAR REGRESSION

## 2.1

# Write your own OLS function

fOLS <- function(df,Xlab,Ylab){
  
  X <- as.matrix(df[,Xlab])
  Y <- df[,Ylab]
  
  bhat <- solve(t(X)%*%X)%*%t(X)%*%Y # OLS estimates
  yhat <- X%*%bhat # predictions of Y
  resid <- Y-yhat # residuals
  sig2hat <- sum(resid^2)/(length(Y) - length(bhat)) # error variance
  var_bhat <- sig2hat*solve(t(X)%*%X) # variance-covariance matrix for bhat
  se_bhat <- sqrt(diag(var_bhat))# standard errors for bhat
  
  output <- list("coefficients"=bhat,
                 "predictions"=yhat,
                 "error variance"=sig2hat,
                 "standard errors"=se_bhat)
  
  return(output)
}

## 2.2

# Apply your function and compare to lm()

#compare results (no intercepts)
Boston_df <- Boston
mod.my <- fOLS(Boston_df,-14,"medv")
mod.lm <- lm(medv ~ .-1, data=Boston_df)

output.lm <- summary(mod.lm)

#beta coefficients
cbind(mod.my$coefficients,mod.lm$coefficients)

#predictions
cbind(mod.my$predictions,predict(mod.lm))[1:10,]

#std-err of regression/sqrt of the error variance
c(sqrt(mod.my$`error variance`),summary(mod.lm)$sigma)
c(mod.my$`error variance`,summary(mod.lm)$sigma^2) #error variance

#std-err of the coefficient estimates
cbind(mod.my$'standard errors',summary(mod.lm)$coefficients[,"Std. Error"])






