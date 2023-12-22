#########################################################
# Course: Machine Learning
#########################################################
# Modeling packages 
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training # a meta-engine

# pumpkin data
pumpkin <- read.csv("~/ML1/Exam2023/pumpkintrain.csv")
pumpkin <- na.omit(pumpkin)
pumpkin <- pumpkin %>%
  select(-Repack)

# Data partitioning (we apply stratified sampling here) 
set.seed(123)
split <- initial_split(pumpkin, prop = 0.7, 
                       strata = "price")
pumpkin_train  <- training(split)
pumpkin_test   <- testing(split)

hist(pumpkin$price)
hist(pumpkin_train$price)
hist(pumpkin_test$price)

# Specify re-sampling strategy: k-fold cross-validation
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1
)

# Create a grid of values for the hyperparameter k
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search (here we use caret package)
knn_fit <- train(
  price ~ ., 
  data = pumpkin_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)
knn_fit
# Comments
# We see that the best model for this run (with seed 123) is associated with 
# k = 2, which resulted in a cv-RMSE of 22.37 
knn_fit$resample$RMSE
# Here RMSE was used to select the optimal model using the smallest value. 
# We can use another metric (e.g. MSE) for model selection. Try to change the 
# code and display the the result.  

# Extra code    
# Plot cv-error
ggplot(knn_fit)

# Test error (on test data)
pred = predict(knn_fit, newdata=pumpkin_test)
pred
test_error = sqrt(mean((pumpkin_test$price - pred)^2))
test_error
# Comments  
# [1] test error = 26.177 is close to the cv-error (43607.26). 
# On average the algorithm overestimate and underestimate the value of houses 
# by approx. 42.000 - 43.000$. Can we do something to improve the performance? 
# We will see next lecture how feature engineering may improve the algorithm 
# performance

# If we want to get the training error for k = 7
pred_train = predict(knn_fit, newdata=pumpkin_train)
pred_train
train_error_train = sqrt(mean((pumpkin_train$price - pred_train)^2))
train_error_train
# Comments
# As expected it is lower than test error, maybe slightly significant. 
# => There might be a sign of overfitting. 

