#########################################################
# Course: Machine Learning for BI 1 
# Lecture: PCR and PLS regression 
# Ana Alina Tudoran
# Aarhus University, Fall 2023
#########################################################

# _____________________________________________________________________________
# We continue working with the ames data set as in previous applications. The 
# support material from this code is HOM, Ch.3.
# _____________________________________________________________________________

# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for cross-validation, etc.

# Model interpretability packages
library(vip)      # variable importance

# data
set.seed(123)
library(rsample)
ames <- AmesHousing::make_ames()
split <- initial_split(ames, prop = 0.7, strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)


# _____________________________________
## Principal component regression (PCR) 
# _____________________________________

# Method 1:  integrate PCA into the blueprint 
# using the command "step_pca(all_numeric())" (see Feature Engineering lecture)
# The command has 2 tuning parameters
#    - num_comp: # of components (default: 5)
#       e.g., step_pca(all_numeric(),-all_outcomes(), num_comp = 5)
#    - threshold: % of variance to retain (default: NA)
#       e.g., step_pca(all_numeric(),-all_outcomes(), threshold = .95)


# Method 2: specify method = "pcr" within train() in caret (see this lecture)
# In this case, we tune # of components  
#   e.g., tuneLength = 100


set.seed(12345)
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pcr", # see method here
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"), # an integrated method for 
  # basic data preprocessing (preProcess()) within caret
  tuneLength = 100 # see tuning parameter here
)

# model with lowest RMSE
cv_model_pcr$bestTune


# results for model with lowest RMSE
cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))

# plot cross-validated RMSE
plot(cv_model_pcr$results$RMSE)

# By controlling for multicollinearity with PCR, we experience 
# significant improvement in our predictive accuracy compared 
# to the previously obtained linear models.
# We reduce the cross-validated RMSE to nearly $ ...
# This is not a rule. Sometimes PCR can perform worse than other models. 


# ______________________________________
## Partial least squares regression (PLS)
# _________________________________________
# Similar to PCR, this technique also constructs a set of linear combinations 
# of the inputs for regression, but unlike PCR it uses the response variable  
# to aid the construction of the principal components
# PCR with caret library simply specify method = "pls" within train() 

# perform 10-fold cross validation on a PLS model tuning the 
# number of principal components to use as predictors
set.seed(234)
cv_model_pls <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)

# model with lowest RMSE
cv_model_pls$bestTune


# results for model with lowest RMSE
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))

#By controlling for multicollinearity with PLS, we experience 
# significant improvement in our predictive accuracy compared 
# to the previously obtained linear models 
# We reduce the cross-validated RMSE from about to nearly $ ...
# This is not a rule. Sometimes PLS can perform worse than other models. 


# plot cross-validated RMSE
plot(cv_model_pls$results$RMSE)


# ______________________________________________
# Summary of cv-error for the models tested here
# ______________________________________________
summary(resamples(list(
  model1 = cv_model_pcr, 
  model2 = cv_model_pls
)))


# _____________________________________________________________________
# Summary of of cv-error for all models tested here and in R file OLS.R
# _____________________________________________________________________
# ! to execute the following lines, first run the code from R file OLS.R
summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3,
  model4 = mod_wo_Garage_Cars,
  model5 = mod_wo_Garage_Area,
  model6 = model_withFE,
  model7 = cv_model_pcr,
  model8 = cv_model_pls
)))
# compare the models based on the output and conclude on the best model/algorithm


# ______________________
# Feature importance 
# ______________________
# For PLS and OLS, variable importance can be computed using vip library
# The importance measure is normalized from 100 (most important) to 0 (least important)
vip::vip(cv_model_pls, num_features = 20, method = "model")
# Go to the dataset description to understand the meaning of the labels (e.g.,type 
# ames in the search box and choose Raw Ames Housing Data).

# We can ask for feature importance in the OLS regression run in from R file OLS.R 
vip::vip(cv_model3, method = "model")
# notice, they are not the same features

# The command vip does not work for PCR.


# _______________________________________________
# # Evaluate the best model on new data (ames_test)
# ________________________________________________
predictions <- predict(cv_model_pls, ames_test) 
predictions 
# the test RMSEA 
RMSEA = sqrt(sum((ames_test$Sale_Price - predictions)^2/877))
RMSEA
#or simply
library(Metrics)
rmse(ames_test$Sale_Price, predictions)


