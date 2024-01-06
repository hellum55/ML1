####################################
# File: TA Problem Set 2 Guiding Solution
# Course: Machine Learning for BI 1 
# TA: Camille Pedersen
# Aarhus University, Fall 2023
####################################

# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations
library(tidyverse)

# Modelling packages
library(caret)    # for various ML tasks

# Feature engineering packages
library(recipes)  # for blueprint
library(rsample)  # for resampling
library(forecast) # for prediction
library(DataExplorer) # for visualization
library(ggplot2)     # for visualization
library(visdat)      # for visualization
library (Hmisc)      # for visualization


### 1. PROBLEM 1

# 1.1 
setwd("...path")
# save data.csv file in the same folder as your working directory

cars_df <- read.csv("...path/data.csv", stringsAsFactors=TRUE)
View(cars_df)
summary(cars_df)
t(variable.names(cars_df))


# 1.2
# check the variable type and their definition, unit of measurement , etc.
str(cars_df)
glimpse(cars_df) #From tidyverse library


# 1.3
nlevels(cars_df$Model) #Check number of unique levels for the Model variable (915)
cars_df$Model <- NULL #delete variable


# 1.4

# # Stratified sampling with the rsample package
set.seed(999)
split <- initial_split(cars_df, prop = 0.7, strata = "MSRP") 
data_train  <- training(split)
data_test   <- testing(split)


# 1.5

# Evaluate data_train$MSRP (DV) distribution and check various transformations 
par(mfrow=c(1,3))
hist(data_train$MSRP, breaks = 20, col = "red", border = "red") #highly skewed

# log-transformation (if all value are positive)
transformed_response <- log(data_train$MSRP)

# Box-Cox transformation (lambda=0 is equivalet to log(x))
transformed_response_BC <- forecast::BoxCox(data_train$MSRP, lambda="auto") # lambda auto

hist(transformed_response, breaks = 20, col = "lightgreen", border = "lightgreen")
hist(transformed_response_BC, breaks = 20, col = "lightblue", border = "lightblue")
# conclude about what transformation will be most appropriate to be implemented later in blueprint



# 1.6 
# Missing data

# Evaluate  missing data
sum(is.na(data_train))

# Visually (e.g. plot)
vis_miss(data_train, cluster = TRUE) #visdat library
plot_missing(data_train) #DataExplorer library

# Given the low proportion of missing data, we may decide either to delete or to impute them

# Options to manage missing data
  # 1. Create a new data set without missing values using na.omit() function 
    # data_without_missing <- na.omit(data_train)
    # dim(data_without_missing)
  # 2. Imputation
    # with KNN imputation for all predictors
      #data_recipe %>%
      #step_knnimpute(all_predictors(), neighbors = 6)
    # with bagged decision trees imputation
      #data_recipe %>%
      # step_bagimpute(all_predictors())


# 1.7
# Feature filtering 

caret::nearZeroVar(data_train, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)

# Conclude there is no nzv so far but they may appear later after feature engineering


# 1.8
# numeric feature engineering 
plot_histogram(data_train)

# based on this output , we can make individual data processing for each variable
# Alternatively, we will: 
# Normalize all numeric columns using the blueprint
  #  e.g., data_recipe %>% step_YeoJohnson(all_numeric())  
# Standardize all numeric values using the blueprint
  # e.g., data_recipe %>%
  # step_center(all_numeric(), -all_outcomes()) %>%
  # step_scale(all_numeric(), -all_outcomes())


# 1.9
# categorical feature engineering 
plot_bar(data_train)

# 1. Lumping (collapsing)
  # Check the factors, and conclude if lumping is necessary 
  # (some levels have very few observations) 
count(data_train, Make) %>% arrange(n)
count(data_train, Year) %>% arrange(n)
count(data_train, Engine.Fuel.Type) %>% arrange(n)
count(data_train, Transmission.Type) %>% arrange(n)
count(data_train, Driven_Wheels) %>% arrange(n)
count(data_train, Market.Category) %>% arrange(n)

# Only discussion: 
# to collapse all levels that are observed in less than 10% of the
# training sample into an “other” category, we will use step_other()
# e.g., Lump levels for two features example
#  lumping <- data_recipe %>%
#    step_other(Engine.Fuel.Type, threshold = 0.01, other = "other") %>%

# 2. Dummy encoding (step_dummy) of all factors
# Only discussion: below an example of one_hot encoding
# data_recipe %>%
# step_dummy(all_nominal(), one_hot = TRUE)


# 1.10
# Now let us see a full-implementation 
# create blueprint
data_recipe <- recipe(MSRP ~ ., data = data_train) %>%
  step_impute_knn(all_predictors(), neighbors = 6)%>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_nzv(all_nominal(), all_numeric())
data_recipe
# in the above, reflect and discuss on the order of the tasks and implications

# prepare: estimate feature engineering parameters based on training data
prepare <- prep(data_recipe, training = data_train)
prepare

# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = data_train)
baked_test <- bake(prepare, new_data = data_test)
dim(baked_train)
dim(baked_test)
# display the size of the new data sets 


# 1.11

# develop a blueprint within each sample iteration and implement knn
# be patient, it take some more time to run

# Specify re-sampling plan
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1
)

# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit <- train(
  data_recipe, 
  data = data_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

knn_fit
# RMSE was used to select the optimal model using the smallest value
# The final value used for the model was k = 2.
# RMSE = 19213.49





