#Question a:####
library(dplyr)
pumpkin_data <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

#Question b:####
str(pumpkin_data)
dim(pumpkin_data)
View(pumpkin_data[1:5, ])

#Question c:####
pumpkin_data <- pumpkin_data %>%
  select(-c(X, X.1))

#Question d:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
          year = year(Date)) %>%
  select(-Date)

#Question e:####
library(visdat)
library(DataExplorer)
pumpkin_data[pumpkin_data==""] <- NA
sum(is.na(pumpkin_data))

# Visually (e.g. plot)
vis_miss(pumpkin_data, cluster = TRUE) #visdat library
plot_missing(pumpkin_data)
#Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment and Grade have all 100% missing values
#in this dataset. This means that they have absolutely zero affect on the future predictions, and it is best to
#drop these variables.

pumpkin_data <- pumpkin_data %>%
  select(-c(Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment, Grade, Sub.Variety, Unit.of.Sale, Origin.District, Type))

#Checking for missing values again:
plot_missing(pumpkin_data)
#Two variables Mostly.High and Mostly.low have 5.86% missing values, which is ok in this case. But i will apply
#knn-imputation because it looks like the prices are in some sort of range. The prices are integer and the
#imputation should be so.

#Question f:####
# Create a new column for average Price
pumpkin_data <- pumpkin_data %>% 
  mutate(price = (Low.Price + High.Price)/2)

#Check for appropriate normalization:
#First checking for the distribution of price without any transformation:
par(mfrow=c(1,3))
hist(pumpkin_data$price, breaks = 20, col = "red", border = "red") 
summary(pumpkin_data$price)
#Highly skewed distribution with almost 400 observations with a price of almost 0 (0.24 as the lowest price). That
#must be a pumpkin of a very poor quality.

#Log transformation:
log_pumpkin <- log(pumpkin_data$price)

# Box-Cox transformation (lambda=0 is equivalent to log(x))
library(forecast)
BC_pumpkin <- forecast::BoxCox(pumpkin_data$price, 
                                            lambda="auto") 

hist(log_pumpkin, breaks = 20, 
     col = "lightgreen", border = "lightgreen",
     ylim = c(0, 800) )
hist(BC_pumpkin, breaks = 20, 
     col = "lightblue", border = "lightblue", 
     ylim = c(0, 800))

#The box-cox transformation seems to be the most normalized distribution. The range of the prices are not as
#abnormal as with no transformation.

#Question g:####
library(caret)
caret::nearZeroVar(pumpkin_data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
#Later in the recipe variables such as Type, Origin.District and repack will be eliminated. In the whole dataset,
#these three predictors are classified as near zero variance, but later when the data is split up into train/test
#or CV/bootstrap.

#Question h:####
library(ggplot2)
library(DataExplorer)
library (GGally)

str(pumpkin_data)
pairs(pumpkin_data[,c(4:7, 12:14)])
ggpairs(pumpkin_data, columns = c(4:7, 12:14))

plot_histogram(pumpkin_data)

pumpkin_data %>%
  group_by(month) %>% 
  summarise(price = mean(price)) %>% 
  ggplot(aes(x = month, y = price)) +
  geom_col(fill = "midnightblue", alpha = 0.7) +
  ylab("Pumpkin Price")

#Question j:####
#Splitting the data:
# data split
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks
library(rsample)

set.seed(123)
split <- initial_split(pumpkin_data, prop = 0.8, strata = "price") 
pumpkin_train <- training(split)
pumpkin_test <- testing(split)

pumpkin_recipe <- recipe(price ~ ., data = pumpkin_train) %>%
  step_impute_knn(all_predictors(), neighbors = 6) %>%
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(all_predictors(), zero_based = F) %>%  
  #step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_BoxCox(all_outcomes()) %>% 
  step_nzv(all_predictors(), -all_outcomes()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

prepare <- prep(pumpkin_recipe, training = pumpkin_train)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = pumpkin_train)
baked_test <- bake(prepare, new_data = pumpkin_test)
baked_train
baked_test

#Question k:####
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1)

# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
pumpkin.knn <- train(
  pumpkin_recipe, 
  data = pumpkin_train, 
  method = "knn", 
  trControl = cv, 
  #tuneGrid = hyper_grid,
  metric = "RMSE"
)
pumpkin.knn

# new data (test)
predictions_reg <- predict(pumpkin.reg, pumpkin_test)
test_RMSE_reg =sqrt(mean((log(pumpkin_test$price) - predictions_reg)^2))
test_RMSE_reg



