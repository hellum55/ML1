library(writexl)
write_csv(pumpkin_data,"~/ML1/Exam2023\\pumpkintrain.csv")

#Question a:####
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

#Question b:####
str(pumpkin)
dim(pumpkin)
View(pumpkin[1:5, ])

#Question c:####
library(dplyr)
pumpkin_data <- pumpkin %>%
  select(-c(X, X.1))

#Question d:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
          year = year(Date)) %>%
  select(-Date)

#It makes sense to treat the dates as factors rather than numeric features because the data could have been in months like "Jan",
#"Feb", "Mar" and so on. Treat them as factors:
pumpkin_data$month <- as.factor(pumpkin_data$month)
pumpkin_data$year <- as.factor(pumpkin_data$year)
str(pumpkin_data)

#Question e:####
library(visdat)
library(DataExplorer)
#pumpkin_data[pumpkin_data==""] <- NA
sum(is.na(pumpkin_data))

# Visually (e.g. plot)
vis_miss(pumpkin_data, cluster = TRUE) #visdat library
plot_missing(pumpkin_data)
#Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment and Grade have all 100% missing values
#in this dataset. This means that they have absolutely zero effect on the future predictions, and it is best to
#drop these variables since it is rows without a single datapoint. 

pumpkin_data <- pumpkin_data %>%
  select(-c(Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment, Grade, Sub.Variety, Unit.of.Sale, Origin.District, Type))

#Checking for missing values again:
library (Hmisc)
plot_missing(pumpkin_data)
vis_miss(pumpkin_data, cluster = TRUE)

#Two variables Mostly.High and Mostly.low have 5.86% missing values, which is ok in this case. But i will apply
#knn-imputation because it looks like the prices are in some sort of range. The prices are integer and the
#imputation should be so.
#It seems like the missing values of the coloumns are missing at random. No generally pattern or relationship
#between the missingness. It is MCAR. If we later on apply mean imputation we do not preserve the realtionshiop between variables.
#and therefor underestimates SE and making type 1 errors. 
#The best method might be KNN even though it is a bit heavier on computation time.

#Question f:####
# Create a new column for average Price
pumpkin_data <- pumpkin_data %>% 
  mutate(price = (Low.Price + High.Price)/2) %>%
  select(-Low.Price, -High.Price, -Mostly.Low, -Mostly.High)

#Check for appropriate normalization:
#First checking for the distribution of price without any transformation:
par(mfrow=c(1,3))
hist(pumpkin_data$price, breaks = 20, col = "red", border = "red") 
no_transform <- pumpkin_data$price
#Highly skewed distribution with almost 400 observations with a price of almost 0 (0.24 as the lowest price). That
#must be a pumpkin of a very poor quality.

#Log transformation:
log_pumpkin <- log(pumpkin_data$price)

# Box-Cox transformation (lambda=0 is equivalent to log(x))
library(forecast)
pumpkin_BC <- forecast::BoxCox(pumpkin_data$price, lambda="auto") 

hist(log_pumpkin, breaks = 20, 
     col = "lightgreen", border = "lightgreen",
     ylim = c(0, 800) )
hist(pumpkin_BC, breaks = 20, 
     col = "lightblue", border = "lightblue", 
     ylim = c(0, 800))

list(summary(no_transform),
summary(log_pumpkin),
summary(pumpkin_BC))

#It is hard to tell which of these normalizations are better, because they are all terrible. 
#Maybe the log transformation are better. The box-cox transformation seems to be the most normalized distribution. The range of the prices are not as
#abnormal as with no transformation.

#Question g:####
library(caret)
caret::nearZeroVar(pumpkin_data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
#Repack is a near zero variance variable. It makes sense when you have a look at the data. It Is
#almost the same observation for the whole dataset and thereby does not really help to predict the prices
#later on.

#Question h:####
library(ggplot2)
library(DataExplorer)
library (GGally)
#This is a weird question. We have been told to delete almost all numeric features until now.
#and the only numeric features such as month and year are treated as factors now. So which numeric features are in play?
#Actually not a single when besides the DP.

str(pumpkin_data)
pairs(pumpkin_data[,c(4:7, 12:14)])
ggpairs(pumpkin_data, columns = c(4:7, 12:14))

#Question i:
plot_bar(pumpkin_data)
count(pumpkin_data, Origin) %>% arrange(n)
count(pumpkin_data, Package) %>% arrange(n)

#check to see if the LR assumptions are fullfilled.
test_lm <- lm(formula = log_pumpkin ~ Package+Item.Size+City.Name+Variety+Origin+Color+month+year, data = pumpkin_data)
summary(test_lm)
par(mfrow=c(2,2))
plot(test_lm)

#We have alredy concluded that Repack should be eliminated due to near zero variance (almost all observations are N). The Year feature
#Shows that there are a couple of observations from 2014, which we might delete from the dataset beacuse the dataset are supposed to contain
#only the years 2016 - 2017. Variables such as package and origin we might consider to lump the very few observations.
#We can benefit from lumping if we lump these few observations into fewer categories. We can try to lump categories into fewer if the observations are
#are observed less than 5% of the time for the package variable and the origin. 

#Question j:####
#Splitting the data:
# data split
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks
library(rsample)

set.seed(123)
split <- initial_split(pumpkin_data, prop = 0.7, strata = "price") 
pumpkin_train <- training(split)
pumpkin_test <- testing(split)

pumpkin_recipe <- recipe(price ~ ., data = pumpkin_train) %>%
  step_impute_knn(all_predictors(), neighbors = 6) %>%
  step_BoxCox(price) %>%
  step_other(Package, threshold = 0.1, other = "other") %>%
  step_other(Origin, threshold = 0.1, other = "other") %>%
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(Item.Size) %>%
  step_center(all_integer_predictors()) %>%
  step_scale(all_integer(), -all_outcomes()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors(), -all_outcomes())

prepare <- prep(pumpkin_recipe, training = pumpkin_train)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = pumpkin_train)
baked_test <- bake(prepare, new_data = pumpkin_test)
dim(baked_train)
dim(baked_test)

#Question k:####
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 1)

cv_model_ols <- train(
  (price)~.,      
  data = baked_train, 
  method = "lm", 
  trControl = cv, 
  metric = "RMSE"
)
cv_model_ols
#  RMSE      Rsquared   MAE    
# 6.00154  0.7541309  3.581401
plot(cv_model_ols$finalModel)

#PCR regression
set.seed(123)
cv_model_pcr <- train(
  (price)~., 
  data = baked_train, 
  method = "pcr",
  trControl = cv,
  tuneLength = 30,
  metric = "RMSE"
)
# model with lowest RMSE
cv_model_pcr$bestTune
##    ncomp
## 23    23

# results for model with lowest RMSE
cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))
##   ncomp  RMSE  Rsquared   MAE   
## 1  23 5.958849 0.7537162 3.552391

# plot cross-validated RMSE
ggplot(cv_model_pcr)
summary(cv_model_pcr)
#the plot looks a bit weird. The model with 45 components yield the smallest RMSE
#but there is not much difference between a model with 5 components and 45. We could benefit
#from the model with 5 components because it is a simpler model. It looks like it is just a single variable
#that can explain the prices such as the package size. The bigger the packages the higher the price.

#PLS regression
# number of principal components to use as predictors from 1-30
set.seed(123)
cv_model_pls <- train(
  (price)~., 
  data = baked_train, 
  method = "pls",
  trControl = cv,
  tuneLength = 30,
  metric = "RMSE"
)
# model with lowest RMSE
cv_model_pls$bestTune
##    ncomp
## 11    11

# results for model with lowest RMSE
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
##   ncomp  RMSE    Rsquared  MAE     RMSESD 
## 1   12 5.934838 0.7568023 3.542692 0.8717315

# plot cross-validated RMSE
ggplot(cv_model_pls)
#Kind of the same issue with this one.

#KNN-regression:
# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 30, by = 1))

cv_knn_model <- train(
  (price)~.,      
  data = baked_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)
cv_knn_model
#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 2.

ggplot(cv_knn_model)

# Question l:####_______________________________
# Summary of cv-error for the models tested here
# ______________________________________________
results <- summary(resamples(list(
  model1 = cv_model_ols,
  model2 = cv_model_pcr, 
  model3 = cv_model_pls,
  model4 = cv_knn_model
)))
results
#The model with lowest RMSE and highest R^2 is the knn-model

#Question m:####
# new data (test)
predictions <- predict(cv_knn_model, baked_test) 
predictions 
price_BC <- forecast::BoxCox(pumpkin_test$price, lambda="auto") 
RMSE_knn = sqrt(mean(price_BC - predictions)^2)
RMSE_knn

#the predicted RMSE on the testset is: 19.43 RMSE which is way higher than
#the in-sample test. hmm....?


#Question n:####
library(vip)
vip(cv_model_pls, num_features = 10, geom = "point")
vip::vip(cv_knn_model, method = "model")
#Of course the package has a huge influence on price. The bigger the package
#The more expensive. We should consider to transform the packages so it is actually in the same unit or same size. 
#We dont even need a model to predict a price point because it is common sense that if the package is bigger the pumpkin
#is more expensive. 

#Question o:####
#Clean dirty data. First of all i would recommend to deal with the data in the same unit because the package variable has very high importance in estimating CV-error.
#Otherwise get rid of the very small values like 0.2 in the price variable because they are often sold as just one pumpkin. There are also a couple of very high prices we could try to get rid off.
#so the distribution are more normally distributed or atlest when we try to transform it.
#Checking for outliers:

predictions_train <- predict(cv_model_pls, baked_train) 
residuals_train = pumpkin_train$price - exp(predictions_train)
plot(residuals_train)

df <- data.frame(actual = pumpkin_train$price, predicted = exp(predictions_train), residuals_train)
df[which.max(df$residuals_train),] 
pumpkin_train[1182,] 
#The above could be removed because it is an outlier and can be noisy regarding the analysis. The package is the type 'each' which means the 
#package contains one pumpkin. In this category the pumpkins are often very low in price like 0.2 - 3 dollars. This observation are more than 100x
#the usual price for a pumpkin in the 'each' category. Therefor, remove it!

#Remove row 1182:
pumpkin_train <- pumpkin_train[-(1182:1186);, ]

library(gridExtra)   
# 1. histogram, Q-Q plot, and boxplot
par(mfrow = c(1, 3))
hist(pumpkin_train$price, main = "Histogram")
boxplot(pumpkin_train$price, main = "Boxplot")
qqnorm(pumpkin_train$price, main = "Normal Q-Q plot")

# 2. Mean and SD
# get mean and standard deviation
mean = mean(pumpkin_train$price)
std = sd(pumpkin_train$price)

# get threshold values for outliers
Tmin = mean-(3*std)
Tmax = mean+(3*std)

# find outliers
pumpkin_train$price[which(pumpkin_train$price < Tmin | pumpkin_train$price > Tmax)]
# [1] 400 400 440 480 440 440

# remove outliers
pumpkin_train$price[which(pumpkin_train$price > Tmin & pumpkin_train$price < Tmax)]

pumpkin_recipe <- recipe(price ~ ., data = pumpkin_train) %>%
  step_impute_knn(all_predictors(), neighbors = 6) %>%
  step_BoxCox(price) %>%
  step_other(Package, threshold = 0.1, other = "other") %>%
  step_other(Origin, threshold = 0.1, other = "other") %>%
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(Item.Size) %>%
  step_center(all_integer_predictors()) %>%
  step_scale(all_integer(), -all_outcomes()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors(), -all_outcomes())

prepare <- prep(pumpkin_recipe, training = pumpkin_train)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = pumpkin_train)
baked_test <- bake(prepare, new_data = pumpkin_test)

#PLS regression
# number of principal components to use as predictors from 1-30
set.seed(123)
cv_modified_pls <- train(
  (price)~., 
  data = baked_train, 
  method = "pls",
  trControl = cv,
  tuneLength = 30,
  metric = "RMSE"
)
# model with lowest RMSE
cv_modified_pls$bestTune
##    ncomp
## 13   13

# results for model with lowest RMSE
cv_modified_pls$results %>%
  dplyr::filter(ncomp == pull(cv_modified_pls$bestTune))
##   ncomp  RMSE    Rsquared  MAE     RMSESD 
## 1   13 5.325916 0.8007857 3.372133 0.5948893

# plot cross-validated RMSE
ggplot(cv_modified_pls)
#Kind of the same issue with this one.

#KNN-regression:
# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 30, by = 1))

cv_knn_modified <- train(
  (price)~.,      
  data = baked_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)
cv_knn_modified
#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 2.

