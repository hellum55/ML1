library(writexl)
write_xlsx(pumpkin_data,"~/ML1/Exam2023\\pumpkintrain.xlsx")

#Question a:####
library(dplyr)
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

#Question b:####
str(pumpkin_data)
dim(pumpkin_data)
View(pumpkin_data[1:5, ])

#Question c:####
pumpkin_data <- pumpkin %>%
  select(-c(X, X.1))

#Question d:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
          year = year(Date)) %>%
  select(-Date)

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
#in this dataset. This means that they have absolutely zero affect on the future predictions, and it is best to
#drop these variables.

pumpkin_data <- pumpkin_data %>%
  select(-c(Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment, Grade, Sub.Variety, Unit.of.Sale, Origin.District, Type))

#Checking for missing values again:
library (Hmisc)
plot_missing(pumpkin_data)

# mutate missing values manually (not needed in this case)
pumpkin_data <- pumpkin_data %>%
  mutate(Mostly.High
         = replace(Mostly.High,
                   is.na(Mostly.High),
                   mean(Mostly.High, na.rm = TRUE)),
         Mostly.Low
         = replace(Mostly.Low,
                   is.na(Mostly.Low),
                   mean(Mostly.Low, na.rm = TRUE)))
#Two variables Mostly.High and Mostly.low have 5.86% missing values, which is ok in this case. But i will apply
#knn-imputation because it looks like the prices are in some sort of range. The prices are integer and the
#imputation should be so.

pumpkin_data$Item.Size <- impute(pumpkin_data$Item.Size)
pumpkin_data$Color <- impute(pumpkin_data$Color)
pumpkin_data$Variety <- impute(pumpkin_data$Variety)
pumpkin_data$Origin <- impute(pumpkin_data$Origin)

plot_missing(pumpkin_data)

#Question f:####
# Create a new column for average Price
pumpkin_data <- pumpkin_data %>% 
  mutate(price = (Low.Price + High.Price)/2) %>%
  select(-Low.Price, -High.Price, -Mostly.Low, -Mostly.High)

# Retain only pumpkins with the string "bushel"
#pumpkin_data <- pumpkin_data %>% 
#  filter(str_detect(string = Package, pattern = "bushel"))

# Normalize the pricing so that you show the pricing per bushel, not per 1 1/9 or 1/2 bushel
#pumpkin_data <- pumpkin_data %>% 
#  mutate(price = case_when(
#    str_detect(Package, "1 1/9") ~ price/(1.1),
#    str_detect(Package, "1/2") ~ price*2,
#    TRUE ~ price))

#Check for outliers:
attach(pumpkin_data)
plot(month, price, main="Scatterplot Example",
     xlab="months ", ylab="price ", pch=19) # set number of bins

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
pumpkin_BC <- forecast::BoxCox(pumpkin_data$price, lambda="auto") 

hist(log_pumpkin, breaks = 20, 
     col = "lightgreen", border = "lightgreen",
     ylim = c(0, 800) )
hist(pumpkin_BC, breaks = 20, 
     col = "lightblue", border = "lightblue", 
     ylim = c(0, 800))

#The box-cox transformation seems to be the most normalized distribution. The range of the prices are not as
#abnormal as with no transformation.

#Question g:####
library(caret)
caret::nearZeroVar(pumpkin_data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)

#manually dropping NZV variable (not needed in this case)
pumpkin_data <- pumpkin_data %>%
  select(-c(Repack))
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


plot_bar(pumpkin_data)

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
  step_impute_knn(all_predictors()) %>%
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(Item.Size, Variety, Color, month, year) %>%
  step_BoxCox(all_numeric(), -Color, -year) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors())

prepare <- prep(pumpkin_recipe, training = pumpkin_train)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = pumpkin_train)
baked_test <- bake(prepare, new_data = pumpkin_test)
baked_train
baked_test
str(pumpkin_train)

#Check for correlation:
library(corrplot)
# Obtain correlation matrix
corr_mat <- cor(baked_train)
corrplot(corr_mat, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", cl.pos = "n", order = "original")

#Very high correlations between mostly low and mostly high. delete these variables.

#Question k:####
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 2)

cv_model_ols <- train(
  price~.,      
  data = baked_train, 
  method = "lm", 
  trControl = cv, 
  metric = "RMSE"
)
cv_model_ols
#  RMSE      Rsquared   MAE    
# 6.048651  0.7759989  3.636813

#PCR regression
set.seed(123)
cv_model_pcr <- train(
  price~., 
  data = baked_train, 
  method = "pcr",
  trControl = cv,
  tuneLength = 55,
  metric = "RMSE"
)
# model with lowest RMSE
cv_model_pcr$bestTune
##    ncomp
## 27    27

# results for model with lowest RMSE
cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))
##   ncomp     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
## 1    25 6.53572 0.7378014 4.229481 0.8922528 0.05922489 0.4612548
# plot cross-validated RMSE
ggplot(cv_model_pcr)


#PLS regression
# number of principal components to use as predictors from 1-30
set.seed(123)
cv_model_pls <- train(
  price~., 
  data = baked_train, 
  method = "pls",
  trControl = cv,
  tuneLength = 70,
  metric = "RMSE"
)
# model with lowest RMSE
cv_model_pls$bestTune
##    ncomp
## 17    17

# results for model with lowest RMSE
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
##   ncomp     RMSE  Rsquared      MAE   RMSESD RsquaredSD   MAESD
## 1    11 1.531377 0.9857728 0.9651711 0.2396004 0.003704062 0.09061704

# plot cross-validated RMSE
ggplot(cv_model_pls)


#KNN-regression:
# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 70, by = 1))

cv_knn_model <- train(
  price~.,      
  data = baked_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)
cv_knn_model 
ggplot(cv_knn_model)

# Question l:####_______________________________
# Summary of cv-error for the models tested here
# ______________________________________________
results <- summary(resamples(list(
  model1 = cv_model_ols,
  model3 = cv_model_pcr, 
  model4 = cv_model_pls,
  model5 = cv_knn_model
)))

#Question m:####
# new data (test)

predictions <- predict(cv_knn_model, baked_test) 
predictions 
RMSE_knn = sqrt(sum((baked_test$price - predictions)^2/nrow(baked_test)))
RMSE_knn
#3.396111 RMSE

#Question n:####
library(vip)
install.packages("tune")
library(tune)
p1 <- vip::vip(cv_model_pcr, num_features = 10, bar = FALSE)

cv_model_pcr %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(fill = "steelblue"), num_features = 10)
#Of course the package has a huge influence on price. The bigger the package
#The more expensive. Of course low.price/high.price has a huge influence as well
#That´s how the price is computed. Maybe one should consider to normalize the price
#so the price reflects the same unit price. 

library(pdp)
pdp::partial(cv_model_pcr, "Item.Size", grid.resolution = 20, plot = TRUE)


