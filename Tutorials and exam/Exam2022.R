####_________________________________________________
#### Fall2022, Machine Learning for BI 1, Final oExam 
####_________________________________________________
#### Sample Solutions and guidelines for grading 
# 1) For the evaluation, it is important that conclusions are stated explicitly.
# 2) This guiding solution has been prepared to assist the lecturer when 
#    evaluating the exam. 
#    All details are not included. Alternative solutions exist. 
#    The solutions can depend on the random seed and updates of R libraries. 

#_______________________________________________________________________________
### Sample solution Problem 1, Ana Alina T.
#_______________________________________________________________________________
library(dplyr)  
library(ggplot2)  
library(visdat)   
library(caret)    
library(recipes)  
library(rsample)
library(forecast)
library(DataExplorer)
library(ggplot2) 
library(visdat)
library (Hmisc) 
library(Metrics)

# 1. Data preparation
# a) 
data <- read.csv("~/Cloud/Documents/Alina Tudoran/TEACHING/Postgraduate/Machine Learning 2020-2021/ML1/2023/New lectures/4. Ch4_Classification/CASE STUDIES/Case Study Exam 2022/data.csv", stringsAsFactors=TRUE)
str(data)
data$Year <- factor(data$Year)
data$Model <- NULL

set.seed(123)
split <- initial_split(data, prop = 0.7, strata = "MSRP")
data_train  <- training(split)
data_test   <- testing(split)

# b)
data_recipe <- recipe(MSRP ~ ., data = data_train) %>%
  step_impute_knn(all_predictors(), neighbors = 6)%>%
  step_YeoJohnson(all_numeric(), -all_outcomes())%>%  
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), one_hot = FALSE) %>%
  step_nzv(all_nominal(), all_numeric())
prepare <- prep(data_recipe, training = data_train)
baked_train <- bake(prepare, new_data = data_train)
baked_test <- bake(prepare, new_data = data_test)

dim(baked_train)
dim(baked_test)
# the number of features has increased 
# it is expected to affect the time of model convergence 


# 2. Data analysis 
# a) 
knn_fit <- train(log(MSRP) ~ .,
                 data = baked_train, 
                 method = "knn", 
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = expand.grid(k = seq(2, 10, by = 1)))
knn_fit
ggplot(knn_fit)


lm <- train(log(MSRP) ~ .,
            data = baked_train, 
            method = "lm",
            trControl = trainControl(method = "cv", number = 10))

pcr <- train(log(MSRP) ~ .,
             data = baked_train, 
             method = "pcr",
             trControl = trainControl(method = "cv", number = 10),
             tuneLength = 43)

pls <- train(
  log(MSRP) ~ .,
  data = baked_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 43)

# b) 
results <- resamples(list(knn = knn_fit, 
                          lm = lm,
                          pcr = pcr,
                          pls = pls))
summary(results)
bwplot(results)
dotplot(results)
# selection of best model required  
# it can depend on random seed


# c) 
# if best model is pls
predictions_train <- predict(pls, baked_train) 
rmse(log(data_train$MSRP), predictions_train)
# train RMSE is approx. 0.62
# compared with cv-RMSEA model, there is no sign of overfitting


# d) 
predictions <- predict(pls, baked_test) 
rmse(log(data_test$MSRP), predictions)
# approx 0.61
# close to cv-RMSEA as expected


# 3)
residuals_train = data_train$MSRP - exp(predictions_train)
plot(residuals_train)
# 3-5 outliers are visible in my current output, 
# but it depends on the random seed)
# Recommendation:  identifying and commenting on the outliers
# Note: students can make other valid recommendations  
# an example: 
df <- data.frame(actual = data_train$MSRP, predicted = exp(predictions_train), residuals_train)
df[which.max(df$residuals_train),] 
#       actual predicted residuals_train
#7967 2065902   1006846         1059056
data_train[7967,] 
#         Make Year            Engine.Fuel.Type Engine.HP Engine.Cylinders Transmission.Type
#11363 Bugatti 2008 premium unleaded (required)      1001               16  AUTOMATED_MANUAL
#         Driven_Wheels Number.of.Doors         Market.Category Vehicle.Size Vehicle.Style
#11363 all wheel drive               2 Exotic,High-Performance      Compact         Coupe
#       highway.MPG city.mpg Popularity    MSRP
#11363          14        8        820 2065902
# As a recommendation, one can delete the outlier and re-run the analysis 


# 3. Theory 
# a) the selection might have occurred: 
#      (a) by minimizing the prediction error using k-fold cross-validation
#      (b) based on the proportion of variance explained 
#      (c) based on eigenvalues >1 
#      (d) elbow rule

# b)   (a) the selection criteria in cross-validated RMSE was minimized for that number of PC, or
#      (b) the reduction in RMSE was so small that it does not justify the inclusion of all the variables.

# c)    possibly overfitting and worse performance when modeling with new data. 



#_______________________________________________________________________________
### Solutions Problem 2 - Phillip H.
#_______________________________________________________________________________
setwd()
#################
##Problem 2.1 ###
#################

#2.1.a) Load and transform
data0 <- read.csv("data.csv", header = TRUE)
data0 <- data0[data0$Year > 2014,]
data0 <- data0[,!colnames(data0)%in%c("Year","MSRP","Model","Make")]
data0 <- data0[rowSums(is.na(data0)) == 0,]
data <- data0

#2.1.b)
lab.f <- c("Engine.Fuel.Type","Engine.Cylinders","Transmission.Type",
           "Driven_Wheels","Number.of.Doors","Market.Category","Vehicle.Size","Vehicle.Style")
for (lab in lab.f){
  print(lab)
  data[[lab]] <- factor(data[[lab]])
}


#2.1.c)
data$popular <- 1*(data0$Popularity > 3000)
data <- data[,!colnames(data)%in%c("Popularity")] 
#recode cylinders (alternative: add + drop)
data[["Engine.Cylinders"]] <- factor((as.numeric(data$`Engine.Cylinders`) > 5)*1,levels = c(0,1), labels = c("below/equal 5","above 5"))
colnames(data)[3] <- "cylinder"

data <- data[,!colnames(data)%in%c("Popularity")] 
#mean(data$popular)

#################
##Problem 2.2 ###
#################

#2.2.1)


#logit
mod1.logit <- glm(popular ~ .,family = binomial, data = data)
y_train.logit <- predict(mod1.logit,type="response")
hist(y_train.logit,300)

#restricted logit/always predict max category
mod2.logit <- glm(popular ~ 1, family = binomial, data = data)

acc_logit <- mean((y_train.logit > 0.5) == data$popular)
acc_mod2 <- mean(0 == data$popular)

#compare LL+acc
logLik(mod1.logit)
logLik(mod2.logit)
acc_logit <- mean((y_train.logit > 0.5) == data$popular)
acc_mod2 <- mean(0 == data$popular)
c(acc_logit,acc_mod2)

#interpretation
#acc of mean model = average of dependent variable by construction
#logit better in-sample fit according to both criteria. 
#logLik must be better for logit by construction as it is the empirical risk function and the constant model is nested by logit

#2.2.1) predictor matrix
x <- model.matrix(mod1.logit)[,!is.na(coefficients(mod1.logit))][,-1]
y <- data$popular

#2.2.3) BMA
#1)BIC is an model selection technique that picks a model with highest (posterior) probability
#of being the true model. Here, the predictor matrix has p = ncol(x) = 98 predictors. Thus, an exhaustive
#BIC selection for all models is computationally infeasible. However, for submodels, we could select with BIC
#weights. Example code (using only 8 regressors): 
#df_short <- data.frame("x" = x[,1:8], "popular" = y)
#m.BIC <- mami(df_short, outcome = "popular", model = "binomial",method = "MS.criterion", criterion = "BIC")

#1) JMA generates prediction optimal combinations of linear regression models
#based on leave-one-out cross-validation. 
#Exhaustive JMA is also computationally infeasible for high predictor (p > 20) dimensions
#even though the original data set had "only" 12 predictors. 
#2) More importantly, JMA is only for linear models, i.e. uses an wrong (MSE) 
#loss function for classification or low risk prediction of a binomial distribution. 


#2.2.4) Split
set.seed(1)

n <- nrow(data)
ind <- sample(seq(1,n),round(0.75*n))
y_train <- y[ind]
x_train <-  x[ind,]
y_test <- y[!seq(1,n)%in%ind]
x_test <- x[!seq(1,n)%in%ind,]
df.train <- data.frame("y"=y_train,"x"=x_train)
df.test <- data.frame("y"=y_test,"x"=x_test)

#2.2.5)
library(glmnet)
set.seed(1)
cv.enet <- cv.glmnet(x=x_train,y=y_train,alpha=0.5, nfolds = 5, 
                     type.measure = "deviance", family = "binomial") 
mod1.enet <- glmnet(x_train,y_train,family = "binomial",alpha=0.5,lambda = cv.enet$lambda.min)
yhat.enet <- predict(mod1.enet,newx = x_test, type = "response") 

cv.l2 <- cv.glmnet(x=x_train,y=y_train,alpha=0, nfolds = 5,
                   type.measure = "deviance", family = "binomial")
mod1.l2 <- glmnet(x_train,y_train,family = "binomial",alpha=0,lambda = cv.l2$lambda.min)
yhat.l2 <- predict(mod1.l2,newx = x_test, type = "response")
# 
# plot(yhat.enet,yhat.l2)

#test set LL (note, this potentially requires some care for the extreme p case)
#create LL function: 
f.loglik <- function(y,p){
  ll <- (sum( log(p[y==1])) + sum( log(1-p[y==0]) ))/length(y)
  return(ll)
}

#check whether 1/0 adaption is necessary (not asked)
#min(yhat.enet[y_test == 1])
#max(yhat.enet[y_test == 0])

ll.enet <- f.loglik(y_test,yhat.enet)
ll.l2 <- f.loglik(y_test,yhat.l2)
c(ll.enet,ll.l2)

#test set acc
#predictions:
acc_enet <- mean((yhat.enet > 0.5) == y_test)
acc_l2 <- mean((yhat.l2 > 0.5) == y_test)

c(acc_enet,acc_l2)

#interpretation:
#enet is slightly preferred (larger likelihood and accuracy) - could depend on random seed!

#2.1.6)
library(SuperLearner) 
#we choose 3 methods
#listWrappers()
SL.methods <- c('SL.mean','SL.glmnet', 'SL.glm')

X_train <- as.data.frame(x_train)
colnames(X_train) <- as.character(seq(1,ncol(x_train)))
X_test <- as.data.frame(x_test)
colnames(X_test) <- as.character(seq(1,ncol(x_train)))

#note: all fold sizes V > 2 (including default = 10) are permitted:
model.SL <- SuperLearner(Y=y_train, X=X_train, SL.library = SL.methods, 
                         family = binomial(link = "logit"))

#get model averaging weights 
model.SL$coef
#note: can depend on random seed:   
#SL.mean_All SL.glmnet_All    SL.glm_All 
#  0.0259362     0.4895806     0.4844832

# predictions you can use predict():
yhat.SL <- predict(model.SL, newdata = X_test, type= "response")$pred

# acc
acc_SL <- mean((yhat.SL > 0.5) == y_test)
c(acc_SL,acc_enet,acc_l2)
#LL
ll.SL <- f.loglik(y_test,yhat.SL)
c(ll.SL,ll.enet,ll.l2)

#Discussion (note: can depend on random seed): 
#Accuracy SL/enet > l2 > constant
#LL: SL > ENET > L2 > constant. 
#SL overall slightly preferable.

#Intuition super learner: Combinations of all predicted probabilities based 
# on k-fold CV weights that minimize CV-error 
# (here of the binomial likelihood fct). 
