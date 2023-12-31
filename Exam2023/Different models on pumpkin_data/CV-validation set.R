#Load data and clean it:
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

library(dplyr)
pumpkin_data <- pumpkin %>%
  select(c(City.Name, Variety, Date, Low.Price, High.Price, Item.Size))

#Question b:
pumpkin_data <- pumpkin_data %>%
  mutate(Price = (Low.Price+High.Price)/2,
         Spread = 5+(High.Price-Low.Price)) %>%
  select(-c(Low.Price, High.Price))

library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

pumpkin_data$month <- as.factor(pumpkin_data$month)
pumpkin_data$year <- as.factor(pumpkin_data$year)

pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)
sum(is.na(pumpkin_data))

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(pumpkin_data),
                  replace = TRUE)
test <- (!train)

#Now, we apply regsubsets() to the training set in order to perform best
#subset selection.
library(leaps)
regfit.best <- regsubsets(Price~.,
                          data = pumpkin_data[train, ], nvmax = 40)

#We now compute the validation set error for the best
#model of each model size. We first make a model matrix from the test
#data
test.mat <- model.matrix(Price~., data = pumpkin_data[test, ])

#The model.matrix() function is used in many regression packages for build- model.matrix() ing an “X” matrix from data. Now we run a loop, and for each size i, we
#extract the coefcients from regfit.best for the best model of that size,
#multiply them into the appropriate columns of the test model matrix to
#form the predictions, and compute the test MSE.
val.errors <- rep(NA, 40)
 for (i in 1:40) {
   coefi <- coef(regfit.best, id = i)
   pred <- test.mat[, names(coefi)] %*% coefi
   val.errors[i] <- mean((pumpkin_data$Price[test] - pred)^2)
}
which.min(val.errors)
#We find that the best model is the one that contains seven variables.
coef(regfit.best, 35)

##Now we apply k-fold CV with k=10
cv.error.10<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:5){
  glm.fit<-glm(price~poly(Package,i), data=pumpkin)  
  cv.error.10[i]<-cv.glm(pumpkin, glm.fit, K=10)$delta[1] ##we set K=10 and chose delta[1]  
}
cv.error.10   #here result is produced very quickly.

#####################lets try with CV on regsubsets ####################################:
k <- 10
n <- nrow(pumpkin_data)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 13,
                  dimnames = list(NULL, paste(1:13)))

#Unfortunately regsubsets() does not have built-in predict function.
#So we write one for us below. You may suppose this as given.

predict.regsubsets<-function(object, newdata, id){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form, newdata)
  coefi<-coef(object, id=id)
  xvars<-names(coefi)
  mat[, xvars]%*%coefi
}

#Now we write a for loop that performs cross-validation. In the jth fold, the
#elements of folds that equal j are in the test set, and the remainder are in
#the training set.
for (j in 1:k) {
   best.fit <- regsubsets(Price~.,
                           data = pumpkin_data[folds != j, ],
                           nvmax = 13)
   for (i in 1:13) {
     pred <- predict(best.fit, pumpkin_data[folds == j, ], id = i)
     cv.errors[j, i] <-
      mean((pumpkin_data$Price[folds == j] - pred)^2)
   }
}
#This has given us a 10×7 matrix
#Now, we calculate average MSE across 10 folds
mean.cv.errors<-apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)  #This yield that 10-variable model has minimum CV error

plot(mean.cv.errors, type="b")  # we can also this graphically

#We see that cross-validation selects a 13-variable model. We now perform
#best subset selection on the full data set in order to obtain the 10-variable model.
reg.cv<-regsubsets(Price~., nvmax=13, data=pumpkin_data)
coef(reg.cv, 13)