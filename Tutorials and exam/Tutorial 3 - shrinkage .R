#Load the data:####
#load data
lymphx <- read.table("lymphx.txt", quote="\"", comment.char="")
lymphstatus <- read.table("lymphstatus.txt", quote="\"", comment.char="")
#save data in matrix form 
x <- as.matrix(lymphx)
y <- as.matrix(lymphstatus)

prop.table(table(lymphstatus$V1))

#Question 2.2:####
library(caret)
library(glmnet)
glm.fits <- glm(y~x, data = lymphx, family = binomial)
summary(glm.fits)

#It does not work with so many predictors due to singularities.

#Apply CV nfolds = 10 and use log likelihood and share of predicted classes as loss function measure
set.seed(100)
cv.fits_dev <- cv.glmnet(x=x, y=y, alpha = 1, nfolds = 10, type.measure = "deviance", family = binomial)
cv.fits_class <- cv.glmnet(x=x, y=y, alpha = 1, nfolds = 10, type.measure = "class", family = binomial)
cv.fits_dev$lambda.min
cv.fits_class$lambda.min

#Plot cv-errors and error-bars (cvup) and (cvlow) for each value of tuning parameter.
plot(cv.fits_dev)
plot(cv.fits_class)

#to be sure about the upper and lower curves, we need to it manually as requested by the task
library(ggplot2)
#for model 1, unlike above these plots as a function of lambda
df1 <- data.frame("lambda" = cv.fits_dev$lambda,"cvm" = cv.fits_dev$cvm,"cvup" = cv.fits_dev$cvup,
                  "cvlo" = cv.fits_dev$cvlo)
ggplot(df1, aes(x=lambda)) +
  geom_point(aes(y=cvm), color ="blue") +
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), color = "lightblue") +
  theme_minimal()

#Predictions:
glm.probs <- predict(glm.fits, type = "response")
glm.pred <- ifelse(glm.probs>0.5,"0", "1")
attach(lymphstatus)
table(glm.pred, V1)
mean(glm.pred==V1)

#The upper and lower bounds of the CV-errors gets smaller and smaller as the number of parameters decreases.
#The discussion is then whether one wants a model with low error but lower and upper bounds varies more
#or a model with lower variety and a model with more stability. In this case when it is about human lives i guess
#the stability of a model is more preferable and the Cv-error does not differ that much in the end of the day.
set.seed(100)
cv.fits_dev <- cv.glmnet(x=x, y=y, alpha = 1, nfolds = 10, type.measure = "deviance", family = binomial)
(cv.fits_dev$lambda.min)
#Best tuning parameter is 0.07 approx.
#post-lasso
#choose selected coefficients 
#this gives us coefficients of model 1 excluding its intercept, we are looking for predictors
cv.fits_pred <- predict(cv.fits_dev, type = "coefficients", s=cv.fits_dev$lambda.min)[2:7400]
#now we store index of corresponding selected variables, there are 27 such variables.
incl.lasso<-which(cv.fits_pred!=0)
#store corresponding coefficients
par.lasso<-cv.fits_pred[incl.lasso]
#We see that there are 27 predictors with coefficients not equal to 0.
#these are the coef that are in the best model with respect to deviance.
incl.lasso
#the parameters are: 30   80  773 1188 1456 1671 1744 1825 1860 1871 2437 3345 3454 3813 4131 4328 4548 6411 6607 6733 6956 7069 7098 7218 7250 7343 7357
post_lasso <- glm(y~x[ ,incl.lasso], family = binomial)
coef(post_lasso)
par.lasso

#We see that only 4 out of 27 gene signs disagree, and we can locate those.
#Sign agreement is important because then both increase or decrease death of patient on average.
#Overall, such sign disagreements are small (4/27) and this is overall good news for the models.