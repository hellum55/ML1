##############################
#### Exam Fall 2021
#### Machine Learning for BI 1
#### Sample solution
##############################


library(caret) # for data modelling
library(caTools) # for AUC
library(e1071) # for data modelling
library(plyr) # for data transformation
library(MASS) # for data modelling


# 1) Cleaning steps (Problem 1)
#_______________________________________________________________________________
# Method 1
#_______________________________________________________________________________
dataTel <- read.csv("~/Cloud/Documents/Alina Tudoran/TEACHING/Postgraduate/Machine Learning 2020-2021/ML1/2023/New lectures/4. Ch4_Classification/CASE STUDIES/Case Study Exam 2021/ADW.csv", 
                    stringsAsFactors=TRUE)
View(dataTel)
str(dataTel)


# Transform into factors
names <- c(1:5, 7:18, 21)
dataTel[,names] <- lapply(dataTel[,names], factor)
str(dataTel)


# Delete ID
dataTel = dataTel[, -1]
str(dataTel)

# # Change the "No internet service" to "No" for the following columns: 
# "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", 
# "StreamingTV", "StreamingMovies"
library(plyr)
cols <- c(9:14)
for(i in 1:ncol(dataTel[,cols])) {
  dataTel[,cols][,i] <- as.factor(mapvalues
                                  (dataTel[,cols][,i], 
                                    from =c("No internet service"),
                                    to=c("No")))
}


# Change "No phone service" to "No" for column "MultipleLines".
dataTel$MultipleLines[dataTel$MultipleLines == "No phone service"] <- "No"
levels(dataTel$MultipleLines)
dataTel$MultipleLines <-droplevels(dataTel$MultipleLines)
dataTel$MultipleLines
# Levels: No Yes

# Check
table(dataTel$OnlineSecurity)
table(dataTel$OnlineBackup)
table(dataTel$DeviceProtection)
table(dataTel$TechSupport)
table(dataTel$StreamingTV)
table(dataTel$StreamingMovies)
table(dataTel$MultipleLines)
# E.g. 
# > table(dataTel$MultipleLines)
# No  Yes 
# 4072 2971 

# Delete NA
table(is.na(dataTel))
data1 <- dataTel[complete.cases(dataTel), ]    

# Check
dim(data1)
# [1] 7032   20
str(data1)

# save the file for later use
# write.csv(data1, file = "data1.csv", row.names = F)


#_______________________________________________________________________________
# Method 2
#_______________________________________________________________________________
{dataTel <- read.csv("~/Cloud/Documents/Alina Tudoran/TEACHING/Postgraduate/Machine Learning 2020-2021/ML1/2023/New lectures/4. Ch4_Classification/CASE STUDIES/Case Study Exam 2021/ADW.csv", 
                     stringsAsFactors=TRUE)
  str(dataTel)
  dataTel$SeniorCitizen <- factor(dataTel$SeniorCitizen)
  
  # Change the "No internet service" to "No" for the following columns: 
  # "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", 
  # "StreamingTV", "StreamingMovies".
  cols <- c(10:15)
  for(i in 1:ncol(dataTel[,cols])) {
    dataTel[,cols][,i] <- as.factor(mapvalues
                                    (dataTel[,cols][,i], 
                                      from =c("No internet service"),
                                      to=c("No")))
  }
  # Change "No phone service" to "No" for column "MultipleLines".
  dataTel$MultipleLines[dataTel$MultipleLines == "No phone service"] <- "No"
  dataTel$MultipleLines <-droplevels(dataTel$MultipleLines)
  
  # remove rows with NAs + customer ID (column 1)
  data1 <- dataTel[rowSums(is.na(dataTel))==0,-1]
  
  str(data1)
}


###############
## Problem 2 ##
###############

ADW_help <- readRDS("~/Cloud/Documents/Alina Tudoran/TEACHING/Postgraduate/Machine Learning 2020-2021/ML1/2022/New lectures/4. Ch4_Classification/CASE STUDIES/Case Study Exam 2021/ADW_help.R")

###############################################
# 1. Discretize continuous variables
###############################################
attach(ADW_help) 
tenure01 = rep(0, length(tenure))
tenure01[tenure > mean(tenure)] = 1

MonthlyCharges01 = rep(0, length(MonthlyCharges))
MonthlyCharges01[MonthlyCharges > mean(MonthlyCharges)] = 1

TotalCharges01 = rep(0, length(TotalCharges))
TotalCharges01[TotalCharges > mean(TotalCharges)] = 1

# Integrate them in the data and delete original            
ADW_help_d = data.frame(ADW_help[, -c(5, 18, 19)],
                        tenure01, MonthlyCharges01, TotalCharges01)
str(ADW_help_d)

# Set all variables as factors 
ADW_help_d$tenure01= as.factor(ADW_help_d$tenure01)
ADW_help_d$MonthlyCharges01= as.factor(ADW_help_d$MonthlyCharges01)
ADW_help_d$TotalCharges01= as.factor(ADW_help_d$TotalCharges01)
ADW_help_d$SeniorCitizen = as.factor(ADW_help_d$SeniorCitizen) #if not done before
str(ADW_help_d)

# if not able to discretize 
#  ADW_help_d <- ADW_help[,-c(5,18,19)]

##############################################################
# 2. Set.seed (123) and partition data (60/40), Naive Bayes 
# and discuss the output 
##############################################################

set.seed(123)         
intrain<- createDataPartition(ADW_help_d$Churn,p=0.6,list=FALSE)
training<- ADW_help_d[intrain,]
testing<- ADW_help_d[-intrain,]

#____________________________
# Method 1: using naiveBayes 
#____________________________
model.nb <- naiveBayes(Churn ~ ., data = training)
model.nb
# Output displays prior and conditional probabilities of X given the class Y (p(X|Y).
# They are [...](reproduce the relevant output from console).
# Discussion: These probabilities multiplied and divided by the probability of X  
# lead to the posterior probability according to Bayes formula, under the assumption 
# of independence. P(Y|X) is the probability based on which the classification of 
# observations in class 0/1 is done.  

## Model performance in the test 
pred.prob <- predict(model.nb, newdata = testing, type = "raw")
pred.prob
pred.class <- predict(model.nb, newdata = testing) 
pred.class
confusionMatrix(pred.class, testing$Churn, positive = "Yes")
colAUC(pred.prob[,1], testing[ ,17], plotROC = TRUE) 
## ! display the relevant output and comment
## example: 

## Confusion Matrix and Statistics
## Reference
## Prediction       No  Yes
# No  1687  255
# Yes  378  492

# Accuracy : 0.7749         
# 95% CI : (0.759, 0.7902)
# No Information Rate : 0.7344         
# P-Value [Acc > NIR] : 4.156e-07      
# Kappa : 0.4518         

# Mcnemar's Test P-Value : 1.240e-06      

#          Sensitivity : 0.6586         
#            Specificity : 0.8169         
#         Pos Pred Value : 0.5655         
#         Neg Pred Value : 0.8687         
#             Prevalence : 0.2656         
#         Detection Rate : 0.1750         
#   Detection Prevalence : 0.3094         
#      Balanced Accuracy : 0.7378         

#       'Positive' Class : Yes            



#___________________________
# Method 2: using caret
#___________________________
# Grid = data.frame(usekernel = FALSE, laplace = 0, adjust = 1)
model.nb1 <- train(
  Churn ~ ., 
  data = training, 
  method="naive_bayes",
  na.action = na.pass,
  #trControl = trainControl(method = "none"),
  trControl = trainControl(method = "cv", number = 10)
  #tuneGrid=Grid
)
model.nb1
pred.prob1 <- predict(model.nb1, newdata = testing, type = "prob")
pred.prob1
pred.class1 <- predict(model.nb1, newdata = testing) 
pred.class1
confusionMatrix(pred.class1, testing$Churn, positive = "Yes")
colAUC(pred.prob1[,1], testing[ ,17], plotROC = TRUE) 
# recall to report the relevant output and discuss it


##################################################
# 3. Evaluate and discuss the model performance  
##################################################


#########################
# 4. LDA   
#########################
#________________________
# Method 1 
#________________________
str(ADW_help)
set.seed(123)         
intrain<- createDataPartition(ADW_help$Churn,p=0.6,list=FALSE)
training<- ADW_help[intrain,]
testing<- ADW_help[-intrain,]

lda.fit1 = lda(Churn ~ ., data = training)
lda.fit1
plot(lda.fit1) # scores and associated probabilities separated by class
# Discuss the meaning of the coefficients
# Example: The coefficients of linear discriminant output provide the linear
# combination of the predictors that are used to form the LDA decision rule.
# If we multiply each value of LD1 (the first linear discriminant) by the corresponding 
# elements of the predictor variables and sum them we get a score for each respondent.
# This score along the the prior are used to compute the posterior probability of class 
# membership.Classification is made based on the posterior probability, 
# with observations predicted to be in the class for which they have the highest probability.

#________________________________________
# Method 2 using caret
#________________________________________
lda.fit2 = train(
  Churn ~ ., 
  data = training, 
  method="lda",
  trControl = trainControl(method = "cv", number = 10))
lda.fit2$finalModel


###################################
# 5. class, posterior, assess model  
###################################
# shown only for lda.fit1
lda.pred = predict(lda.fit1, testing)
head(lda.pred$class)
# [1] No Yes Yes Yes No  No 
head(lda.pred$posterior)
#No        Yes
#4  0.9627408 0.03725918
#5  0.2363543 0.76364573
#7  0.4304018 0.56959815
#9  0.3786101 0.62138993
#10 0.9839197 0.01608031
#12 0.9388943 0.06110567

confusionMatrix(lda.pred$class, testing$Churn, positive = "Yes")
colAUC(lda.pred$posterior, testing[ ,20], plotROC = TRUE) 
# Interpretation required


###############################################################
# 6. Compare the Naive Bayes and the LDA algorithm in terms of 
# assumptions & prediction performance 
###############################################################
# Discussion hints: They both rely on the Bayes Theorem. 
# Regarding LDA: It assumes multivariate normality of 
# predictors within each class. Assuming that the observations from each class 
# come from normal distribution with a class specific mean vector and a 
# common variance-covariance matrix, LDA approximates Bayes classifier by 
# plugging estimates for priors, means and variance/covariance matrix.
# Regarding NB, The predictors are assumed to be independent within each class. 
# NB works better when all the variables are factors; the discretization for the
# continuous variables may have a significant impact on our result.


# 6.2. Comparing prediction performance e.g.
# Example of comparison based on cv-accuracy below
summary(
  resamples(
    list(
      model1 = model.nb1, 
      model2 = lda.fit2
    )
  )
)$statistics$Accuracy
# Interpretation: similar performance based on cv-accuracy criterion
# Complete answer also consider sensitivity, specificity, precision, ROC/AUC



#__________________________________________________________________________
###############
## Problem 3 ## 
###############

# Preliminaries
# split data 
n <- dim(data1)[1]
i_IN <- seq(1,5000)
i_OUT <- seq(5000+1,n)

data_IS <- data1[i_IN,]
data_OS <- data1[i_OUT,]

#############################################
# 3.1) Estimate Logits + Model Selection 
#############################################
# Estimation 
m.glm1 <- glm(Churn ~1,data=data_IS,family = "binomial")
m.glm2 <- glm(Churn ~.,data=data_IS,family = "binomial")
m.glm3 <- glm(Churn ~.^2,data=data_IS,family = "binomial")


# in-sample criteria 
logLik(m.glm1)
logLik(m.glm2)
logLik(m.glm3)

# model selection criterion 
AIC(m.glm1)
AIC(m.glm2)
AIC(m.glm3)

#Interpretation/Justification: 
#Loglik increases with more regressors by construction (in-sample error). 
#Loglik does not control for potential overfit, suggests Model 3.
#AIC corrects for overfit 
#AIC is an unbiased estimator of out-of-sample risk -> right criterion here
#AIC suggest model 2 (more parsimonious, lower variance compared to Model 3)
#potentially better out-of-sample performance.


#############################################
# 3.2) Probabilities + Test error 
#############################################


# predict probabilities on test set 
y.glm1 <- predict(m.glm1, newdata=data_OS, type = "response")
y.glm2 <- predict(m.glm2, newdata=data_OS, type = "response")
y.glm3 <- predict(m.glm3, newdata=data_OS, type = "response")


#calculate test error 
# Auxiliary Steps: 
# transform test outcome to numeric to use in ll() 
y.os <- data_OS[,"Churn"] == "Yes"
# Define out-of-sample criteria: LL+accuracy 
ll <- function(p,y){
  return( sum(y*log(p) + (1-y)*log(1-p)) )
}

#calculate test error function:
ll(y.glm1,y.os)
ll(y.glm2,y.os)
ll(y.glm3,y.os)

# discussion 
# Model 2 best test error. Equivalent model selection to AIC in Problem (1)
# in line with theory as AIC is supposed to pick low risk model
# on average. 



#############################################
# 3.3) Extended data frame + cleaning 
#############################################

#setup axuliary model for x_frame 
m.glm <- glm(Churn ~.^2,data=data1,family = "binomial")
# generate new data objects 
X <- as.matrix(model.matrix(m.glm)[,!is.na(m.glm$coefficients)][,-1])
Y <- data1[,"Churn"] == "Yes"    

#split newdata
X_IS <- X[i_IN,]
X_OUT <- X[i_OUT,]
Y_IS <- Y[i_IN]

#############################################
# 3.4) Bagging/Elastic Net + cross-validation 
#############################################
library(glmnet)


B <- 3 #B >= 2 here
votes1 <- matrix(0,n-5000,B)
votes2 <- votes1

for (b in seq(1,B)){
  print(b/B) #just for checking duration
  b_IS <- sample(seq(1,5000),5000,replace = TRUE)
  b_cv10.out = cv.glmnet(X_IS[b_IS,],Y_IS[b_IS],
                         alpha=0.5, 
                         nfolds = 5,
                         family="binomial",
                         type.measure = "class")
  #predict
  b_min.pred <- predict(b_cv10.out, s="lambda.min", newx = X[i_OUT,],type="response") 
  b_1se.pred <- predict(b_cv10.out, s="lambda.1se", newx = X[i_OUT,],type="response") 
  
  votes1[,b] <- as.numeric(b_min.pred > 0.5)
  votes2[,b] <- as.numeric(b_1se.pred > 0.5)
  
}

y1.pred <- rowMeans(votes1) > 0.5
y2.pred <- rowMeans(votes2) > 0.5

c(mean(y1.pred==y.os),mean(y2.pred == y.os))



#############################################
# 3.5) Ensemble + test accuracy 
#############################################
# Super Learner library
library(SuperLearner)

##Choose at least 3 methods 
SL.methods <- c('SL.mean',"SL.lda","SL.xgboost","SL.glm","SL.glmnet")

##Run SuperLearner with correct data, family and method 
#create data.frame for input in SuperLearner
X_sl <- as.data.frame(model.matrix(m.glm2)[,-1])
#change x-labels to avoid problem with some methods
#(only relevant for some SL.methods choices)
colnames(X_sl) <- paste("x",seq(1,dim(X_sl)[2]),sep="")
#create outcome variable for input in SuperLearner
Y_sl <- m.glm2$y
#estimate ensemble
model.SL <- SuperLearner(Y=Y_sl, X=X_sl, SL.library = SL.methods,
                         family = "binomial", method = "method.NNloglik")



# create "new data" for (out-of-sample) predictions using same labels as above, 
#(if methods that do not require relabeling skip this step)
m.glm <- glm(Churn ~.,data=data_OS,family = "binomial")
X_sl_out <- as.data.frame(model.matrix(m.glm)[,-1])
colnames(X_sl_out) <- paste("x",seq(1,dim(X_sl)[2]),sep="")

## predict probabilities and calculate accuracy     
y.sl <- predict(model.SL, newdata = X_sl_out)$pred > 0.5
c(mean(y.sl==y.os))

## get model averaging weights + name largest (note: depends on random seed) 
model.SL$coef

