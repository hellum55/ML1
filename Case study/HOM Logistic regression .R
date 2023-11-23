##############################################################
## Classification: Logistic regression                   
## Modified script based on Hands on Machine Learning textbook      
##############################################################

# Helper packages
library(dplyr)     # for data wrangling
library(ggplot2)   # for plotting
library(rsample)   # for data splitting
library(caret)     # for logistic regression modeling
library(vip)       # for variable importance 
library(modeldata) # for data set "Job attrition"

# These data set is from the IBM Watson Analytics Lab. 
# The website describes the data: “Uncover the factors 
# that lead to employee attrition (churn). Explore important 
# questions, such as ‘show me a breakdown of distance from 
# home by job role and attrition’ or ‘compare average monthly 
# income by education and attrition’. This is a fictional data
# set created by IBM data scientists.”. There are 1470 rows.
# for details about data, see within package "modeldata"
# modeldata::attrition

# Our objective: use employee attrition data to predict "Attrition", Yes/No

data(attrition)
dim(attrition)
# 1470 obs, 31 var

head(attrition$Attrition)
prop.table(table(attrition$Attrition)) # unbalanced

str(attrition) # variables' types 
# Ord.factor into factor
df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE) 
str(df)


set.seed(123)  
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
churn_train <- training(churn_split) 
churn_test  <- testing(churn_split) 
str(churn_train)

# Two simple LR
model1 <- glm(Attrition ~ MonthlyIncome, 
              family = "binomial", 
              data = churn_train)
model2 <- glm(Attrition ~ OverTime, 
              family = "binomial", 
              data = churn_train)

# Original coefficients
options(scipen=999)
summary(model1)
summary(model2)

# CI 
confint(model1) 
confint(model2)

# Exponentiated coefficients
exp(coef(model1))
exp(coef(model2))

# CI 
exp(confint(model1))
exp(confint(model2))


# Multiple LR with 2 predictors 
model3 <- glm(
  Attrition ~ MonthlyIncome + OverTime,
  family = "binomial", 
  data = churn_train
)
summary(model3)

#...

# ML approach

# Simple and Multiple LR trained with 10-fold cv  
# simple LR
set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# multiple LR with 2 predictors
set.seed(123)
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# multiple LR with all predictors
set.seed(123)
cv_model3 <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# multiple LR with all predictors + feature engineering in caret
set.seed(123)
cv_model4 <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("nzv", "center", "scale") #added
)


# PLS LR + feature engineering in caret
set.seed(123)
cv_model_pls <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("nzv", "center", "scale"),
  tuneLength = 30 
)
cv_model_pls$bestTune 

# display results for model with lowest loss
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
# Plot cv-acuraccy
ggplot(cv_model_pls)


# extract out of sample performance measures
# ! notice the default performance function generates the accuracy and kappa !
summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3,
      model4 = cv_model4,
      model5 = cv_model_pls
    )
  )
)$statistics$Accuracy


summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3,
      model4 = cv_model4,
      model5 = cv_model_pls
    )
  )
)$statistics$Kappa


# Run the same 4 models, but now perform model selection based on ROC 
# this change will bring a few changes - see below lines with comment "added"
    # !_________________________________________________________________________
    levels(churn_train$Attrition)
    # to explicitly model the probability of the "Yes" factor level, 
    # the function relevel() is used to temporarily reverse the factors levels 
    # to avoid sensitivity and specificity are reversed in the output
    # __________________________________________________________________________

set.seed(123)
cv_model1 <- train(
  relevel(Attrition, ref = "Yes") ~ MonthlyIncome,  #added
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    classProbs = TRUE, #added
    summaryFunction = twoClassSummary), #added
    metric="ROC" #added
)

set.seed(123)
cv_model2 <- train(
  relevel(Attrition, ref = "Yes") ~ MonthlyIncome + OverTime, #added
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    classProbs = TRUE, #added
    summaryFunction = twoClassSummary), #added
  metric="ROC" #added
)

set.seed(123)
cv_model3 <- train(
  relevel(Attrition, ref = "Yes") ~ ., #added
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    classProbs = TRUE, #added
    summaryFunction = twoClassSummary), #added
  metric="ROC" #added
)


set.seed(123)
cv_model4 <- train(
  relevel(Attrition, ref = "Yes") ~ ., #added
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    classProbs = TRUE, #added
    summaryFunction = twoClassSummary), #added
  metric="ROC", #added
  preProcess = c("nzv", "center", "scale")
)


set.seed(123)
cv_model_pls <- train(
  relevel(Attrition, ref = "Yes") ~ ., #added
  data = churn_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    classProbs = TRUE, #added
    summaryFunction = twoClassSummary), #added
  metric="ROC", #added
  preProcess = c("nzv", "center", "scale"),
  tuneLength = 20 
)


summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3,
      model4 = cv_model4,
      model5 = cv_model_pls
    )
  )
)
# model 3, 4 and 5 are best models and very close in terms of performance
# model 3 and model 4 are a bit better because their Sensitivity is better 
# and in this application predicting the "Yes" class is particularly important


# Evaluating the test error for the 'best performance' model 
# Let's assume we chose model 4 as the best
  pred_class_test <- predict(cv_model4, churn_test) #response 'class' 
  pred_class_test
  pred_prob <- predict(cv_model4, churn_test,type = "prob")$Yes # probability
  pred_prob

  # confusion matrix and statistics
  confusionMatrix(
    data = relevel(pred_class_test, ref = "Yes"), 
    reference = relevel(churn_test$Attrition, ref = "Yes"))
  # Brief discussion on the output:
        # Accuracy: 0.8636, and its CI
        # No Information Rate (NIR): 0.8386
            # NIR represents the ratio of non-attrition vs. attrition in our testing data 
            # table(churn_test$Attrition) %>% prop.table()
            # If we simply predicted "No" for every employee we would still get 
            # an accuracy rate of 83.86%.Our goal is to maximize the accuracy rate 
            # over and above this no information baseline.
            # NIR is also called a 'naive benchmark'. 
         # Kappa: 0.400 
            # Kappa means how much better our classifier is performing over the 
            # performance of a classifier that simply guesses at random 
            # according to the frequency of each class.
            # Kappa
              # * 0 or less, indicate that the classifier is useless,
              # * 0–0.20 is slight, 
              # * 0.21–0.40 is fair
              # * 0.41–0.60 is moderate, 
              # * 0.61–0.80 is substantial, 
              # * 0.81–1 as almost perfect.
          # Sensitivity: 0.38028
              # Our classifier is not performing very well in recalling the 
              # 'positive' class ("Yes" in this application)        
          # Specificity: 0.95664.
              # Our classifier is performing very well in predicting the 
              # 'negative' class ("No" in this application)          
  
  # building ROC
  library(ROCR)
  m4_prob <- predict(cv_model4, churn_test, type = "prob")$Yes  # predicted prob.
  perf4 <- prediction(m4_prob, churn_test$Attrition) 
  roc_ROCR_model4 <-  performance(perf4, measure = "tpr", x.measure = "fpr")
  plot(roc_ROCR_model4 , col = "red", lty = 2, main = "ROC curve Model 4")
  abline(a = 0, b = 1)
  # calculating AUC 
  auc_ROCR_model4 <- performance(perf4, measure = "auc")
  auc_ROCR_model4 <- auc_ROCR_model4@y.values[[1]]
  auc_ROCR_model4
  #[1] 0.7917478 ! it may depend on your split
  

  # For comparison,one can represent the ROC curves for the three best models in the same plot 
  m3_prob <- predict(cv_model3, churn_test, type = "prob")$Yes
  perf_m3 <- prediction(m3_prob, churn_test$Attrition) 
  roc_ROCR_model3 <-  performance(perf_m3, measure = "tpr", x.measure = "fpr")
  
  mpls_prob <- predict(cv_model_pls, churn_test, type = "prob")$Yes
  perf_pls <- prediction(mpls_prob, churn_test$Attrition) 
  roc_ROCR_pls <-  performance(perf_pls, measure = "tpr", x.measure = "fpr")
 
  plot(roc_ROCR_model4, col = "red", lty = 2, 
       main = "ROC curve in the test set for the three best models")
  plot(roc_ROCR_model3, add = TRUE, col = "green")
  plot(roc_ROCR_pls, add = TRUE, col = "blue")
  legend(0.8, 0.2, legend = c("Model LR with feat.eng", "Model LR", 
                              "Model LR_PLS"),col = c("red", "green", "blue"), 
                              lty = 2:1, cex = 0.6)

 
# Feature importance
  par(mfrow=c(2,1))
  vip(cv_model4, num_features = 20)
# Conclusion: 
  # OverTime (whether the employee works overtime) is the most influential 
  # variable in the prediction of churn, followed by JobSatisfaction, and 
  # EnvironmentSatisfaction

 
  
# ____________________________________________________________________________  
# If the major objective is accurate classification, what cutoff value should 
# be used when deploying the model?
# ____________________________________________________________________________
  
  # Plotting accuracy and overall error as a function of cutoff value
  probabilities = as.data.frame(m3_prob)
  
  # create empty accuracy table
  accT = c()
  # compute accuracy per cutoff
  for (cut in seq (0, 1, 0.1)) {
    cm <- confusionMatrix(factor(ifelse(probabilities  > cut,"Yes", "No")), 
                          factor(churn_test$Attrition))
    accT = c(accT, cm$overall[1])}
  
  # plot 
  plot(accT ~ seq(0,1, 0.1), xlab= "Cutoff value", ylab ="", type="l", ylim=c(0,1))
  lines (1-accT ~ seq(0,1, 0.1), type = "l", lty=2)
  legend ("center", c("accuracy", "overall error"), lty  = c(1,2), merge = TRUE)
  # The accuracy level is pretty stable for cutoff values between 0.40 and 0.70
  # thus any of these cutoff can be selected.
  
  
  
# ____________________________________________________________________________
# When is Accuracy Rate Insufficient as an Evaluation Metric?
# ____________________________________________________________________________

# The accuracy rate is a commonly used performance metric (default in R), 
# but it can be misleading in certain situations, particularly when dealing 
# with asymmetric costs of misclassification.  
  
# Consider a scenario where the costs of misclassification are not equal. 
# For instance, in a churn prediction model for employees, misclassifying an 
# employee as "unlikely to churn" ('No') when they actually belong to the 
# churner category ('Yes') could have more severe consequences for a company 
# than the reverse error of misclassifying an employee as 'Yes' when they truly 
# belong to the 'No' category.
  
# Case 1: Misclassifying as 'No' when Actually 'Yes':
    # In the first case, if the model incorrectly labels an employee as 'No' 
    # when they are likely to churn ('Yes'), the company may lose that employee 
    # without taking preventive measures. This could result in substantial costs
    # in terms of recruitment, training, and productivity loss.
  
# Case 2: Misclassifying as 'Yes' when Actually 'No':
    # On the other hand, if an employee is mistakenly classified as 'Yes' when 
    # they are unlikely to churn ('No'), the company may incur the relatively 
    # lower cost of contacting an employee who has no intention of leaving. 
    # While this can be an inconvenience, it is generally a less critical error 
    # compared to the first case.  
  
  
# If we had the costs of misclassification, we could calculate and compare the 
  # different classifiers using the overall expected costs as a criterion. 
  # A popular performance measure is the "average misclassification cost"
  # which measures the average misclassification cost per classified record.
  
# Example: # If we denote:
    # q1 the cost of misclassifying a "Yes/1" record as belonging to class "No/0"
    # q2 the cost of misclassifying a "No/0" record as belonging to class "Yes/1"
    # then, the "average misclassification cost" can be calculated as:
    # (q1* n_01 + q2 *n_10) / n 
    # where n is the total number of records, and n_01 and n_10 are the numbers 
    # of items incorrectly classified.
    # Thus, we are looking for a classifier that minimizes this quantity.
        
  # For our selected model the Confusion Matrix was:
  
  #             Reference
  #  Prediction Yes  No
  #         Yes  34  15
  #         No   38 355
    
  # given q1 = 1000 €  and q2 = 3000€
  # Average miss.class cost = (38 * 1000 + 15 * 3000)/442 = 187.3€
  
  
  
# In addition to using the Accuracy rate and costs for model selection, another 
# important criterion is the Recall or Sensitivity of a classifier. 
# In other words, the ability of the classifier to detect the critical class 
# members ("Yes" or 1's) correctly. 
  
