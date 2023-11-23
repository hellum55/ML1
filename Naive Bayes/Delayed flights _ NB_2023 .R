# *************************************************************************************
#                           **  Predicting Delayed Flights **
#      Based on Data Mining for Business Analytics, by G. Shmueli et al., 2017, Wiley 
#                                 # updated October 2023 #
# *************************************************************************************
# Background: 
  # Predicting flights delays can be useful to a variety of organizations:
  # airport, airlines and aviation authorities. 
  # At times, joint task forces have been formed to address this problem. 
  # If such an organization were to provide ongoing real-time assistance with 
  # flight delays, it would benefit from some advance notice about flights that are 
  # likely to be delayed. In this simplified example we look only at five predictors:

     # Day of the week - Coded as 1 = Monday, 2 = Tuesday ..., 7 = Sunday
     # Schedule Dep. time (CRS_DEP_TIME): Broken down into 18 intervals between 6:00 AM abs 10:00 PM
     # Origin: Three airport codes: DCA, IAD and  BWI 
     # Destination: Three airport codes: JFK, LGA, EWR
     # Carrier: Eight airline codes : CO, DH, DL, MQ, OH, RU, UA, and US (USairways).

  # The outcome of interest is whether or not the flight is delayed 
  # ("delayed" here means arrived more than 15 minutes late)
  # Our data consist of all flights from Washington DC area into the New York City 
  # area during January 2004. 
  # A record is a particular flight. The % of delays among the 2201 flights is 19.5%.
  # The data were obtained from the Bureau of Transportation Statistics (www.transtats.bts.gov)
  # The goal is to accurately predict whether of not a new flight will be delayed. 
  
  # The data were first partitioned into training (60%) and validation (40%) sets.
  # A Naive Bayes classifier is first applied to the training data set and tested on the test data set.
# *************************************************************************************

  library(caret)   # for NB modeling
  library(e1071)   # for NB modeling
  library(DataExplorer) # for data exploration
  library(caTools) # for AUC
  

  delays.df <- read.csv("~/")
  str(delays.df)
  # first consider the 5 predictors mentioned in the background: 
  # DAY_WEEK , CRS_DEP_TIME, ORIGIN, DESTINATION and CARRIER
  # classical NB works with categorical variables
  # it requires binning (transforming numeric into factor)
  
  # DAY_WEEK as factor 
  delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
  table(delays.df$DAY_WEEK) 
  
  # create hourly bins departure time 
  delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))
  table(delays.df$CRS_DEP_TIME)
  
  # ORIGIN as factor
  delays.df$ORIGIN <- factor(delays.df$ORIGIN)
  table(delays.df$ORIGIN)
  
  #DEST
  delays.df$DEST <- factor(delays.df$DEST)
  table(delays.df$DEST)
  
  # CARRIER as factor
  delays.df$CARRIER <- factor(delays.df$CARRIER)
  table(delays.df$CARRIER)
  
  # Flight.Status as factor
 delays.df$Flight.Status <- as.factor(delays.df$Flight.Status)
 table(delays.df$Flight.Status)
 
 
  
  # select var and split data
  selected.var <- c(10, 1, 8, 4, 2, 13)
  set.seed(12345)
  train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
  train.df <- delays.df[train.index, selected.var]
  valid.df <- delays.df[-train.index, selected.var]

  
  #___________________________________
  # 1.naiveBayes with library e1071
  #___________________________________
  delays.nb <- naiveBayes(Flight.Status ~ ., data = train.df)
  delays.nb
  # A-priori probabilities are the priors
  prop.table(table(train.df$Flight.Status))
  # The conditional probabilities are P(X|Y) 
  # They can be replicated by looking at the % of records 
  # for each values relative to the entire class
  prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 1)
    
  
  # predict probabilities for new data 
  pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
  pred.prob
  # predict class membership for new data
  pred.class <- predict(delays.nb, newdata = valid.df, type="class") 
  pred.class
  # deliver the results as a dataframe
  df <- data.frame(actual = valid.df$Flight.Status, predicted = pred.class, pred.prob)
  df
  
  # create a confusion matrix and assess performance on unseen data
  confusionMatrix(pred.class, valid.df$Flight.Status, positive = "delayed") # for the default 50% cut-off
  
  # choosing a different cutoff 
  confusionMatrix(factor(ifelse(pred.prob[, 1] > 0.2, "delayed", "ontime")), 
            valid.df$Flight.Status, positive = "delayed")

  #  ROC and AUC
  colAUC(pred.prob[,1], valid.df[ ,6], plotROC = TRUE)
  

  # _______________________________________
  # 2.naive Bayes with 10-fold cv in caret
  #_______________________________________
  # Adv. allows to control a grid of hyperparameters when fitting a model

  
  Grid <- expand.grid(usekernel = FALSE,
                     laplace = c(0, 0.5, 1), 
                     adjust = c(0, 0.25, 0.50, 0.75, 1, 1.25, 1.5))
  
# usekernel = This is a boolean (TRUE or FALSE) parameter that specifies 
# whether to use a kernel density estimate for continuous 
# variables in the Naive Bayes model.
# when set to FALSE, it means that a parametric estimation 
# (assuming a normal distribution) is used for continuous variables (if any)

# laplace and adjust are hyperparameters used for smoothing.  
  # Smoothing is a technique used to address the issue of zero probabilities or 
  # probabilities that are extremely low when estimating conditional probabilities from data
  # By applying smoothing techniques, the Naive Bayes algorithm becomes more robust
  # and less sensitive to data sparsity, which can lead to better generalization 
  # and classification performance. Smoothing ensures that the model can make 
  # reasonable probability estimates for all possible events, even if they were
  # not observed in the training data.
  # A value of 0 means no Laplace smoothing, and higher values provide more aggressive smoothing.
  # adjust: smoothing parameter for conditional probabilities (typically 1 for no adjustment)

  
  delays.nb1 <- train(
    Flight.Status ~ ., 
    data = train.df, 
    method="naive_bayes",
    trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
    tuneGrid=Grid)
delays.nb1
  
  # predict class membership for new data
  pred.class = predict(delays.nb1, newdata = valid.df) 
  pred.class
  # predicted probabilities for new data
  pred.prob1 <- predict(delays.nb1, newdata = valid.df, type = "prob")
  pred.prob1
  
  confusionMatrix(factor(ifelse(pred.prob1[, 1] > 0.2, "delayed", "ontime")), 
                  valid.df$Flight.Status, positive = "delayed")
  # ROC and AUC
  colAUC(pred.prob1[,1], valid.df[ ,6], plotROC = TRUE)  

  

# ___  
# using Sens metric for optimizing the model selection
#___  
  delays.nb2 <- train(
    Flight.Status ~ ., 
    data = train.df, 
    method="naive_bayes",
    trControl = trainControl(method = "cv", number = 5, classProbs = TRUE,
                             summaryFunction = twoClassSummary), # added
    metric="Sens", # added
    tuneGrid=Grid,
    )
  delays.nb2
  
  # predict class membership for new data
  pred.class = predict(delays.nb2, newdata = valid.df) # default is class
  pred.class
  # predicted probabilities for new data
  pred.prob2 <- predict(delays.nb2, newdata = valid.df, type = "prob")
  pred.prob2
  
  confusionMatrix(factor(ifelse(pred.prob2[, 1] > 0.2, "delayed", "ontime")), 
                  valid.df$Flight.Status, positive = "delayed")
  # ROC and AUC
  colAUC(pred.prob2[,1], valid.df[ ,6], plotROC = TRUE) 
# we obtain similar results in this case  
