# ***************************************************************************************************
#                                         Classification Applied Exercises
#                          Source: James et al., Introduction to Statistical Learning 
# ***************************************************************************************************
#   Guidelines solutions to: Applied Ex.10, 11, 12, 13 # (ISL, version 2017)
#             Equivalent to: Applied Ex.13, 14, 15, 16 # (ISL, version 2021)
#   Updated Version: 13 Oct 2022 # 



#######
# Ex. 10
#######
# This question should be answered using the Weekly dataset, which is part of thee ISLR library

    # Upload/access and explore your data
    library(ISLR)
    ?Weekly # make sure you read this
    View(Weekly)
    dim(Weekly)
    str(Weekly)
    
    
    # (a) Numerical and graphical summaries + comment on patterns
    table(is.na(Weekly))
    summary(Weekly)
    pairs(Weekly)
    cor(Weekly[, -9])
    library (GGally) 
    ggpairs(Weekly) 
    # High + corr between Volume and Year
    
  
    
    # (b) Logistic Regresssion, DV = Direction, IV = Lags var + Volume
    attach(Weekly)
    glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
                  family = binomial)
    summary(glm.fit)
    # Looking at the coefficients, Lag 2 appears to have some statistical 
    # significance with a Pr(>|z|) = 3%.

             
              
    # (c) Confusion matrix (based on the training sample - as in classical statistics)          
    glm.probs = predict(glm.fit, type = "response") # predict probabilities
    glm.probs
              
    glm.pred = rep("Down", length(glm.probs)) #  assigning labels Down to all respondents
    glm.pred[glm.probs > 0.5] = "Up"  # classifying
    
    # one can add these vectors in the dataset 
    # (notice this method will alter the original dataset for future uses)
     Weekly$glm.probs = predict(glm.fit, type = "response")
     Weekly$glm.pred = ifelse(Weekly$glm.probs>0.5, "Up", "Down")
    
    prop.table(table(Weekly$Direction))
    table(glm.pred, Weekly$Direction)
    # Accuracy or Percentage of currect predictions: 
    # (54+557)/(54+557+48+430) = 56.1%. 
    # Error rate = 1- Accuracy = 43.9
              
    # Weeks the market goes "Up" and the logistic regression is right:
    # 557/(557+48) = 92.1%. 
    # this is Recall or Sensitivity (considering Up="1") - see Lecture slides
              
            
    # Weeks the market goes "Down" and the logistic regression is right:
    # 54/(430+54) = 11.2%.
    # This is True negative rate or Specificity (considering Down="0")  see Lecture slides
              
    
    # Weeks the market is predicted to go "Up" the logistic regression is right:
    # Precision, or Positive Predicted Value: TP/ (TP + FP)
    # 557/(557 + 430) = 56.4%
              
              
    # A more elegant way using ´caret´ library 
      library(caret)
      confusionMatrix(factor(ifelse(glm.probs > 0.5, "Up", "Down")), 
                              factor(Weekly$Direction), positive = "Up") 
             
        
     # (d) Train and test on holdout sample    
     train = (Weekly$Year < 2009) # select training period from 1990 to 2008
     Weekly.9008 = Weekly[train, ] # train
     Weekly.0910 = Weekly[!train, ] # holdout 
              
     table(Weekly.9008$Year)
     table(Weekly.0910$Year)
              
     # train
     glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
     # alternatively,
     # glm.fit = glm(Direction ~ Lag2, data = Weekly.9008, family = binomial) # train
      
             
    # predict in the holdout
    glm.probs = predict(glm.fit, Weekly.0910, type = "response") 
    # glm.probs = predict(glm.fit, Weekly.0910) # default are logits 
    
    
    glm.pred = rep("Down", length(glm.probs))  
    glm.pred[glm.probs > 0.5] = "Up" 
    Direction.0910 = Weekly$Direction[!train] # DV in the holdout
    # Weekly.0910$Direction
    table(glm.pred, Direction.0910) # confusion matrix for the holdout sample
    # Accuracy (9+56)/(9+5+34+56) = 62.5%
    # one can display the rest of indexes using ´caret´
    confusionMatrix(factor(ifelse(glm.probs > 0.5, "Up", "Down")), 
                    factor(Direction.0910), positive = "Up")    
    
    # e) LDA (train and test)
     library(MASS)
     lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train) 
     lda.pred = predict(lda.fit, Weekly.0910) 
     lda.pred$posterior # probability
     lda.pred$class # class; default cutoff 50%
     table(lda.pred$class, Direction.0910) 
     # Accuracy (9+56)/(9+5+34+56) = 62.5%
     # or
     library(caret)
     confusionMatrix(factor(ifelse(lda.pred$posterior[,2] > 0.5, "Up", "Down")), 
                              factor(Direction.0910), positive = "Up") 
              
              
              
              
      # (f) QDA (train and test)
      qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
      qda.pred = predict(qda.fit, Weekly.0910) 
      qda.pred$posterior # probability
      qda.pred$class # class; default cutoff 50%
      table(qda.pred$class, Direction.0910)
      # Accuracy (0+61)/(0+0+61+43) = 58.65%, even though it predicted Up well the whole time.
      # or using caret
        library(caret)
        confusionMatrix(factor(ifelse(qda.pred$posterior[,2] > 0.5, "Up", "Down")), 
                              factor(Direction.0910), positive = "Up") 
              
              
              
              
      # (g) KNN (train and test)
      library(class)
      attach(Weekly)
      train.X = as.matrix(Lag2[train]) # IV train
      test.X = as.matrix(Lag2[!train]) # IV test
      train.Direction = Weekly$Direction[train] # DV train
      test.Direction = Weekly$Direction[!train]

      set.seed(12345,"L'Ecuyer")
      knn.pred = knn(train.X, test.X, train.Direction, k = 1)  # k=1 (probably overfitting), try another k. 
      knn.pred # predicted class 
      table(knn.pred, test.Direction)
      # Accuracy (21+31)/(21+31+30+22) = 50%
      # prob not available. Why?
      
      
      # (h) Which of these classifiers  appear to provide the best results  
      # on this data? LR and LDA appearr to be the best for this data. 
      # Both LR and LDA methods provide similar estimates for 
      # the accurracy in the holdout set (i.e. similar test error rates).
              
              
      # (i) Experiment with different combinations of variables. Notice that
      # in the previous models we worked only with Lag2 as predictor.
              
      # LR with main effects Lag 1, Lag 2 and interaction Lag2:Lag1
      glm.fit = glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)
      glm.probs = predict(glm.fit, Weekly.0910, type = "response")
      glm.pred = rep("Down", length(glm.probs))
      glm.pred[glm.probs > 0.5] = "Up"
      Direction.0910 = Direction[!train]
      table(glm.pred, Direction.0910)
      mean(glm.pred == Direction.0910) # a way to get quickly the Accuracy
      
              
      # same model with LDA 
      lda.fit = lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
      lda.pred = predict(lda.fit, Weekly.0910)
      table(lda.pred$class, Direction.0910)
      mean(lda.pred$class == Direction.0910) 
            

      # QDA with sqrt(abs(Lag2))
      qda.fit = qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
      qda.pred = predict(qda.fit, Weekly.0910)
      table(qda.pred$class, Direction.0910)
      mean(qda.pred$class == Direction.0910)
              
              
      # KNN k =7
      knn.pred = knn(train.X, test.X, train.Direction, k = 7)
      table(knn.pred, Direction.0910)
      mean(knn.pred == Direction.0910)
              
      # KNN k = 100
      knn.pred = knn(train.X, test.X, train.Direction, k = 100)
      table(knn.pred, Direction.0910)
      mean(knn.pred == Direction.0910)
              
              
      # Now, consider making a table with a summary of the results 
      # using Accuracy. Reflect when Accuracy is a good index of 
      # evaluating the model and when it is not. 
      # Do the same for Sensitivity (Recall), Specificity and Precision of the models
      # Write down your thoughts here.
              
              
      
    #######
    # Ex. 11
    #######             
    #(a) create DV
    library(ISLR)
    summary(Auto)  
    attach(Auto)
    mpg01 = rep(0, length(mpg))
    mpg01[mpg > median(mpg)] = 1
    Auto = data.frame(Auto, mpg01)
    View (Auto)
    str(Auto)
              
    #(b) Explore data
    table(is.na(Auto))
    summary(Auto)
    pairs(Auto)
    library (GGally) 
    ggpairs(Auto [, -c(9)]) # excluding "name" 
    # E.g. mpg negatively corr with cylinders, weight, displacement, horsepower
              
              
    # (c) split the data 
    train = (year%%2 == 0)  # if the year is even; by default, use random
    test = !train
    Auto.train = Auto[train, ]
    Auto.test = Auto[test, ]
    mpg01.test = mpg01[test]
              
              
    # (d) LDA
    library(MASS)
    lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
                            subset = train) # train LDA
    lda.pred = predict(lda.fit, Auto.test) # predit in the holdout set
    mean(lda.pred$class == mpg01.test) # accuracy
    mean(lda.pred$class != mpg01.test) # test error rate 
              
    # (e) QDA
    qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
                            subset = train)
    qda.pred = predict(qda.fit, Auto.test)
    mean(qda.pred$class != mpg01.test)
              
              
    # (f) LR
    glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
                            family = binomial, subset = train)
    glm.probs = predict(glm.fit, Auto.test, type = "response") # probabilities
    glm.pred = rep(0, length(glm.probs))
    glm.pred[glm.probs > 0.5] = 1 # note: cut-off set up at 50 %; it can be altered.
    mean(glm.pred != mpg01.test)
              
              
    # (g) KNN
    library(class)
    train.X = cbind(cylinders, weight, displacement, horsepower)[train, ] # IV train set
    test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]  # IV test set
    train.mpg01 = mpg01[train] # DV train
    
    set.seed(1)
    # KNN(k=1)
    knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
    mean(knn.pred != mpg01.test)
              
    # KNN(k=50)
    knn.pred = knn(train.X, test.X, train.mpg01, k = 50)
    mean(knn.pred != mpg01.test)
              
    # KNN(k=100)
    knn.pred = knn(train.X, test.X, train.mpg01, k = 100)
    mean(knn.pred != mpg01.test)
              
    # Write the conclusion here by comparing the error rates. 
              
              
#######
# Ex. 12
#######             
     # a) 
     Power = function() {
                2^3
              }
     print(Power())
              
     # b)   
     Power2 = function(x, a) {
                x^a
              }
     Power2(3, 8)
              
     # c)   
     Power2(10, 3)
     Power2(8, 17)
     Power2(131, 3)
              
     # d)
     Power3 = function(x, a) {
                result = x^a
                return(result)
              }
              
      # e)
     x = 1:10
     plot(x, Power3(x, 2), log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
                   main = "Log of x^2 versus Log of x")
              
      # f)
     PlotPower = function(x, a) {
     plot(x, Power3(x, a))
              }
     PlotPower(1:10, 3)
              
              
#######
# Ex. 13
#######               
# Boston data set 
library(MASS)
View(Boston)
str(Boston)
summary(Boston) 
# Our DV crim is originally metric
              
              
# Creating DV crime01 for Logistic Regression 
attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1   # if higher than median => 1, otherwise 0
Boston = data.frame(Boston, crime01) 
              
# Split the data; in practice, it should be random split (see Case Study)
# Below I show an example of 50/50% split without random selection.
# Why is this split not quite correct? What is the disadvantage?
train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]
# One of the best method of splitting the data to preserve the proportion of 0/1 
# in the train and test sample is STRATIFIED random split (see more in Case Study).
              
              
# logistic regression 1
  glm.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
                            subset = train)
  summary(glm.fit)
              
  glm.probs = predict(glm.fit, Boston.test, type = "response")
  glm.pred = rep(0, length(glm.probs))
  glm.pred[glm.probs > 0.5] = 1
  mean(glm.pred != crime01.test) # 18.2% test error rate.
              
              
# logistic regression 2 
  glm.fit = glm(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, family = binomial, 
                            subset = train)
              
  glm.probs = predict(glm.fit, Boston.test, type = "response")
  glm.pred = rep(0, length(glm.probs))
  glm.pred[glm.probs > 0.5] = 1
  mean(glm.pred != crime01.test) # 18.6% test error rate.
  
# logistic regression 3 
  glm.fit = glm(crime01 ~ . - crime01 - crim - chas - tax - lstat - indus - age, data = Boston, family = binomial, 
                subset = train)
  glm.probs = predict(glm.fit, Boston.test, type = "response")
  glm.pred = rep(0, length(glm.probs))
  glm.pred[glm.probs > 0.5] = 1
  mean(glm.pred != crime01.test) # 18.9%
  
  
  
# LDA 1
  lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
  lda.pred = predict(lda.fit, Boston.test)
  mean(lda.pred$class != crime01.test) # 13.43% test error rate
              
# LDA 2             
  lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, subset = train)
  lda.pred = predict(lda.fit, Boston.test)
  mean(lda.pred$class != crime01.test) # 12.25% test error rate
              
# LDA 3              
  lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax - lstat - indus - age, 
                            data = Boston, subset = train)
  lda.pred = predict(lda.fit, Boston.test)
  mean(lda.pred$class != crime01.test) # 11.85% test error rate
              

# KNN
    library(class)
    train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                              lstat, medv)[train, ]
    test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                             lstat, medv)[test, ]
    train.crime01 = crime01[train]
    set.seed(1)
              
  # KNN(k=1)
              knn.pred = knn(train.X, test.X, train.crime01, k = 1)
              mean(knn.pred != crime01.test)# 45.84% test error rate
  # KNN(k=10)
              knn.pred = knn(train.X, test.X, train.crime01, k = 10)
              mean(knn.pred != crime01.test) # 11.06% test error rate
  # KNN(k=100)
              knn.pred = knn(train.X, test.X, train.crime01, k = 100)
              mean(knn.pred != crime01.test) # 48.61% test error rate
  # KNN(k=10) with a subset of variables
              train.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[train, ]
              test.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[test, ]
              knn.pred = knn(train.X, test.X, train.crime01, k = 10)
              mean(knn.pred != crime01.test) # 27.66% test error rate
              
# Write a final conclusion by comparing the different models and 
# the resulting test error rates. 
              
              
# Note: 
# The test error rates above were calculated at a cut-off of 50% (default). 
# For model comparions, AUC should be considered which considers all cut-offs
# library(caTools) 
# colAUC(lda.pred$posterior[,2], crime01.test, plotROC = TRUE)
# See later under Case Study.           
# Sensitivity (Recall) and Specificity maybe considered too, 
# given the importance of type error I and type error II in different contexts.  

              

            
              