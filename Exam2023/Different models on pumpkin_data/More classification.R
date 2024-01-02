#Question a:####
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

# Select desired columns
pumpkin <- pumpkin %>% 
  select(c(City.Name, Package, Variety, Origin, Item.Size, Color, Low.Price, High.Price, Date)) %>%
  mutate(Price = (Low.Price+High.Price)/2) %>%
  select(-c(Low.Price, High.Price))

library(tidyverse)
pumpkin_data <- pumpkin %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

# Drop rows containing missing values and encode color as factor (category)
pumpkin_data <- pumpkin_data %>% 
  drop_na() %>% 
  mutate(Color = factor(Color))

# Subset distinct observations in outcome column
pumpkin_data %>% 
  distinct(Color)

#pumpkin_data <- pumpkin_data %>%
  #filter(Color != "STRIPED")

#pumpkin_data <- pumpkin_data %>%
  #mutate(Color = if_else(Color == "STRIPED", "WHITE", Color))

# (a) Numerical and graphical summaries + comment on patterns
table(is.na(pumpkin_data))
summary(pumpkin)
pairs(pumpkin_data)
cor(pumpkin_data[, 8:10])
library (GGally) 
ggpairs(Weekly) 
#Of course a higher correlation between years and month because they some what have a relationship. But not any
#high correlations between price and years and months.

#Make some preprocessing before using the data:
library(recipes)
pumpkin_recipe <- recipe(price ~ ., data = pumpkin_data) %>%
  step_impute_knn(all_predictors(), all_outcomes(), neighbors = 6) %>%
  step_other(Package, threshold = 0.05, other = "other") %>%
  step_other(Origin, threshold = 0.05, other = "other") %>%
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(Item.Size)

prepare <- prep(pumpkin_recipe, training = pumpkin_data)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked <- bake(prepare, new_data = pumpkin_data)

# (b) Logistic Regresssion
attach(pumpkin_data)
glm.fit = glm(Color ~ ., data = pumpkin_data, 
              family = binomial)

summary(glm.fit)
# Looking at the coefficients, price, item size and variety cinderella and miniature appears to have some statistical 
# significance with a Pr(>|z|) < 5%.

# (c) Confusion matrix (based on the training sample - as in classical statistics)          
glm.probs = predict(glm.fit, type = "response") # predict probabilities
glm.probs

glm.pred = rep("ORANGE", length(glm.probs)) #  assigning labels White to all respondents
glm.pred[glm.probs > 0.5] = "ORANGE"  # classifying

# one can add these vectors in the dataset 
# (notice this method will alter the original dataset for future uses)
pumpkin_data$glm.probs = predict(glm.fit, type = "response")
pumpkin_data$glm.pred = ifelse(pumpkin_data$glm.probs>0.5, "WHITE", "ORANGE")
glm.probs = predict(glm.fit, type = "response")
glm.pred = ifelse(pumpkin_data$glm.probs>0.5, "WHITE", "ORANGE")

prop.table(table(pumpkin_data$Color))
table(glm.pred, pumpkin_data$Color)
# Accuracy or Percentage of correct predictions: 
# (138+790)/(790+18+45+138) = 93.64%. 
# Error rate = 1- Accuracy = 6.36%

# pumpkin is "White" and the logistic regression is right:
# 138/(138+18) = 88.46%. 
# this is Recall or Sensitivity (considering White="1") - see Lecture slides


# Pumpkin is "Orange" and the logistic regression is right:
# 790/(790+45) = 94.46%%.
# This is True negative rate or Specificity (considering Orange="0")  see Lecture slides


# Pumpkins is predicted to be "White" the logistic regression is right:
# Precision, or Positive Predicted Value: TP/ (TP + FP)
# 138/(138 + 45) = 75.4%

# A more elegant way using ´caret´ library 
library(caret)
confusionMatrix(factor(ifelse(glm.probs > 0.5, "WHITE", "ORANGE")), 
                factor(pumpkin_data$Color), positive = "ORANGE") 


# (d) Train and test on holdout sample
pumpkin_data <- pumpkin_data %>%
  group_by(year, month, City.Name) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup()

set.seed (185)
train <- sample(1:nrow(pumpkin_data), nrow(pumpkin_data) * 0.7) 
test <- (-train)

table(pumpkin.train$year)
table(pumpkin.out$year)

# train
glm.fit = glm(Color ~ Package + Price, data = pumpkin_data[train, ], family = binomial)
# alternatively,
# glm.fit = glm(Direction ~ Lag2, data = Weekly.9008, family = binomial) # train

# predict in the holdout
glm.probs = predict(glm.fit, pumpkin_data[test, ], type = "response") 
# glm.probs = predict(glm.fit, Weekly.0910) # default are logits 

glm.pred = rep("ORANGE", length(glm.probs))  
glm.pred[glm.probs > 0.5] = "WHITE" 
Color.test = pumpkin_data$Color[test]

# one can display the rest of indexes using ´caret´
confusionMatrix(factor(ifelse(glm.probs > 0.5, "WHITE", "ORANGE")), 
                factor(Color.test), positive = "ORANGE") 


# e) LDA (train and test)
library(MASS)
lda.fit = lda(Color ~ Variety, data = pumpkin_data[train, ], family = binomial) 
lda.pred = predict(lda.fit, pumpkin_data[test,]) 
lda.pred$posterior # probability
lda.pred$class # class; default cutoff 50%
table(lda.pred$class, Color.test) 
# Accuracy (9+56)/(9+5+34+56) = 62.5%
# or
library(caret)
confusionMatrix(factor(ifelse(lda.pred$posterior[,2] > 0.5, "Up", "Down")), 
                factor(Direction.0910), positive = "Up") 


# (f) QDA (train and test)
qda.fit = qda(Color ~ Variety, data = pumpkin_data[train, ], family = binomial)
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
glm.fit = glm(Color ~ Price:Package, data = pumpkin_data[train, ], family = binomial)
glm.probs = predict(glm.fit, pumpkin_data[test,], type = "response")
glm.pred = rep("ORANGE", length(glm.probs))
glm.pred[glm.probs > 0.5] = "WHITE"
table(glm.pred, Color.test)
mean(glm.pred == Color.test) # a way to get quickly the Accuracy

# same model with LDA 
lda.fit = lda(Color ~ Price:Package, data = pumpkin_data[train, ], family = binomial)
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
price01 = rep(0, length(pumpkin_data$Price))
price01[pumpkin_data$Price > median(pumpkin_data$Price)] = 1
pumpkin_data01 = data.frame(pumpkin_data, price01)
View (pumpkin_data01)

# (c) split the data 
set.seed (185)
train <- sample(1:nrow(pumpkin_data01), nrow(pumpkin_data01) * 0.7)
test <- (-train)
price01.test = price01[test]

# (d) LDA
library(MASS)
lda.fit = lda(price01 ~ year + month, data = pumpkin_data01, 
              subset = train) # train LDA
lda.pred = predict(lda.fit, pumpkin_data01[test, ]) # predit in the holdout set
mean(lda.pred$class == price01.test) # accuracy
mean(lda.pred$class != price01.test) # test error rate 

# (e) QDA
qda.fit = qda(price01 ~ year + month, data = pumpkin_data01, 
              subset = train)
qda.pred = predict(qda.fit, pumpkin_data01[test, ])
mean(qda.pred$class != price01.test)


# (f) LR
glm.fit = glm(price01 ~ Item.Size + City.Name + Package, data = pumpkin_data01, 
              subset = train)
glm.probs = predict(glm.fit, pumpkin_data01[test, ], type = "response") # probabilities
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1 # note: cut-off set up at 50 %; it can be altered.
mean(glm.pred != price01.test)


# (g) KNN
library(class)
train.X = cbind(Item.Size, City.Name, Package)[train, ] # IV train set
test.X = cbind(Item.Size, City.Name, Package)[test, ]  # IV test set
train.price01 = price01[train] # DV train

set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.price01, k = 1)
mean(knn.pred != price01.test)

# KNN(k=50)
knn.pred = knn(train.X, test.X, train.price01, k = 50)
mean(knn.pred != price01.test)

# KNN(k=100)
knn.pred = knn(train.X, test.X, train.price01, k = 100)
mean(knn.pred != price01.test)

# Write the conclusion here by comparing the error rates. 
#Doing worse than logistic regression

#######
# Ex. 13
#######               
# logistic regression 1
glm.fit = glm(price01 ~ . -Price -Origin, data = pumpkin_data01, 
              subset = train)
summary(glm.fit)

glm.probs = predict(glm.fit, pumpkin_data01[test, -4], type = "response")
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