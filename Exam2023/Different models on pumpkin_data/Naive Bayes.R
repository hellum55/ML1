#Question a:####
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

# Select desired columns
library(tidyverse)
pumpkin <- pumpkin %>% 
  select(c(City.Name, Package, Variety, Origin, Item.Size, Color)) 

# Drop rows containing missing values and encode color as factor (category)
pumpkin <- pumpkin %>% 
  drop_na() %>% 
  mutate(Color = factor(Color))

# Subset distinct observations in outcome column
pumpkin %>% 
  distinct(Color)

# select var and split data
set.seed(12345)
train.index <- sample(c(1:dim(pumpkin)[1]), dim(pumpkin)[1]*0.7)  
train.df <- pumpkin[train.index, ]
valid.df <- pumpkin[-train.index, ]

#___________________________________
# 1.naiveBayes with library e1071
#___________________________________
pumpkin.nb <- naiveBayes(Color ~ ., data = train.df)
pumpkin.nb
# A-priori probabilities are the priors
prop.table(table(train.df$Color))
# The conditional probabilities are P(X|Y) 
# They can be replicated by looking at the % of records 
# for each values relative to the entire class
prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 1)


# predict probabilities for new data 
pred.prob <- predict(pumpkin.nb, newdata = valid.df, type = "raw")
pred.prob
# predict class membership for new data
pred.class <- predict(pumpkin.nb, newdata = valid.df, type="class") 
pred.class
# deliver the results as a dataframe
df <- data.frame(actual = valid.df$Color, predicted = pred.class, pred.prob)
df

# create a confusion matrix and assess performance on unseen data
confusionMatrix(pred.class, valid.df$Color, positive = "ORANGE") # for the default 50% cut-off

# choosing a different cutoff 
confusionMatrix(factor(ifelse(pred.prob[, 1] > 0.2, "ORANGE", "WHITE")), 
                valid.df$Color, positive = "ORANGE")

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

pumpkin.nb1 <- train(
  Color ~ ., 
  data = train.df, 
  method="naive_bayes",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneGrid=Grid)
pumpkin.nb1

# predict class membership for new data
pred.class = predict(pumpkin.nb1, newdata = valid.df) 
pred.class
# predicted probabilities for new data
pred.prob1 <- predict(pumpkin.nb1, newdata = valid.df, type = "prob")
pred.prob1

confusionMatrix(factor(ifelse(pred.prob1[, 1] > 0.2, "ORANGE", "WHITE")), 
                valid.df$Color, positive = "WHITE")
# ROC and AUC
colAUC(pred.prob1[,1], valid.df[ ,6], plotROC = TRUE)  


# ___  
# using Sens metric for optimizing the model selection
#___  
pumpkin.nb2 <- train(
  Color ~ ., 
  data = train.df, 
  method="naive_bayes",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE,
                           summaryFunction = twoClassSummary), # added
  metric="Sens", # added
  tuneGrid=Grid,
)
pumpkin.nb2

# predict class membership for new data
pred.class = predict(pumpkin.nb2, newdata = valid.df) # default is class
pred.class
# predicted probabilities for new data
pred.prob2 <- predict(pumpkin.nb2, newdata = valid.df, type = "prob")
pred.prob2

confusionMatrix(factor(ifelse(pred.prob2[, 1] > 0.2, "ORANGE", "WHITE")), 
                valid.df$Color, positive = "ORANGE")
# ROC and AUC
colAUC(pred.prob2[,1], valid.df[ ,6], plotROC = TRUE) 
# we obtain similar results in this case  