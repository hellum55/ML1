###############################################
## Classification: LDA, QDA and KNN-classifier
###############################################

library(DataExplorer) # for data exploration
library(caret)        # for fitting KNN models
library (caTools)     # for ROC and AUC (ROCR used in our previous code currently 
                      # supports only evaluation of binary classification tasks)
library(ggplot2)      # for graphics


# Consider the flower data set 'iris', where the task is to classify 
# a DV with 3 categories. This famous (Fisher's or Anderson's) iris 
# data set gives the measurements in centimeters of the variables:
# sepal length and width and petal length and width, respectively, 
# for 50 flowers from each of 3 species of iris. The species are:
# Iris setosa, versicolor, and virginica.
# Objective: Predict species using all the variables in the data set

# data
data(iris)
names(iris)
plot_missing(iris)
plot_histogram(iris)
# if you get this error
# "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)):
# invalid graphics state", simply clear all Plots in the plot pane.


# distribution by category
ggplot(iris, aes(x=Sepal.Length, col=Species)) + geom_density()
ggplot(iris, aes(x=Sepal.Width, col=Species)) + geom_density()
ggplot(iris, aes(x=Petal.Length, col=Species)) + geom_density()
ggplot(iris, aes(x=Petal.Width, col=Species)) + geom_density()

# box plot by category
par(mfrow=c(2,2))
boxplot(Sepal.Length ~ Species, data=iris,
        main="Box Plot",
        xlab="Species",
        ylab="Sepal Length")

boxplot(Sepal.Width ~ Species, data=iris,
        main="Box Plot",
        xlab="Species",
        ylab="Sepal Width")

boxplot(Petal.Length ~ Species, data=iris,
        main="Box Plot",
        xlab="Species",
        ylab="Petal Length")

boxplot(Petal.Width  ~ Species, data=iris,
        main="Box Plot",
        xlab="Species",
        ylab="Sepal Width")

# corr matrix by category
lapply(split(iris[,1:4],iris$Species),cov)

# data split
index = createDataPartition(y=iris$Species, p=0.7, list=FALSE)
train = iris[index,]
test = iris[-index,]

# model LDA
lda.fit = train(
  Species ~ ., 
  data=train, 
  method="lda",
  trControl = trainControl(method = "cv", number = 10))
lda.fit
summary(lda.fit)

# LDA + feature engineering
lda.fit_1 = train(
  Species ~ ., 
  data=train, 
  method="lda",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("nzv", "center", "scale")
  )
lda.fit_1

# QDA
qda.fit = train(
  Species ~ ., 
  data=train, 
  method="qda",
  trControl = trainControl(method = "cv", number = 10))
qda.fit
summary(qda.fit)

# QDA + feature engineering
qda.fit_1 = train(
  Species ~ ., 
  data=train, 
  method="qda",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("nzv", "center", "scale"))
qda.fit_1
summary(qda.fit_1)

# KNN classifier + feature engineering
hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(train)/3, length.out = 20)))
knn.fit_1 = train(
  Species ~ ., 
  data=train, 
  method="knn",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid,
  preProcess = c("nzv", "center", "scale"))
ggplot(knn.fit_1)
knn.fit_1$finalModel$k

# summary
summary(
  resamples(
    list(
      model1 = lda.fit, 
      model2 = lda.fit_1,
      model3 = qda.fit, 
      model4 = qda.fit_1,
      model5 = knn.fit_1
    )
  )
)$statistics$Accuracy
# based on accuracy, LDA performs better than 
# QDA and knn-classifier for this data set

#  evaluating the test error for LDA
pred.species = predict(lda.fit, test)
confusionMatrix(pred.species, test$Species)

# ROC plot based on caTools library; AUC is displayed in the console
par(mfrow=c(1,1))
LDA_prob <- predict(lda.fit, test, type = "prob")$setosa
colAUC(LDA_prob, test$Species, plotROC = TRUE)



