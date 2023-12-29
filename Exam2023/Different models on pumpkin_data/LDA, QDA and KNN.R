library(DataExplorer) # for data exploration
library(caret)        # for fitting KNN models
library (caTools)     # for ROC and AUC (ROCR used in our previous code currently 
# supports only evaluation of binary classification tasks)
library(ggplot2)      # for graphics


#Question a:####
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

# Select desired columns
pumpkin <- pumpkin %>% 
  select(c(City.Name, Package, Variety, Origin, Item.Size, Color)) 

# Drop rows containing missing values and encode color as factor (category)
pumpkin <- pumpkin %>% 
  drop_na() %>% 
  mutate(Color = factor(Color))

# Subset distinct observations in outcome column
pumpkin %>% 
  distinct(Color)

# data
plot_histogram(pumpkin)
# if you get this error
# "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)):
# invalid graphics state", simply clear all Plots in the plot pane.

# distribution by category
ggplot(pumpkin, aes(x=Item.Size, col=Color)) + geom_density()

# box plot by category
par(mfrow=c(2,2))
boxplot(Variety ~ Color, data=pumpkin,
        main="Box Plot",
        xlab="Color",
        ylab="Variety")

# data split
index = createDataPartition(y=pumpkin$Color, p=0.7, list=FALSE)
train = pumpkin[index,]
test = pumpkin[-index,]

pumpkin_recipe <- recipe(Color ~ ., data = train) %>%
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(Item.Size) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors(), -all_outcomes())

prepare <- prep(pumpkin_recipe, training = train)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = train)
baked_test <- bake(prepare, new_data = test)

# LDA + feature engineering
lda.fit_1 = train(
  Color ~ ., 
  data=baked_train, 
  method="lda",
  trControl = trainControl(method = "cv", number = 10)
)
lda.fit_1

# QDA + feature engineering
qda.fit_1 = train(
  Color ~ ., 
  data=baked_train, 
  method="qda",
  trControl = trainControl(method = "cv", number = 10)
)
qda.fit_1
summary(qda.fit_1)

# KNN classifier + feature engineering
hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(train)/3, length.out = 20)))
knn.fit_1 = train(
  Color ~ ., 
  data=baked_train, 
  method="knn",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid,
)
ggplot(knn.fit_1)
knn.fit_1$finalModel$k

# summary
summary(
  resamples(
    list(
      model2 = lda.fit_1,
      model5 = knn.fit_1
    )
  )
)$statistics$Accuracy

#  evaluating the test error for LDA
pred.species = predict(lda.fit_1, baked_test)
confusionMatrix(pred.species, test$Color)

# ROC plot based on caTools library; AUC is displayed in the console
par(mfrow=c(1,1))
LDA_prob <- predict(lda.fit_1, baked_test, type = "prob")$ORANGE
colAUC(LDA_prob, baked_test$Color, plotROC = TRUE)