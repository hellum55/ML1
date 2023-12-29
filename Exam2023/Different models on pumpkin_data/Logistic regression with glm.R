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

#Lets look at have the probabilities are:
prop.table(table(pumpkin$Color)) # very unbalanced to the orange side

library(glmnet)
#Logistic regression
glm.fit <- glm(Color~., data = pumpkin, family = binomial)
summary(glm.fit)

#Predictions:
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs>0.5,"ORANGE", "WHITE")
attach(pumpkin)
table(glm.pred, Color)
mean(glm.pred==Color)

set.seed(2056)
pumpkins_split <- pumpkin %>% 
  initial_split(prop = 0.8)
# Extract the data in each split
pumpkins_train <- training(pumpkins_split)
pumpkins_test <- testing(pumpkins_split)
pairs(pumpkins_train, col=pumpkins_train$Color)

# Create a recipe that specifies preprocessing steps for modelling
pumpkins_recipe <- recipe(Color ~ ., data = pumpkins_train) %>% 
  step_mutate(Item.Size = ordered(Item.Size, levels = c('sml', 'med', 'med-lge', 'lge', 'xlge', 'jbo', 'exjbo'))) %>%
  step_integer(Item.Size) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

prepare <- prep(pumpkins_recipe, training = pumpkins_train)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked_train <- bake(prepare, new_data = pumpkins_train)
baked_test <- bake(prepare, new_data = pumpkins_test)

glm.fit <- glm(Color~., data=baked_train, family = binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, newdata = baked_test, type ="response")
glm.pred <- ifelse(glm.probs > 0.5, "WHITE", "ORANGE")
Color_train <- baked_test$Color
table(glm.pred, Color_train)
mean(glm.pred==Color_train)
