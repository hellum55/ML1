pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

library(dplyr)
pumpkin_data <- pumpkin %>%
  select(-c(X, X.1))


library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

pumpkin_data$month <- as.factor(pumpkin_data$month)
pumpkin_data$year <- as.factor(pumpkin_data$year)
str(pumpkin_data)

pumpkin_data <- pumpkin_data %>%
  select(-c(Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment, Grade, Sub.Variety, Unit.of.Sale, Origin.District, Type))

#Question f:####
# Create a new column for average Price
pumpkin_data <- pumpkin_data %>% 
  mutate(price = (Low.Price + High.Price)/2) %>%
  select(-Low.Price, -High.Price, -Mostly.Low, -Mostly.High)

##################################################################################
pumpkins_recipe_test <-
  recipe(price ~ ., data = pumpkin_data) %>%
  step_integer(all_predictors(), zero_based = TRUE)
# Prep the recipe
pumpkins_prep <- prep(pumpkins_recipe_test)

# Bake the recipe to extract a preprocessed new_pumpkins data
baked_pumpkins <- bake(pumpkins_prep, new_data = NULL)

# (b) corr matrix
library(corrplot)
cm <- cor(baked_pumpkins, use = "complete.obs")
cm
corrplot(cm)
corrplot(cm, type="upper", tl.pos="d", method="number", diag=TRUE)
# Most of the predictors show some correlation with mpg 
# But also many predictors correlate with each other?? We return to this point later


# Why variable "name" is not used in the analysis? 
# Merge levels by brands to be able to handle a decent number (see in a future application), 
# do not use the variable - as required
table(Auto$name)


# (c) Run the model
lm.fit <- lm(price ~ .-Repack, data = baked_pumpkins)
summary(lm.fit)
# Comment on the output.
# i) Yes, there is a relatioship between the predictors and the response by testing
#the null hypothesis of whether all the regression coefficients are zero. The F
#-statistic is far from 1 (with a small p-value), indicating evidence against
#the null hypothesis.
# ii) displacement, weigh, year, origin have a significant relationship when looking at their t-statistic.
# iii) 0.750773 can be interpreted as ...
plot(year, mpg)


# (d) Diagnostic
par(mfrow=c(2,2))
plot(lm.fit)
# Comment on the output 

# (e) Interaction effect: horsepower:weight
summary(update(lm.fit, . ~ . + Variety:City.Name))

# We may evaluate if this new model is indeed a better model 
# Using "anova()" function which performs an analysis of variance in order to 
# test the null hypothesis that a model (M1) is sufficient to explain the data against 
# the alternative hypothesis that a more complex model (M2) is required 
# anova () for model comparison only applies to nested models - 
# meaning that the predictors in M1 should be a subset of the predictors in M2) 
anova(lm.fit, update(lm.fit, . ~ . + Variety:City.Name))
# Concl.: the decrease in RSS is substantial for M2, so the interaction term 
# is justified statistically. 
# repeat the above two lines by trying other interaction effects 

# (f) transformation of the predictors 
plot(baked_pumpkins$Package, baked_pumpkins$price)
# A visual inspection reveals a possible non-linear effect (curve line, U shape); 
# Does this also make sense theoretically? 
# Let´s try to model horsepower^2
summary(update(lm.fit, . ~ . + I(Package^2))) 
# and compare
anova(lm.fit, update(lm.fit, . ~ . + I(Package^2)))
# Conclude  


# PS:  In this exercise, we neglected the colinearity of predictors. 
#      Also, in these first exercises, we have not used distiguished between 
#      training and validation datasets. We used only the training data set to 
#      estimate our models and interpret the parameters. 
#      This was the classsical approach.

poly_pumpkins_recipe <-
  recipe(price ~ Package, data = pumpkin_data) %>%
  step_integer(all_predictors(), zero_based = TRUE) %>% 
  step_poly(all_predictors(), degree = 4)
# Prep the recipe
pumpkins_prep <- prep(poly_pumpkins_recipe)

# Bake the recipe to extract a preprocessed new_pumpkins data
baked_pumpkins <- bake(pumpkins_prep, new_data = NULL)

lm.fit <- lm(price ~ ., data = baked_pumpkins)
summary(lm.fit)
