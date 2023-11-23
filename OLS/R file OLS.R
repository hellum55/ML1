######################################################
## Ordinary Linear regression in ML                          
######################################################

# _____________________________________________________________________________
# We continue working with the ames data set as in previous applications
# _____________________________________________________________________________


# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for cross-validation, etc.

# Model interpretability packages
library(vip)      # variable importance

# data
set.seed(123)
library(rsample)
ames <- AmesHousing::make_ames()
split <- initial_split(ames, prop = 0.7, strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)


# ___________________
# Simple LR with OLS 
# ___________________

model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
plot(model1) 
summary(model1) 
  # interpret the beta as the mean selling price increases by approx 100 USD,
  # for each additional one square foot of above ground living space.
sigma(model1)    # training RMSE (also called residual standard error)
  ## [1] 58113.73
sigma(model1)^2  # training MSE
  ## [1] 3377206020
  # confidence intervals for each coefficient
confint(model1, level = 0.95)


# ____________________
# Multiple LR with OLS 
# ____________________

# Gr_Liv_Area and Year_Built as predictors
model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
summary(model2)

# Same model with interaction
model2_wint <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Gr_Liv_Area:Year_Built, data = ames_train)
summary(model2_wint)

# Model incl. all predictors
model3 <- lm(Sale_Price ~ ., data = ames_train) 
# print estimated coefficients in a tidy data frame
summary(model3)  

# ___________________________________________________
## Training MLR models using 10-fold cross-validation
# ___________________________________________________

set.seed(123)
(cv_model1 <- train(
  form = Sale_Price ~ Gr_Liv_Area, 
  data = ames_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

set.seed(123)
cv_model2 <- train(
  Sale_Price ~ Gr_Liv_Area + Year_Built, 
  data = ames_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# Extract out of sample performance measures
summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3
)))

# Concl.: model 3 is are able to improve 
# the out-of-sample cross validation performance metrics.


# ___________________________________________________
# Model concerns: the strong assumptions 
# ___________________________________________________

# (1) Linear regression assumes a linear relationship between the predictor(s) 
# and the response variable

# e.g. the relation year built - sale price is not linear  
p1 <- ggplot(ames_train, aes(Year_Built, Sale_Price)) + 
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE) +
  scale_y_continuous("Sale price", labels = scales::dollar) +
  xlab("Year built") +
  ggtitle(paste("Non-transformed variables with a\n",
                "non-linear relationship."))
p1

# in this case, we can achieve a linear relationship by log transforming sale price
p2 <- ggplot(ames_train, aes(Year_Built, Sale_Price)) + 
  geom_point(size = 1, alpha = .4) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10("Sale price", labels = scales::dollar, 
                breaks = seq(0, 400000, by = 100000)) +
  xlab("Year built") +
  ggtitle(paste("Transforming variables can provide a\n",
                "near-linear relationship."))

gridExtra::grid.arrange(p1, p2, nrow = 1)



# (2). Constant variance among residuals

df1 <- broom::augment(cv_model1$finalModel, data = ames_train) # add predicted value to each observation
p1 <- ggplot(df1, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Sale_Price ~ Gr_Liv_Area")
# model1 appears to have non-constant variance (violation of assumption)

df2 <- broom::augment(cv_model3$finalModel, data = ames_train) # add predicted values to each observation
p2 <- ggplot(df2, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4)  +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Sale_Price ~ .")

gridExtra::grid.arrange(p1, p2, nrow = 1)
# model3 appears to have near-constant variance

# Concl. Non-constant variance can often be resolved 
# with variable transformations or by including additional predictors 
# (as in this case)


# (3) Linear regression assumes the errors are independent (uncorrelated)

#order data by ID
df1 <- mutate(df1, id = row_number())
df2 <- mutate(df2, id = row_number())

# plot residuals 
p1 <- ggplot(df1, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals")
# the residuals for homes in the same neighborhood tend to be correlated 

p2 <- ggplot(df2, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Less correlated residuals")

gridExtra::grid.arrange(p1, p2, nrow = 1)
# when introducing more predictors (model 3) the effect is reduced 


# (5) Multicollinearity
# e.g. given Garage_Area and Garage_Cars are two variables that have a 
# correlation of 0.89. When running the model Garage_Cars is found to be 
# statistically significant but Garage_Area is not

# see the parameters of these two strongly correlated variables
summary(cv_model3) %>%
  broom::tidy() %>%
  filter(term %in% c("Garage_Area", "Garage_Cars"))


# test model again without Garage_Cars
set.seed(123)
mod_wo_Garage_Cars <- train(
  Sale_Price ~ ., 
  data = select(ames_train, -Garage_Cars), 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(mod_wo_Garage_Cars) %>%
  broom::tidy() %>%
  filter(term == "Garage_Area")

# test model without Garage_Area
set.seed(123)
mod_wo_Garage_Area <- train(
  Sale_Price ~ ., 
  data = select(ames_train, -Garage_Area), 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(mod_wo_Garage_Area) %>%
  broom::tidy() %>%
  filter(term == "Garage_Cars")

#Concl. This reflects the instability in the linear regression model caused by 
# between-predictor relationships; this instability may also get propagated 
# directly to the model predictions.

# How can we control for multicolinearity?
  # i) manually removal of one of the variables (as shown above)
  # ii) dimension reduction techniques (PCA, PLS)
  # iii) regularized methods (LASSO, ...)
  # ii) - iii) this will be covered in the following lectures


# _________________________________________________________
# Implement some feature engineering and re-train the model 
# _________________________________________________________
# blueprint
ames_recipe <- recipe(Sale_Price ~ ., data = ames_train)
  step_nzv(all_nominal())  %>%
  step_integer(matches("Qual|Cond|QC|Qu")) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_pca(all_numeric(), -all_outcomes())
ames_recipe
# prepare
prepare <- prep(ames_recipe, training = ames_train)
prepare
# bake
baked_train <- bake(prepare, new_data = ames_train)
baked_test <- bake(prepare, new_data = ames_test)
baked_train
baked_test


set.seed(123)
model_withFE <- train(
  Sale_Price ~ ., 
  data = baked_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)


# ____________________________
# Summary of all models tested
# ____________________________
summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3,
  model4 = mod_wo_Garage_Cars,
  model5 = mod_wo_Garage_Area,
  model6 = model_withFE
)))

# ______________________________________________________________________________
# - Conclude based on the results and reflect on the possibility to continue 
# improving the model performance 
# - It is left as a homework the task of evaluating the 'test-error'
# for the best model 
# - We have not applied step_log(all_outcomes()) in the blueprint. Reflect on the
# implications of adding this particular line of code
# ______________________________________________________________________________

