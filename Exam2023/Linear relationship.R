#Model concerns
#Load data:
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

library(dplyr)
pumpkin_data <- pumpkin %>%
  select(-c(X, X.1))

#Question d:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

pumpkin_data$month <- as.factor(pumpkin_data$month)
pumpkin_data$year <- as.factor(pumpkin_data$year)

pumpkin_data <- pumpkin_data %>%
  select(-c(Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment, Grade, Sub.Variety, Unit.of.Sale, Origin.District, Type))

pumpkin_data <- pumpkin_data %>% 
  mutate(price = (Low.Price + High.Price)/2) %>%
  select(-Low.Price, -High.Price, -Mostly.Low, -Mostly.High)

#1. Linear relationship:
p1 <- ggplot(pumpkin_data, aes(month, price)) + 
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE) +
  scale_y_continuous("price", labels = scales::dollar) +
  xlab("month") +
  ggtitle(paste("Non-transformed variables with a\n",
                "non-linear relationship."))

p2 <- ggplot(pumpkin_data, aes(month, price)) + 
  geom_point(size = 1, alpha = .4) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10("price", labels = scales::dollar, 
                breaks = seq(0, 400000, by = 100000)) +
  xlab("month") +
  ggtitle(paste("Transforming variables can provide a\n",
                "near-linear relationship."))

gridExtra::grid.arrange(p1, p2, nrow = 1)

#As we can see from the left plot is there a non-linear relationship between the price of the pumpkins and the months they are
#being sold. This is a violation in the OLS assumptions and has to be transformed in a way. The right plot shows a log transformation to 
#normalize the response variable. 

#2. Constant variance among residuals:
library(recipes)
pumpkin_recipe <- recipe(price ~ ., data = pumpkin_data) %>%
  step_impute_knn(all_predictors(), all_outcomes(), neighbors = 6)
  
prepare <- prep(pumpkin_recipe, training = pumpkin_data)
prepare
# bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
baked <- bake(prepare, new_data = pumpkin_data)

set.seed(123)
cv_model1 <- train(
  price ~ month, 
  data = baked, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
  price ~ ., 
  data = baked, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

df1 <- broom::augment(cv_model1$finalModel, data = pumpkin_data)

p1 <- ggplot(df1, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "price ~ month")

df2 <- broom::augment(cv_model3$finalModel, data = pumpkin_data)

p2 <- ggplot(df2, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4)  +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Sale_Price ~ .")

gridExtra::grid.arrange(p1, p2, nrow = 1)
# Linear regression assumes constant variance among the residuals. model1 (left) shows definitive
#signs of heteroskedasticity whereas model3 (right) appears to have more constant variance. But there are some
#issues in the start of the plot to the right. Ideally they should appear in a group but there is some kind of
#a line shape and then a group of plots. Some prdicted values have high residuals compared to others which can give 
#some issues.

#3. No autocorrelation
#Linear regression assumes the errors are independent and uncorrelated. If in fact, there is correlation among the errors, then the estimated standard errors of the coefficients
#will be biased leading to prediction intervals being narrower than they should be.
df1 <- mutate(df1, id = row_number())
df2 <- mutate(df2, id = row_number())

p1 <- ggplot(df1, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.")

p2 <- ggplot(df2, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Uncorrelated residuals.")

gridExtra::grid.arrange(p1, p2, nrow = 1)
#As we can see on the left plot there seems to be a pattern. Plots with -1(ish) residuals are more or less plotted in groups and the same
#with the plots above but a bit more variance. Stil i bit of a pattern on the right plot, but not as much as the left plot. 
#This can lead to the issue that one error-term can incluence the other and thereby lead to incorrectly predictions.

#More observations than predictors:
#This should not be a problem in this case. Only 9 predictors and 1757 observations which is fine.

#No or little multicollinearity:
library (GGally)
library(DataExplorer)
pumpkin_data <- pumpkin_data %>%
  group_by(year, month, City.Name, Origin) %>%
  filter(n() / nrow(.) >= 0.1) %>%
  ungroup()

plot_bar(pumpkin_data)
ggpairs(pumpkin_data[ ,8:10])
str(pumpkin_data)
