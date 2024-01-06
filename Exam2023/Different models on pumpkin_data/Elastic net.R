pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

library(dplyr)
pumpkin_data <- pumpkin %>%
  select(c(City.Name, Variety, Date, Low.Price, High.Price, Item.Size))

pumpkin_data <- pumpkin_data %>%
  mutate(Price = (Low.Price+High.Price)/2,
         Spread = 5+(High.Price-Low.Price)) %>%
  select(-c(Low.Price, High.Price))

library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

pumpkin_data$month <- as.factor(pumpkin_data$month)
pumpkin_data$year <- as.factor(pumpkin_data$year)

pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)
sum(is.na(pumpkin_data))

pumpkin_data <- pumpkin_data %>%
  group_by(year, month, City.Name) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup()

library(rsample)
x <- model.matrix(Price~.,pumpkin_data)[, -1]
y <- pumpkin_data$Price

#decreasing lambda grid from 1000 to 0.01
l.grid <- 10^seq(10, -2, length = 100)

set.seed (185)
train <- sample(1:nrow(x), nrow(x) * 0.7) 
test <- (-train)
y.test <- y[test]

library(glmnet)
library(leaps)
library(caret)
l12.model <- cv.glmnet(x=x[train, ], y=y[train], alpha=0.5, nfolds = 10, lambda = l.grid)
l12.lam = l12.model$lambda.min
l12.model.pred <- predict(l12.model, s=l12.lam, newx = x[test, ])
mse <- mean((y.test - l12.model.pred)^2)



