pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

library(dplyr)
pumpkin_data <- pumpkin %>%
  select(c(City.Name, Variety, Date, Low.Price, High.Price, Item.Size))

#Question b:
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

#Question d:####
library(DataExplorer)
plot_missing(pumpkin_data)
pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)
sum(is.na(pumpkin_data))

#Use the summary function to get estimates of SE for the coefficients in a
#multiple LR:
library(glmnet)
glm.fit <- glm(Price~ Item.Size, data = pumpkin_data, family = gaussian)
summary(glm.fit)               

#Now write a function (boot.fn) that takes pumpkin data set as well as the indexes of
#the observations and outputs of City name and month:
attach(pumpkin_data)
boot.fn = function(data, index) return(coef(glm(Price~
          Item.Size, data = data, subset = index)))
boot.fn(pumpkin_data, 1:1473)

#Use the boot() function together with your boot.fn() function to
#estimate the standard errors of the LR coefficients.
library(boot)
set.seed(123)
boot.fn(pumpkin_data, sample(1473, 1473, replace = T))
boot.out=boot(pumpkin_data, boot.fn, 1000)
boot.out
summary(glm.fit) 
#As we can see there is a difference in the estimates. The bootstrap acutally predicts a lower SE
#for all the coefficients, but does it mean that it is more accurate? Yes. When computing std errors
#for the coefficients using summary the estimates relies on several assumptions and one of them are
#linear dependency. We have seen trough out the exercises that there are non(ish) linear relationship between
#the price and Item.Size. This brings a lot of noise to variance parameter. The non-linear relationship
#gives noisy predictions. But the bootstrap does not rely on these assumptions and thereby a more accurate
#prediction.


