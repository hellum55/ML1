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

#Question 2:####
##Best Subset Selection
library(leaps)
reg.full<-regsubsets(Price~., nvmax = 20, data=pumpkin_data)
summary(reg.full)
names(summary(reg.full))
plot(summary(reg.full)$cp, xlab="No of Variables", ylab="Cp", type="l") #figure of Cp
#minimize by Cp
which.min(summary(reg.full)$cp) #at which p does Cp attain minimum? This produces p=10
coef(reg.full, 21) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.full)$bic)
coef(reg.full, 11)

#Question 3:####
##Forward Stepwise Selection
reg.fwd<-regsubsets(Price~., nvmax=43, method="forward", data=pumpkin_data)
summary(reg.fwd)
#minimize by Cp
which.min(summary(reg.fwd)$cp) #at which p does Cp attain minimum? This produces p=10
coef(reg.full, 21) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.fwd)$bic)
coef(reg.full, 14)



