#Question a:####
library(dplyr)
pumpkin <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

pumpkin_data <- pumpkin %>%
  select(c(City.Name, Variety, Date, Low.Price, High.Price, Item.Size))

#Question b:
pumpkin_data <- pumpkin_data %>%
  mutate(Price = (Low.Price+High.Price)/2,
         Spread = 5+(High.Price-Low.Price)) %>%
  select(-c(Low.Price, High.Price))

par(mfrow=c(1,2))
hist(pumpkin_data$Price, breaks = 20, col = "red", border = "red") 
hist(pumpkin_data$Spread, breaks = 20, col = "blue", border = "blue") 

library(ggplot2)


# Basic density
#Price
par(mfrow=c(1,2))

# Histogram with density plot (Price)
ggplot(pumpkin_data, aes(x=Price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

# Histogram with density plot (Price)
ggplot(pumpkin_data, aes(x=Spread)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

#Question c:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
         year = year(Date)) %>%
  select(-Date)

#Question d:####
pumpkin_data[pumpkin_data==""] <- NA
pumpkin_data <- na.omit(pumpkin_data)

#Question e:###
AdjPrice <- median(pumpkin_data$Price*pumpkin_data$Spread)/mean(pumpkin_data$Spread)
std_adj <- sd(pumpkin_data$Price * pumpkin_data$Spread) / mean(pumpkin_data$Spread)


#Question f:####
library(boot)
set.seed(123)
  
(bs <- boot(pumpkin_data$AdjPrice, function(v, i) mean(v[i]), 1000))
se <- sd(bs$t)
(mu <- mean(pumpkin_data$Adjprice))
c(mu - 2*se, mu + 2*se)




