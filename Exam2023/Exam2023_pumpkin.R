#Question a:####
library(dplyr)
pumpkin_data <- read.csv("~/ML1/Exam2023/US-pumpkins.csv", stringsAsFactors=TRUE)

#Question b:####
str(pumpkin_data)
dim(pumpkin_data)
View(pumpkin_data[1:5, ])

#Question c:####
pumpkin_data <- pumpkin_data %>%
  select(-c(X, X.1))

#Question d:####
library(tidyverse)
pumpkin_data <- pumpkin_data %>%
  mutate(Date = mdy(Date),
         month = month(Date),
          year = year(Date)) %>%
  select(-Date)

#Question e:####
library(visdat)
library(DataExplorer)
sum(is.na(pumpkin_data))

# Visually (e.g. plot)
vis_miss(pumpkin_data, cluster = TRUE) #visdat library
plot_missing(pumpkin_data)
#Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment and Grade have all 100% missing values
#in this dataset. This means that they have absolutely zero affect on the future predictions, and it is best to
#drop these variables.

pumpkin_data <- pumpkin_data %>%
  select(-c(Trans.Mode, Crop, Storage, Appearance, Condition, Quality, Environment, Grade))

#Checking for missing values again:
plot_missing(pumpkin_data)
#Two variables Mostly.High and Mostly.low have 5.86% missing values, which is ok in this case. But i will apply
#knn-imputation because it looks like the prices are in some sort of range. The prices are integer and the
#imputation should be so.

#Question f:####
# Create a new column for average Price
pumpkin_data <- pumpkin_data %>% 
  mutate(price = (Low.Price + High.Price)/2)

#Check for appropriate normalization:
#First checking for the distribution of price without any transformation:
par(mfrow=c(1,3))
hist(pumpkin_data$price, breaks = 20, col = "red", border = "red") 
summary(pumpkin_data$price)
#Highly skewed distribution with almost 400 observations with a price of almost 0 (0.24 as the lowest price). That
#must be a pumpkin of a very poor quality.

#Log transformation:
log_pumpkin <- log(pumpkin_data$price)

# Box-Cox transformation (lambda=0 is equivalent to log(x))
library(forecast)
BC_pumpkin <- forecast::BoxCox(pumpkin_data$price, 
                                            lambda="auto") 

hist(log_pumpkin, breaks = 20, 
     col = "lightgreen", border = "lightgreen",
     ylim = c(0, 800) )
hist(BC_pumpkin, breaks = 20, 
     col = "lightblue", border = "lightblue", 
     ylim = c(0, 800))

#The box-cox transformation seems to be the most normalized distribution. The range of the prices are not as
#abnormal as with no transformation.

#Question g:####
library(caret)
caret::nearZeroVar(pumpkin_data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
#Later in the recipe variables such as Type, Origin.District and repack will be eliminated. In the whole dataset,
#these three predictors are classified as near zero variance, but later when the data is split up into train/test
#or CV/bootstrap.

#Question h:####
library(ggplot2)
library(DataExplorer)
library (GGally)

str(pumpkin_data)
pairs(pumpkin_data[,c(6:9, 16:18)])
ggpairs(pumpkin_data, columns = c(6:9, 16:18))

plot_histogram(pumpkin_data)

pumpkin_data %>%
  group_by(month) %>% 
  summarise(price = mean(price)) %>% 
  ggplot(aes(x = month, y = price)) +
  geom_col(fill = "midnightblue", alpha = 0.7) +
  ylab("Pumpkin Price")










