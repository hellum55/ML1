#Loading, Manipulating, and Visualizing Boston Data

library(ggplot2)

#Regular BOSTON data is available from library(ISLR2) or library(MASS) in 1st edition. 
#The data we are interested in is the updated version of this Boston dataset.
#For details, read documentation https://nowosad.github.io/spData/reference/boston.html#format

boston2 <- read.delim(
  "http://lib.stat.cmu.edu/datasets/boston_corrected.txt", skip = 9)

# read.delim is a sub-version of read.table for txt files.
#for some reason read.table gives an error about unequal column names in this file.
#note that skipping 7 lines also works instead of 9, but skipping 5 lines does not work.


#check the data for size and missing values
head(boston2)
dim(boston2)
sum(is.na(boston2))


#remove features (about longitude, latitude, tract)
#smart way to do this is 
#note that ! is negation operator for logicals, and %in% is an element operator for sets
boston <- boston2[,!names(boston2) %in% c("OBS.","TOWN","TOWN.","TRACT","LON","LAT","MEDV")]
# we could also check column labels and perform boston2[,8:21].

#rename CMEDV and "medv"
colnames(boston) <- tolower(colnames(boston))
colnames(boston)[1] <- "medv"
head(boston)
attach(boston)

#and save it locally as .csv
write.table(boston, "bostonBI.csv", sep=',', row.names= FALSE)
test <- read.csv("bostonBI.csv",sep=',')

#provide summary statistics
summary(boston[,c("tax","medv")])
cor(tax,medv)


#visualize single variables menv, tax
library(scales) #library for manipulating axis labels
#reguar $-scale
test2 <- ggplot(boston) +
  geom_density(aes(x=medv)) +
  scale_x_continuous(labels=dollar)




#create factor for tax based on discretization
taxlabel <- c("low","medium","high")
tax_discrete <- 0 + (tax >= 300) + (tax > 600)
tax2 <- factor(tax_discrete, 0:2, labels=taxlabel)
#note that tax2<-factor(tax_discrete) converts to factors with levels but no labels.

#add to data:
boston <- cbind(boston,tax2)
#or# boston$tax2 <- tax2

#summarize and visualize medv by defined tax groups
tapply(boston[,"medv"],tax2,summary)

ggplot(boston, aes(x=tax2, y=medv)) + 
  geom_boxplot()


#scatterregression (tax, medv)
ggplot(boston, aes(x=tax,y=medv)) +
  geom_point() + 
  ylim(0,50) +
  stat_smooth(method="lm")
  #geom_smooth()











