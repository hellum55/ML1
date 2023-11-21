#online source
bostonbi <- read.table(
  file = "http://lib.stat.cmu.edu/datasets/boston_corrected.txt",
  sep = "\t",
  skip = 10,
  header = FALSE, 
  stringsAsFactors = TRUE,
)

colnames(bostonbi) <- c("OBS.", "TOWN",	"TOWN.",	"TRACT",	"LON",	"LAT",	"MEDV",	"CMEDV",	"CRIM",	"ZN",	"INDUS",	"CHAS",	"NOX",	"RM",	"AGE",	"DIS",	"RAD",	"TAX",	"PTRATIO",	"B",	"LSTAT")

bostonbi <- subset (bostonbi, select = -c(1, 2, 3, 4, 5, 6, 7))

names(bostonbi) <- tolower(names(bostonbi))

names(bostonbi)[names(bostonbi) == "cmedv"] <- "medv"

colSums(is.na(bostonbi))

str(bostonbi)
head(bostonbi)

write.csv(bostonbi,"~/Desktop/BostonBI.csv")

summary(bostonbi$tax)
summary(bostonbi$medv)

#density plots
library(scales) #library for manipulating axis labels
#reguar $-scale
ggplot(bostonbi) +
  geom_density(aes(x=medv)) +
  scale_x_continuous(labels=dollar)

#histograms (continuous/integer)
ggplot(bostonbi) +
  geom_histogram(aes(x=tax), binwidth = 5, fill = "gray")















