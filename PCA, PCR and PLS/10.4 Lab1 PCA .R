######################################################
# Principal Components Analysis (PCA)           
# based on Chapter 10, Lab 1, ISL textbook 
# Ana Alina Tudoran, 2023               
######################################################


#___________________________________________________________________________
# Data "USArrests" from baseR package
# For each of the 50 states in the USA,the data contains the number of arrests 
# per 100,000 residents for each of three crimes: Assault, Murder, Rape. 
# In addition, the variable UrbanPop records the percent of the population 
# in each state leaving in urban areas.
#___________________________________________________________________________

# Objectives: 
# - Perform a PCA to reduce the number of variables to a smaller number of PCs
#   that will summarize most of the information in the data
# - Discover and visualize how the 50 USA states differ based on
#   the multiple number of variables taken simultaneously
# ____________________________________________________________________________


#______________________
##  1. Explore the data 
#______________________
View(USArrests)
states=row.names(USArrests)
states
names(USArrests)
str(USArrests) # all var are continuous (numeric and integer)
cormatrix <- cor(USArrests)
round(cormatrix, 2) 
# certain variables are correlated => PCA is justified

# We can see how the variables correlate and how the states differ 
# by ploting 2 variables at once: 
# for example, Murder and Assault
plot(USArrests$Murder, USArrests$Assault, xlab = "Murder", ylab = "Assault")
text(USArrests$Murder, USArrests$Assault, row.names(USArrests), cex=0.6, 
     pos=4, col="red")
# This plot is quiet informative. However, using PCA we want a plot summarizing 
# records and all variables in the data set with minimum loss of information


apply(USArrests, 2, mean) #  to get column means; 1 for row means
# on average there are 3 times as many rapes as murders, and more than 8 times 
# as many assaults as rapes
apply(USArrests, 2, var)  #  variances 
# variables have vastly different variances. 
# hence, it is important to standardize the variables 


## ____________________________________
##   2. Run PCA and evaluate the output
## ____________________________________
pr.out = prcomp(USArrests, scale=TRUE)
names(pr.out)   # List of 5 elements (see also in the Global Environment)

pr.out$x # PC scores for each record  
dim(pr.out$x) 
# These are the new surrogate variables created, which (selectively) can be 
# used in other analyses instead of the original variables

pr.out$sdev     # The std. deviations of each PC
# equivalent to:
sqrt(apply(pr.out$x, 2, var))

pr.out$center   # Mean of the original variables
pr.out$scale    # Std.dev.of the original variables
pr.out$rotation # PC loading vector; 
# high loading on a single component allow us 
# to assign meanings to principal component. 
# Here, murder, assault and rape characterize PC1; 
# urban population characterizes PC2.

# Note: the number of components extracted by the software is always min(n-1,p), 
# where n is the size of the dataset and p is the number of original variables. 
# In other words, the number of PCs is less than or equal to the number of 
# original variables.
# The analyst will have to select only a few PCs, otherwise the PCA analysis 
# does not justify. 


# ____________________________________________________________________________
## 3. Scree plot
# (evaluate % of variance explained, elbow rule, ..)
# ____________________________________________________________________________
# variance and % of variance explained by each PC (eigen values)
summary(pr.out)
# alternatively
   pr.var = apply(pr.out$x, 2, var)
   pve = pr.var/sum(pr.var)
   pve
# Notice, PCs are ranked by how much they describe the data
# PC1 explains 62% of the variance in the data, 
# PC2 explains 24.7% of the variance in the data, etc..

 
# Represent explained variance in a graph (scree plot)
par(mfrow = c(1,2))
plot(pve, xlab="Principal Components", 
     ylab= "Proportion of variance explained",
     ylim=c(0,1), type='b') # identify the elbow of the plot and choose the 
# number of PC above the elbow
plot(cumsum(pve), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1), type='b') # check the cumulative plot and make the decision 

# _____________________________________________________________________________
# The question is: Is this PCA meaningful? Are there sufficient correlated 
# variables in the dataset to reduce the dimension of the data set to fewer 
# than four variables?
#______________________________________________________________________________
# For data summarization and visualization, PCA is worthy only if the top 2 
# or 3 PCs cover most of the variation in our data. This way, one can represent 
# all the records based on those PC in a bi- or maximum tri-dimensional space.
# As we can see, in this case, the first two components explain together more 
# than 80% of total variation in the data. 
# Hence, PC1 and PC2 can thus be used as surrogate variables for the original 
# data set. Of note, the selection of PC based on the elbow rule or variance 
# explained is not entirely objective because it only uses the training data set
# (minimizing the training error). 
# In the next lecture, I will discuss another method of selecting the best number
# of PC, tuning this parameter as a function the prediction accuracy of PCs
# (i.e.,cv-testing error).

# ______________________________________________________________________________
## 4.Biplot -evaluate how data and variables are mapped in regard to PC extracted
# ______________________________________________________________________________

biplot(pr.out, scale=0) 
# the sign of the loading vectors may vary depending on the software used, 
# but the values are the same. 
# to obtain exactly the biplot from the ISL textbook, we can reverse the signs:
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)

# How do we read this biplot?  
# (1) by default the plot represents only the first two PCs 
# (2) the plot represents both the loading vectors and the PCs scores
# (3) the loadings are the ones given in the rotation table
# (4) Assault, Murder and Rape have approx equal loading on PC1. 
#     UrbanPop has much less weight on PC1.
# (5) UrbanPop has high loading on PC2. 
# (6) Assault, Murder and Rape are close to each other, meaning they are 
#     correlated with each other. 
# (7) UrbanPop is less correlated with the other three.
# (8) States that have similar profiles (scores) on PC1 and PC2, are clustered
#     together. 
# (9) If the states are different based on PC1, like the California 
#     and South Dakota, such differences are likely to be due to the variables 
#     that have heavy influences on PC1. These variables were the crime-related 
#     variables. Thus, states like California have high crime rate, while states 
#     like South Dakota, which scores in the opposite direction of PC1, have 
#     low crime rate.


# ____________________________________________________________________________
## To conclude, PCA is a tool for:
#  a) data visualization and summarizing (objective fulfilled in this script) 
#  b) dimension reduction for implementing supervised predictive techniques 
#  We approach objective b) next lecture under Principal Component Regression 
#  There we will use function pcr() from the library "pls" 
#  pcr() function can perform PCA on a mix of categorical & continuous variables 
#  By default, PCA is a technique applied to continuous variables and therefore 
#  prcomp() function from base R (implemented in this script) works only with 
#  continuous/numeric data.   
# ____________________________________________________________________________


