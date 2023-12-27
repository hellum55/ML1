# ******************************************
# Principal Component Analysis (Chapter 10)
# Applied Ex. 10 a) and b) 
# Solution 
# ******************************************


library(ISLR)

# a. Generate a simulated data with 20 observations in each of three classes 
#    (i.e. 60 observations in total), and 50 variables. 

set.seed (123, "L'Ecuyer")
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
# rnorm() simulates 20 obs. for 3 classes (60 in total) and 50 variables -->
# the 50 variables have a normal distribution with mean=0 and sd=0.001
View(x)


# Now we add a mean shift to the observations in each class to make sure there are 3 distinct classes 
# 1st class
x[1:20, 2] = 1

# 2nd class
x[21:40, 1] = 2
x[21:40, 2] = 2

# 3rd class
x[41:60, 1] = 1

# The idea here is to separate the three classes amongst two dimensions.


# b.Now perform PCA
pca.out = prcomp(x)
# pca.out
screeplot(pca.out, type="line") # displays the actuak variance

# to obtain the % of variance
pca.out$sdev # take the std deviations
pca.var =pca.out$sdev^2 # square them
pve=pca.var/sum(pca.var) # calculate proportion
pve
# visualize 
par(mfrow=c(1,2))
plot (pve, xlab = "PCA", ylab = "% of Variance Explained",
      ylim=c(0,1), type='b')
plot (cumsum(pve), xlab = "PCA", ylab = "Cummulative % of Variance Explained",
      ylim=c(0,1), type='b')


summary(pca.out)
pca.out$rotation #loadings
pca.out$rotation[,1:2] # loadings on the first two components

# Visualize the biplot
par(mfrow=c(1,1))
biplot(pca.out, scale=0) 

# See the scores on the first two principal components * See NOTE at the end
pca.out$x[,1:2]

# plot the scores (this plot is similar to the biplot except that it omits the vector of loadings)
plot(pca.out$x[,1:2], col=1:3, xlab="PC1", ylab="PC2", pch=19)

# Observe there are three main classes of observations:
#  - red class scores negatively on both PC1 and PC2
#  - black class scores positively high on PC1 
#  - green class scores positively green on PC2
# The meaning of components is irrelevant here because we simulated the data.


# NOTE 
dc <- scale(x,scale=FALSE)     # center the data but do not standardize it
L <- pca.out$rotation[,1:2]
sc <- dc %*% L  
sc
# as you can see the scores are linear combinations of the loadings and teh centered data

# End. 