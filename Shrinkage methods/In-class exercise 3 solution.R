

############### Exercise 1

##Exercise 1.1
#this is data data that saved while doing In Class Exercise Set 1:
Boston <- read.csv("bostonBI.csv")
attach(Boston)
head(Boston)

##Exercise 1.2

####subset selection###
library(leaps) #we need this package for subset selection

##Best subset selection up to all p=13 regressors
reg.full <- regsubsets(medv ~.,Boston, nvmax = 13)
#summaries
reg.sum <- summary(reg.full)
names(reg.sum)
#optimal model sizes p:
which.max(reg.sum$rsq)  #leads to p=13
which.min(reg.sum$cp)  #leads to p=11
which.min(reg.sum$bic)  #leads to p=11

#get coefficients of optimal model (p=11)
coef(reg.full, 11)
#we see that indus and age are not included in the selected model.


##Exercise 1.3 

#forward + backwards selection
reg.fwd = regsubsets(medv ~., Boston, nvmax = 13, method = "forward")
summary(reg.fwd)
reg.bwd = regsubsets(medv ~., Boston, nvmax = 13, method = "backward")
summary(reg.bwd)

#compare (p=11 turns out to optimal model in both cases- so all optimal models coincide
cbind(coef(reg.full,11),coef(reg.fwd,11),coef(reg.bwd,11))


###Exercise 1.4
library(glmnet)
#get data in (x,y) format (without intercept)
X <- model.matrix(medv~., Boston)[,-1]
y <- Boston$medv

#decreasing lambda grid from 1000 to 0.01 
l.grid <- 10^seq(3,-2,length=100)

#estimate ridge regression along grid
l2.mod <- glmnet(X,y,alpha=0,lambda = l.grid)

#obtain compare lambda and coefficients and l2 norm
gp <- c(1,50,100)
l2.mod$lambda[gp[1]]
l2.mod$lambda[gp[2]]
l2.mod$lambda[gp[3]]

#Next we compare coefficients
#First we ols
reg.lm<-lm(y~X, Boston)
#Comparing coefficients of three ridge models and ols shows that coefs of lambda=0.01 
#ridge model is very close and ridge with lambda=1000 has all coefficents close to 0.
#These are consistent with expectations.
round(cbind(coef(l2.mod)[,gp], coef(reg.lm)), digits=5)
#compare ell-2 norm
#again we see that ell 2 norm of lambda=0.01 case is very close to ols,
#while that of lambda=1000 is close to 0.
c(sqrt(colSums(coef(l2.mod)[ -1 , gp]^2) ), sqrt(sum(coef(reg.lm)[-1]^2)))

#look at coefficients along grid.
plot(l2.mod, xvar = "lambda")


###Exercise 1.5

##Now we evaluate the different cv methods to select optimal lambda

#LOOCV using cv.glmnet
n <- length(y)  #sample size
cv.out<-cv.glmnet(X,y,alpha=0, nfolds = n, lambda = l.grid)
bestlam1<-cv.out$lambda.min

#now 5- and 10-fold using cv.glmnet
set.seed(1) #set seeds for replicability as folds are random
cv5.out = cv.glmnet(X,y,alpha=0, nfolds = 5, lambda = l.grid)
bestlam2 =cv5.out$lambda.min
cv10.out = cv.glmnet(X,y,alpha=0, nfolds = 10, lambda = l.grid)
bestlam3 =cv10.out$lambda.min

#we compare three obtained optimal lambda estimates
#LOOCV is smaller (0.091) than other two (which are 0.145 and 0.163)
c(bestlam1, bestlam2, bestlam3)


###Exercise 1.6
# Estimate lasso regression along the same grid numbers for lambda
l1.mod <- glmnet(X,y,alpha=1,lambda = l.grid)

#we pick these three coordinates in the lambda grid
gp <- c(1,80,100)
#actual lambda values of them are 1000, 0.1 and 0.01
l1.mod$lambda[gp[1]]
l1.mod$lambda[gp[2]]
l1.mod$lambda[gp[3]]

#compare coefficients with OLS
#At lambda=1000 all coefficients are zero, at lambda=0.1 indus and age are excluded.
cbind(coef(l1.mod)[,gp],coef(reg.lm)) 
#compare the l1 norm of coefficients with OLS
#again expected outcomes for ell 1 norm results
c(colSums(abs(coef(l2.mod)[ -1 , gp]) ),sum(abs(coef(reg.lm)[-1])) )

#plot coefficient path
plot(l1.mod, xvar = "lambda")


##Exercise 1.7
#obtain cross-validated lambda
#here only 5-fold using cv.glmnet
cvl1.out = cv.glmnet(X,y,alpha=1, nfolds = 5, lambda = l.grid)
laml1 =cvl1.out$lambda.min #this gives 0.02
