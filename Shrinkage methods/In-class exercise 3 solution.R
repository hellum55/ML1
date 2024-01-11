

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
cbind(coef(reg.fwd,11),coef(reg.bwd,11))


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

##Exercise 1.8
#Let's define predictions models, remember here we use training set as x.
l2.pred <- predict(l2.mod, s=bestlam2, newx = X)
l1.pred <- predict(l1.mod, s=laml1, newx = X)
lm.pred <- predict(reg.lm)
#get predictions, summary statistics of prediction of three models are very close.
summary(l2.pred)
summary(l1.pred)
summary(lm.pred)


#compare to OLS, correlations are 0.99
cor(lm.pred,l2.pred)
cor(lm.pred,l1.pred)



###Exercise 1.9
#we randomize all data
sample_id <- sample(1:dim(Boston)[1],dim(Boston)[1], replace = FALSE)

train_id <- sample_id[1:404]
test_id <- sample_id[405:506]


#Define training/test response and predictors
y_test <- y[test_id]
y_train <- y[train_id]
x_test<-X[test_id,]
x_train<-X[train_id,]


#Set space for test set mse
mse <- matrix(0,1,4)
colnames(mse) <- c("OLS","L2","L1","ENET")
#OLS
lm.high <- lm(medv~., data=Boston[train_id,])
mse[1,"OLS"] <- mean((y_test - predict(lm.high,newdata=Boston[test_id,]))^2)


##L2
l2.model <- cv.glmnet(x_train,y_train,alpha=0,nfolds = 5)
l2.lam = l2.model$lambda.min
l2.model.pred <- predict(l2.model, s=l2.lam, newx = x_test)
mse[1,"L2"] <- mean((y_test - l2.model.pred)^2)

##L1
l1.model <- cv.glmnet(x_train,y_train,alpha=1,nfolds = 5)
l1.lam = l1.model$lambda.min
l1.model.pred <- predict(l1.model, s=l1.lam, newx = x_test)
mse[1,"L1"] <- mean((y_test - l1.model.pred)^2)

##ENET
l12.model <- cv.glmnet(x_train,y_train,alpha=0.5,nfolds = 5)
l12.lam = l12.model$lambda.min
l12.model.pred <- predict(l12.model, s=l12.lam, newx = x_test)
mse[1,"ENET"] <- mean((y_test - l12.model.pred)^2)
##Now displaying results
mse/mse[1,1]

# lasso, ridge and elastic net are all close to each other
#and have 0.5-1% prediction gain over ols.
#These gains are not surprising because p=13 in this example is small relative to n. 



######################### Exercise 2
##Exercise 2.1
#load data
lymphx <- read.table("lymphx.txt", quote="\"", comment.char="")
lymphstatus <- read.table("lymphstatus.txt", quote="\"", comment.char="")
#save data in matrix form 
x <- as.matrix(lymphx)
y <- as.matrix(lymphstatus)

#let's try to run a logistic regression
glm(y ~ x, family = "binomial")
#logit model is not feasible. Indeed, we see that algorithm does not converge due to p > n 
#Similar to ols, we can estimate logistic regression with maximum likelihood if p> n. 
#because there will be more parameter values than corresponding moment equations set to zero.

##Exercise 2.2
#optimal tuning parameters for lasso 
library(glmnet)
set.seed(100)
alpha = 1 #for lasso
mod1.cv <- cv.glmnet(x,y,family ="binomial",type.measure = "deviance",alpha=alpha, nfolds = 10)
mod2.cv <- cv.glmnet(x,y,family ="binomial",type.measure = "class",alpha=alpha, nfolds = 10)

#to get an initial idea, let's check which lambdas produce minimizations
mod1.cv$lambda.min #this gives 0.07
mod2.cv$lambda.min #this gives 0.04


#Concerning plotting, simple plot of cv.glmnet object produces both average and 
#also upper and lower bounds as a function of log lambda
plot(mod1.cv)
plot(mod2.cv)


#to be sure about the upper and lower curves, we need to it manually as requested by the task
library(ggplot2)
#for model 1, unlike above these plots as a function of lambda
df1 <- data.frame("lambda" = mod1.cv$lambda,"cvm" = mod1.cv$cvm,"cvup" = mod1.cv$cvup,
                  "cvlo" = mod1.cv$cvlo)
ggplot(df1, aes(x=lambda)) +
  geom_point(aes(y=cvm), color ="blue") +
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), color = "lightblue") +
  theme_minimal()
#for model 2
df2 <- data.frame("lambda" = mod2.cv$lambda,"cvm" = mod2.cv$cvm,"cvup" = mod2.cv$cvup,
                  "cvlo" = mod2.cv$cvlo)
ggplot(df2, aes(x=lambda)) +
  geom_point(aes(y=cvm), color ="blue") +
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), color = "lightblue") +
  theme_minimal()
#Both simple plot version and this ggplot clearly show deviance measure leads to stable optimum
#class plot has a sort of zigzag pattern with unclear convergence.
#For this reason, we continue with deviance measure.

##Exercise 2.3
#run lasso with deviance


#post-lasso
#choose selected coefficients 
#this gives us coefficients of model 1 excluding its intercept, we are looking for predictors
mod1.coef <- predict(mod1.cv, type="coefficients",s=mod1.cv$lambda.min)[2:7400,]

#now we store index of corresponding selected variables, there are 27 such variables.
incl.lasso<-which(mod1.coef!=0)

#store corresponding coefficients
par.lasso<-mod1.coef[incl.lasso]
#We find that 27 genes are relevant for our prediction task. 
#and we know which genes those are, gene # 30, 80, 773, ..., 7357.

##Exercise 2.4
#run post-lasso and store coefficients
mod.post.lasso <- glm(y~x[,incl.lasso],family ="binomial")
par.post.lasso <- mod.post.lasso$coefficients[-1]

#compare coefficients
cbind(par.lasso, par.post.lasso)
##We observe that lasso coefficients are smaller than post-lasso due to l1-shrinkage.

#active coefficients + where sign disagrees
c(length(par.lasso),sum(sign(par.lasso) != sign(par.post.lasso)))
#We see that only 4 out of 27 gene signs disagree, and we can locate those.
#Sign agreement is important because then both increase or decrease death of patient on average.
#Overall, such sign disagreements are small (4/27) and this is overall good news for the models.



