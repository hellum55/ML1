#Q1:####
data_boston <- read.csv("~/ML1/Shrinkage methods/bostonBI.csv")

#Remove outliers:
data_boston <- data_boston[!(row.names(data_boston) %in% c("381","419")),]

#Q2:####
library(leaps)
regfit.full <- regsubsets(medv~.,data = data_boston, nvmax = 13)
reg.summary <- summary(regfit.full)
plot(reg.summary$cp, xlab = "Number of variables", ylab = "cp")
which.min(reg.summary$cp)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC")
which.min(reg.summary$bic)
#Plot method for regsubsets:

plot(regfit.full, scale = "Cp")
coef(regfit.full, 11)

plot(regfit.full, scale = "bic")
coef(regfit.full, 10)

#When measuring for Cp, the model with the lowest Cp doesnt contain
#industry and age, but includes all the others.
#When measuring for BIC the best model doesnt contain crim, industry
#and age.

#Q3: ####
#Forward stepwise selection - a greedy method
regfit.fwd <- regsubsets(medv~., data = data_boston,
                         nvmax = 13,
                         method = "forward")

#Backward stepwise selection:
regfit.bwd=regsubsets(medv~.,data=data_boston,nvmax=13,method="backward")
summary(regfit.bwd)

which.min(summary(regfit.bwd)$bic)
which.min(summary(regfit.fwd)$bic)

coef(regfit.fwd,6)
coef(regfit.bwd,10)

#Q4: ####
library(glmnet)
x=model.matrix(medv~., data_boston)[,-1]
y=data_boston$medv

#First we will fit a ridge-regression model by calling glmnet with
#alpha = 0. there is also a cv.glmnet function for cross-validation
grid=10^seq(3, -2, length = 100)
fit.ridge <- glmnet(x,y, alpha = 0, lambda = grid)
plot(fit.ridge, xvar="lambda", label = TRUE)
dim(coef(fit.ridge))
coef(fit.ridge)[,1]
coef(fit.ridge)[,50]
coef(fit.ridge)[,10]

gp <- c(1,50,100)
fit.ridge$lambda[gp[1]]
fit.ridge$lambda[gp[2]]
fit.ridge$lambda[gp[3]]

#OLS
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],
                   exact = T, x = x[train, ], y = y[train])
mean((ridge.pred-y.test)^2)

predict(ridge.mod, s = 0, exact = T, type = "coefficients",
        x = x[train, ], y = y[train])[1:14, ]

#We refit our ridge regression model on the full data set,
#using the value of λ chosen by cross-validation, and examine
#the coefcient estimates.
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:14, ]

#Q5: ####
############ Cross Validation
library(boot)
##First we apply LOOCV
n <- length(y)  #sample size
cv.out<-cv.glmnet(x,y,alpha=0, nfolds = n, lambda = grid)
bestlam1<-cv.out$lambda.min
bestlam1
plot(cv.out)

#10-fold cross validation:
set.seed(1)
cv.out10<-cv.glmnet(x,y,alpha=0, nfolds = 10, lambda = grid)
bestlam10<-cv.out10$lambda.min
bestlam10
#lines(degree, cv.error.10, type = "b", col = "red")

#5-fold cross validation:
cv.out5<-cv.glmnet(x,y,alpha=0, nfolds = 5, lambda = grid)
bestlam5<-cv.out5$lambda.min
bestlam5
#lines(degree, cv.error.5, type = "b", col = "blue")

#LOOCV = 0.072, 10CV = 0.091, 5CV = 0.145

#6: ####
#Important to use the same estimates as before: (lambda/grid)
fit.lasso <- glmnet(x,y,alpha = 1, lambda = grid)
plot(fit.lasso, xvar = "lambda", labels = T)
plot(fit.lasso, xvar = "dev", labels = T)

#Compare with OLS:
gp <- c(1,80,100)
#actual lambda values of them are 1000, 0.1 and 0.01
fit.lasso$lambda[gp[1]]
fit.lasso$lambda[gp[2]]
fit.lasso$lambda[gp[3]]

#compare coefficients with OLS
#At lambda=1000 all coefficients are zero, at lambda=0.1 indus and age are excluded.
cbind(coef(fit.lasso)[,gp],coef(OLS.model)) 
#compare the l1 norm of coefficients with OLS
#again expected outcomes for ell 1 norm results
c(colSums(abs(coef(fit.ridge)[ -1 , gp]) ),sum(abs(coef(OLS.model)[-1])))

#Suppose we want to use our earlier train/validation set to select
#the lambda for the lasso:
lasso.tr <- glmnet(x[train,], y[train])
lasso.tr
pred <- predict(lasso.tr, x[-train,])
dim(pred)
rmse.lasso <- sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda), rmse.lasso, type = "b", xlab="Log(lambda)")
lam.best <- lasso.tr$lambda[order(rmse.lasso)[1]]
lam.best
coef(lasso.tr, s=lam.best)

#Comparing Ridge, Lasso and OLS based on MSE:
lasso.mod=glmnet(x[train,],y[train],alpha=0,lambda=0.0065, thresh=1e-12)
lasso.pred=predict(lasso.mod,s=0,newx=x[test,])
mean((lasso.pred-y.test)^2)
#Looks to be a bit better than OLS

#Q7: ####
set.seed(1)
cv.lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 5, lambda = grid)
plot(cv.lasso)
bestlam.lasso <- cv.lasso$lambda.min
bestlam
#Best lampda is in this case 0.02

#Q8: ####
ridge.pred <- predict(fit.ridge, s = bestlam5,
                        newx = x)
mean((ridge.pred - y)^2)

lasso.pred <- predict(cv.lasso, s = bestlam.lasso,
                      newx = x)
mean((lasso.pred - y)^2)

OLS.pred <- predict(OLS.model)
mean((OLS.pred-y)^2)

summary(ridge.pred)
summary(lasso.pred)
summary(OLS.pred)

#compare to OLS, correlations are 0.99
cor(OLS.pred,ridge.pred)
cor(OLS.pred,lasso.pred)

#Q9: ####
set.seed(123)  # Setting seed for reproducibility
train_id <- sample(1:506, 404)
test_id <- setdiff(1:506, train_id)

x_train <- x[train_id,]
x_test <- x[test_id,]
y_train <- y[train_id]
y_test <- y[test_id]

#Ridge reg:
ridge.mod <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
bestlam.ridge <- ridge.mod$lambda.min
ridge.test <- predict(ridge.mod, s = bestlam.ridge, newx = x_test)
mean((y_test - ridge.test)^2)

#Lasso reg:
lasso.mod <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
bestlamlasso <- lasso.mod$lambda.min
lasso.test <- predict(lasso.mod, s = bestlamlasso, newx = x_test)
mean((lasso.test - y_test)^2)

#ENET
enet.mod <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 5)
bestlambda.enet <- enet.mod$lambda.min
enet.test <- predict(lasso.mod, s = bestlambda.enet, newx = x_test)
mean((enet.test - y_test)^2)




