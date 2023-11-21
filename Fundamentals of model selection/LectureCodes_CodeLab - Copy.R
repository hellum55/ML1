

#loading
library(ISLR2)
set.seed(1) #for replicability
train<-sample(392,196) #this picks random 196 numbers among 1, 2,...,392.
attach(Auto)

#Performing regressions and validation set approach manually
lm1<-lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm1,Auto))[-train]^2) #Test MSE of linear regression
#Fits polynomial of degree 2
lm2<-lm(mpg~poly(horsepower, 2), subset=train)
mean((mpg-predict(lm2,Auto))[-train]^2)
#Fits polynomial of degree 3
lm3<-lm(mpg~poly(horsepower, 3), subset=train)
mean((mpg-predict(lm3,Auto))[-train]^2)


############ Cross Validation
library(boot)
##First we apply LOOCV
cv.error<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:10){
glm.fit<-glm(mpg~poly(horsepower,i), data=Auto)  
cv.error[i]<-cv.glm(Auto, glm.fit)$delta[1] ##note we skipped K part and chose delta[1]  
}
cv.error  #note that it takes several seconds to get output.

##Now we apply k-fold CV with k=10
cv.error.10<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:10){
  glm.fit<-glm(mpg~poly(horsepower,i), data=Auto)  
  cv.error.10[i]<-cv.glm(Auto, glm.fit, K=10)$delta[1] ##we set K=10 and chose delta[1]  
}
cv.error.10   #here result is produced very quickly.


#glm function runs n many separate regressions for LOOCV and hence is slow. 
#Let's use the formula for linear models that we saw in the lecture.
cv.error.formula<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:10){
  lm<-lm(mpg~poly(horsepower,i), data=Auto)  
  cv.error.formula[i]<-mean((residuals(lm)/(1-hatvalues(lm)))^2) #hatvalues has h_i-s 
}
cv.error.formula

############ Bootstrap

## Portfolio Example
#Function for alpha
View(Portfolio)  ### this is simulated data of X and Y with n=100.
alpha.fn<-function(data, index){
  X<-data$X[index]
  Y<-data$Y[index]
  (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
}
alpha.fn(Portfolio, sample(100,100, replace=T)) #just trying with one bootstrap sample
#Now with boot() function
boot(data=Portfolio, statistic=alpha.fn, R=1000) # here R is number of bootstrap samples (B)
#note that these standard errors based on seed(1) are somewhat different than
#results in the book (Ch 5.3.4) which uses seed(7)

##Regression example
#Function that gets beta coefficients for each bootstrap sample
coef.fn<-function(data, index){  
  coef(lm(mpg~horsepower, data=data, subset=index))
}

boot(Auto,coef.fn, R=1000)



########### Subset Selection
library(ISLR2)
#install.packages("leaps")
library(leaps)
View(Hitters)
Hitters<-na.omit(Hitters) ## its dimension is 263x20
dim(Hitters)

##Best Subset Selection
reg.full<-regsubsets(Salary~., nvmax=19, data=Hitters)
summary(reg.full)
names(summary(reg.full))
plot(summary(reg.full)$cp, xlab="No of Variables", ylab="Cp", type="l") #figure of Cp
#minimize by Cp
which.min(summary(reg.full)$cp) #at which p does Cp attain minimum? This produces p=10
coef(reg.full, 10) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.full)$bic)
coef(reg.full, 6)

##Forward Stepwise Selection
reg.fwd<-regsubsets(Salary~., nvmax=19, method="forward", data=Hitters)
summary(reg.fwd)
#minimize by Cp
which.min(summary(reg.fwd)$cp) #at which p does Cp attain minimum? This produces p=10
coef(reg.full, 10) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.fwd)$bic)
coef(reg.full, 6)

##Best Subset Selection with 10-fold Cross validation

k<-10
n<-nrow(Hitters)
set.seed(1)
folds<-sample(rep(1:k, length=n)) #selects k fold
cv.errors<-matrix(NA, k, 19) #readies kx19 matrix space for cv errors


#Unfortunately regsubsets() does not have built-in predict function.
#So we write one for us below. You may suppose this as given.

predict.regsubsets<-function(object, newdata, id){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form, newdata)
  coefi<-coef(object, id=id)
  xvars<-names(coefi)
  mat[, xvars]%*%coefi
}


#This function calculates 10x19 cross validation errors, fold and variable wise.
for(j in 1:k){
  
  reg<-regsubsets(Salary~., data=Hitters[folds!=j, ], nvmax=19)
  
  for(i in 1:19){
    
    pred<-predict.regsubsets(reg, newdata=Hitters[folds==j, ], id=i)
    
   cv.errors[j,i] <-mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
#Now, we calculate average MSE across 10 folds
mean.cv.errors<-apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)  #This yield that 10-variable model has minimum CV error

plot(mean.cv.errors, type="b")  # we can also this graphically

#We already estimated 10-variable model in Cp case. Below code leads to the same model coefficients.
reg.cv<-regsubsets(Salary~., nvmax=19, data=Hitters)
coef(reg.cv, 10)
  