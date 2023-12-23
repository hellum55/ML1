pumpkin <- read.csv("~/ML1/Exam2023/pumpkintrain.csv", stringsAsFactors=TRUE)
pumpkin <- na.omit(pumpkin)

set.seed(1) #for replicability
train<-sample(991,693) #this picks random 196 numbers among 1, 2,...,392.
attach(pumpkin)

#Performing regressions and validation set approach manually
lm1<-lm(price~Package, subset=train)
mean((price-predict(lm1,pumpkin))[-train]^2) #Test MSE of linear regression
#Fits polynomial of degree 2
lm2<-lm(price~poly(Package, 2), subset=train)
mean((price-predict(lm2,pumpkin))[-train]^2)
#Fits polynomial of degree 3
lm3<-lm(price~poly(Package, 3), subset=train)
mean((price-predict(lm3,pumpkin))[-train]^2)
#Fits polynomial of degree 4
lm4<-lm(price~poly(Package, 4), subset=train)
mean((price-predict(lm4,pumpkin))[-train]^2)

############ Cross Validation
library(boot)
##First we apply LOOCV
cv.error<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:5){
  glm.fit<-glm(price~poly(Package,i), data=pumpkin)  
  cv.error[i]<-cv.glm(pumpkin, glm.fit)$delta[1] ##note we skipped K part and chose delta[1]  
}
cv.error  #note that it takes several seconds to get output.

##Now we apply k-fold CV with k=10
cv.error.10<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:5){
  glm.fit<-glm(price~poly(Package,i), data=pumpkin)  
  cv.error.10[i]<-cv.glm(pumpkin, glm.fit, K=10)$delta[1] ##we set K=10 and chose delta[1]  
}
cv.error.10   #here result is produced very quickly.


#glm function runs n many separate regressions for LOOCV and hence is slow. 
#Let's use the formula for linear models that we saw in the lecture.
cv.error.formula<-rep(NA, 10) #space for 10 values now set to NA. 
for(i in 1:5){
  lm<-lm(price~poly(Package,i), data=pumpkin)  
  cv.error.formula[i]<-mean((residuals(lm)/(1-hatvalues(lm)))^2) #hatvalues has h_i-s 
}
cv.error.formula

############ Bootstrap

##Regression example
#Function that gets beta coefficients for each bootstrap sample
coef.fn<-function(data, index){  
  coef(lm(price~Item.Size, data=data, subset=index))
}
boot(pumpkin,coef.fn, R=1000)
#Bootstrap Statistics :
#original       bias    std. error
#t1*  189.166667  0.009507422    3.423110
#t2*  -33.015893 -0.070149443    5.434024
#t3*  -45.473773  0.034368461    4.531675
#t4*  -54.926941 -0.138349411    6.208904
#t5*   -5.770417  0.026053229    7.784459
#t6* -105.095238 -0.085472373    6.184658
#t7*    0.752761  0.107304946    6.162726

########### Subset Selection
library(leaps)
library(dplyr)
dim(pumpkin)
relocate(.data = pumpkin, .after = year)

##Best Subset Selection
reg.full<-regsubsets(price~., nvmax = 50, data=pumpkin)
summary(reg.full)
names(summary(reg.full))
plot(summary(reg.full)$cp, xlab="No of Variables", ylab="Cp", type="l") #figure of Cp
#minimize by Cp
which.min(summary(reg.full)$cp) #at which p does Cp attain minimum? This produces p=10
coef(reg.full, 7) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.full)$bic)
coef(reg.full, 7)

##Forward Stepwise Selection
reg.fwd<-regsubsets(price~., nvmax=50, method="forward", data=pumpkin)
summary(reg.fwd)
#minimize by Cp
which.min(summary(reg.fwd)$cp) #at which p does Cp attain minimum? This produces p=36
plot(summary(reg.fwd)$cp, xlab="No of Variables", ylab="Cp", type="l") #figure of Cp
#minimize by Cp
coef(reg.fwd, 36) #here we can find coefficients of the model.
#minimize by BIC
which.min(summary(reg.fwd)$bic)
coef(reg.fwd, 29)

##Best Subset Selection with 10-fold Cross validation

k<-10
n<-nrow(pumpkin)
set.seed(1)
folds<-sample(rep(1:k, length=n)) #selects k fold
cv.errors<-matrix(NA, k, 10) #readies kx19 matrix space for cv errors


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
  
  reg<-regsubsets(price~Item.Size, data=pumpkin[folds!=j, ], nvmax=10)
  
  for(i in 1:10){
    
    pred<-predict.regsubsets(reg, newdata=pumpkin[folds==j, ], id=i)
    
    cv.errors[j,i] <-mean((pumpkin$price[folds==j]-pred)^2)
  }
}
#Now, we calculate average MSE across 10 folds
mean.cv.errors<-apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)

plot(mean.cv.errors, type="b")  # we can also this graphically

#We already estimated 10-variable model in Cp case. Below code leads to the same model coefficients.
reg.cv<-regsubsets(price~Item.Size, nvmax=10, data=pumpkin)
coef(reg.cv, 6)
