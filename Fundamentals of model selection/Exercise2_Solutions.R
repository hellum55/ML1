
########### Exercise 1
##Exercise 1.1
#set up parameters
M <- 1000
beta <- 0.05
n <- 100
p.grid <- seq(6,50)
select.adj.r2 <- matrix(NA,M,45) 
select.BIC <- matrix(NA,M,45) 
select.AIC <- matrix(NA,M, 45) 

##Exercise 1.2
#initiate loop

for (m in seq(1,M)){
  #report progress (1 = complete)
  print(m/M)
  #simulate TRUE data-generating process from Equation (1)
  x <- matrix(rnorm(n*50),n,50)
  y <- x[,1:5]%*%rep(beta,5) + rnorm(n,0,1) #here %*% is matrix multiplication.
  #estimate model using the true 5 predictors
  mod_true <- lm(y~x[,1:5])
  #store MS criterion for largest model
  adj.r2.true <- summary(mod_true)$adj.r.squared
  bic.true <- BIC(mod_true)
  aic.true <- AIC(mod_true)
  
  #loop over models of size from 6,7,...,50 predictors
  for (p in p.grid){
    #estimate model with p predictors (including the 5 true ones)
    mod.p <- lm(y~x[,1:p])
    #save model selection decision based on all 3 criteria
    select.adj.r2[m,p-5] <- (adj.r2.true > summary(mod.p)$adj.r.squared) #R2 higher = better
    select.BIC[m,p-5] <- (bic.true < BIC(mod.p)) #BIC lower = better
    select.AIC[m,p-5] <- (aic.true < AIC(mod.p)) #AIC lower = better
  }
  
}

# Exercise 1.3 report share of correct selection of true model
colMeans(select.adj.r2)
colMeans(select.AIC)
colMeans(select.BIC)

#plot results
##It is possible to plot these lines by using the built-in plot function.

library(ggplot2)
library(reshape2)
plot.data <- data.frame("Degrees.of.Freedom"=p.grid,
                        "Adjusted.R2"=colMeans(select.adj.r2),
                        "BIC"=colMeans(select.BIC),
                        "AIC"=colMeans(select.AIC))

melted = melt(plot.data, id.vars="Degrees.of.Freedom")
colnames(melted)[2] <- "criterion"
ggplot() + geom_line(data=melted, 
                     aes(x=Degrees.of.Freedom, y=value, group=criterion, color=criterion))

## You may also do this plot simply by plot function as below, uncomment it if you want to run
#plot(p.grid, colMeans(select.adj.r2) , type="l", col="red", xlab="Degrees of Freedom", 
#     ylab="Probabilities", ylim=c(0.4,1.1))
#lines(p.grid, colMeans(select.AIC), type="l", col="blue")
#lines(p.grid, colMeans(select.BIC), type="l", col="green")

#interpretation as in lecture:
#Here we have a true model. We know that BIC is consistent, 
#so should select true model with probability close to 1.
#AIC and R2 are inconsistent. R2 actually converges to random model selection (value = 50).
#AIC is better but is aimed at low risk prediction and not consistent model selection and 
#thus the probabilities do not reach 1. 



###### Exercise 2

#Exercise 2.1 load and manipulate data

library(ISLR2)
names(Smarket)
dim(Smarket)
data <- Smarket

#set outcome numeric
data$Direction <- (data$Direction == "Up")*1   ###This converts Up/Down to 0/1

#split into training and test set by year for prediction. Remove year and Today (cols 1 and 8).
data.train <- data[data[,"Year"]!=2005,-c(1,8)]
data.test <- data[data[,"Year"]==2005,-c(1,8)]

#Exercise 2.2: Model Estimation


##This is logit with all variables
mod.logit <- glm(Direction ~., family = binomial, data= data.train)

##Unfortunately, regsubsets() function is for linear models and does not support logit.
## bestglm package does regsubsets kind of things for logit and other families.
# install.packages("bestglm")
library(bestglm)
#bestglm function requires input in stacked X y form where the dependent variable is the last column.
#so we set a separate data frame for this function.
train.bglm <- data.train[, c("Lag1", "Lag2", "Lag3", 
                          "Lag4", "Lag5", 
                          "Volume", "Direction")]


#now we can apply this
mod.logit.aic <- bestglm(train.bglm, IC = "AIC",family=binomial, method = "exhaustive")

#compare coefficients

mod.logit$coef
mod.logit.aic$BestModel$coef


#logit model is a full model, while AIC optimal model is just an intercept only model. So we can't compare more than this.
#AIC model predicts: Market goes up with 50.8016%,  use 1/(1+exp(-beta)) formula.
#This is independent of previous information, and 
#hence no value in looking at return history or volume. 



#Exercise 2.3: probabilities and 
#extract probabilities and class prediction
p.logit <- predict(mod.logit, newdata = data.test, type = "response")
p.aic <- predict(mod.logit.aic$BestModel, newdata = data.test, type = "response")


#compare:
cbind(p.logit,p.aic) 







#Exercise 2.4: Calculate number of predicted days
y.test <- data.test$Direction
y.logit <- (p.logit > 0.5)
y.aic <- (p.aic > 0.5) #always guesses market goes up

c(sum(y.test == y.logit),
  sum(y.test == y.aic))


#Correctly predicted days: 
#AIC(141) > logit(121) out of 252
#logit does worse than random guessing.




