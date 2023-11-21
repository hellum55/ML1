
# ***************************************************************************************************
#                          Linear and Multiple Linear Regression Problems 
#                          Source: James et al., Introduction to Statistical Learning
# ***************************************************************************************************
                          #   Guidelines solutions to Applied Ex.8, 9, 10, 11, 14, 15    #
                                                 # Updated Version: 22 Sept 2021 # 

# Recommendation: First, consider reading the problem from the textbook, 
# reflect on it; afterwards look at the solution presented here.

  #######
  # Ex. 8
  #######

  # Upload/access and explore your data
    library(ISLR)
    ?Auto
    attach(Auto) 
    # This means that this particular database is searched by R when evaluating a variable; 
    # the objects in the database can be accessed by simply giving their names
    # detach(Auto)
    table(is.na(Auto))
    dim(Auto)
    summary(Auto)
    View(Auto)
    str(Auto)
    
    
    # "mpg" stands for miles per gallon, and is used to show how far your car is able 
    # to travel for every gallon (4.55 litres) of fuel it uses. 
    # As an example, if you own a car that can return 50 mpg and its fuel tank only 
    # has 1 gallon of petrol or diesel in it, you should be able to drive 50 miles 
    # before the car runs out of fuel.
    
    
    
  # (a) Objective:Investigate whether the engine horsepower is associated with miles per gallon
         lm.fit = lm( mpg ~ horsepower, data=Auto)
         # lm.fit$
         summary(lm.fit)
         # std. coefficients
            # install.packages("lm.beta")
            library(lm.beta)
            lm.beta(lm.fit) 
        
    
      # i) Is there a relationship? 
            # - Look at F-statistic + its p-value and then to the individual 
            #   p-values associated with the predictors
            # - Both are highly significant evidence of a possible association
         
      # ii) How strong is the relationship? 
            # - beta = -0.157845 meaning that 1 unit increase in horsepower 
            #   is associted with an average decrease in miles per gallon by around 0.15
            # - Look at R-squared (in SLR is equal to corr coef. squared): 
            #   the % of variance explained in the response is 0.6059 - quite high. 
            # - in MLR, check Adjusted R-squared which adjusts the statistic based on 
            #   the number of independent variables in the model 
            
            # - Look at RSE (4.906). RSE is considered a measure of the lack of fit of 
            #   the model to the data 
            # - In other words, the actual miles per gallon deviate from the true regression 
            #   line by approx. 4.91 miles on average. 
            # - Is this an acceptable prediction error? It depends on the context. 
            #   In our dataset, the mean value of mile per gallon (mpg) for the cars is 23.44
            #   Dividing RSE by the mean value for the response (23.44) indicates a percentage 
            #   error of roughly 21%.
            # 
      # iii) Direction or sign of the relationship 
            # - There is a negative asssociation. Horsepower is negatively asssocited with mpg.
         
      # vi) 95%CI for the coeficients
            confint(lm.fit, level=0.95)
            
      # iv) Predicted value of y if x = 98
            predict (lm.fit, data.frame(horsepower = 98), interval="confidence")
            predict (lm.fit, data.frame(horsepower = 98), interval="prediction")
            # - both intervals are centered at 24.4678. 
            # - Confidence interval is narrower because it reflects the uncertainty 
            #   surrounding the average mpg of all vehicles with a horsepower of 98
            # - Prediction interval is substantially wider because it reflects the uncertainty 
            #   surrounding a particular vehicle 
            # - Prediction 95% CI  for x=98 goes from 14.8 to 34.12 mpg
            # - We interpret the CI saying that 95% of intervals of this form, will contain 
            #   the true value of Y
            
  # (b) plot data and fitted line
            plot( mpg ~ horsepower, Auto)    
            abline(lm.fit,col='red')
            # dev.off() - function shuts down all open graphics 

  # (c) diagnostic plots
            par(mfrow=c(2,2))
            plot(lm.fit ) 
            # see video Brush up LR or go to p. 92 textbook and review assumptions 
            
            # extra
            plot(lm.fit, col="red", which=c(1))
            plot(lm.fit, col="red", which=c(2))
            plot(lm.fit, col="red", which=c(3))
            plot(lm.fit, col="red", which=c(4)) 
            plot(lm.fit, col="red", which=c(5))
            plot(lm.fit, col="red", which=c(6))
            
            cookdista <- round(cooks.distance(lm.fit),5)
            rev(sort(cookdista))
            ?cooks.distance
            
            
            # Write briefly the conclusions here. e.g. 
            
            # 1) Plot 1: the residuals exhibit a clear U-shape, providing strong indication 
            #            of non-linearity. Solution: Try to run a model using a non-linear 
            #            transformation of the predictor, X.
            # 2) Plot 2: the residuals exhibit some deviation from normal distribution 
                         # Solution: Explore and exclude the problematic observations; 
            #            or tranform X.  
            # 3) Plot 3: the residuals exhibit some "funel shape" distribution: The magnitude 
            #            of residuals tend to increase with fitted values. Non-constant variance.
            #            Solution: Check specific outliers, tranform Y using log or square-root, 
            #            re-run the regression.  
            # 4) Plot 4: identify observations with high leverage
            #            For this data, Obs. 117 could be one of them, but it does not deviate 
            #            extremly and it has low std. residual so we may decide to keep it. 
            
            # Optional
            #           plot(hatvalues(lm.fit))
            #           which.max(hatvalues(lm.fit)) 
            #           residuals(lm.fit) 
            #           rstudent(lm.fit)
            #           which.max(residuals(lm.fit)) 
            
           
            
            
  ########            
  # Ex. 9
  ########
            
      # (a) Same data file as Ex. 8 
      # Quantitative: mpg, cylinders, displacement, horsepower, weight,acceleration, year
      # Qualitative: name, origin
            
       # (a) scatterplot   
      pairs(Auto) 
      # alternartively: 
      plot(Auto$mpg, Auto$weight) # Heavier cars correlates with lower mpg.
      plot(Auto$mpg, Auto$cylinders) # More cylinders, less mpg.
      plot(Auto$mpg, Auto$year)# Cars become more efficient over time

      # (b) corr matrix
      library(corrplot)
      cm <- cor(Auto[, -c(9)], use = "complete.obs")
      cm
      corrplot(cm)
      corrplot(cm, type="upper", tl.pos="d", method="number", diag=TRUE)
      # Most of the predictors show some correlation with mpg 
      # But also many predictors correlate with each other?? We return to this point later
      
      
      # Why variable "name" is not used in the analysis? 
      # Merge levels by brands to be able to handle a decent number (see in a future application), 
      # do not use the variable - as required
      table(Auto$name)
      
      
      # (c) Run the model
      lm.fit <- lm(mpg ~ .-name, data = Auto)
      summary(lm.fit)
      # Comment on the output.
      # i) F-statistic rveals that..
      # ii) displacement, weigh, year, origin ...
      # iii) 0.750773 can be interpreted as ...
      plot(year, mpg)
      
  
      # (d) Diagnostic
      par(mfrow=c(2,2))
      plot(lm.fit)
      # Comment on the output 
      
      # (e) Interaction effect: horsepower:weight
      summary(update(lm.fit, . ~ . + horsepower:weight))
      
      # We may evaluate if this new model is indeed a better model 
      # Using "anova()" function which performs an analysis of variance in order to 
      # test the null hypothesis that a model (M1) is sufficient to explain the data against 
      # the alternative hypothesis that a more complex model (M2) is required 
      # anova () for model comparison only applies to nested models - 
      # meaning that the predictors in M1 should be a subset of the predictors in M2) 
      anova(lm.fit, update(lm.fit, . ~ . + horsepower:weight))
      # Concl.: the decrease in RSS is substantial for M2, so the interaction term 
      # is justified statistically. 
      # repeat the above two lines by trying other interaction effects 
      
      
      # (f) transformation of the predictors 
      plot(Auto$mpg, Auto$horsepower)
      # A visual inspection reveals a possible non-linear effect (curve line, U shape); 
      # Does this also make sense theoretically? 
      # Let´s try to model horsepower^2
      summary(update(lm.fit, . ~ . + I(horsepower^2))) 
      # and compare
      anova(lm.fit, update(lm.fit, . ~ . + I(horsepower^2)))
      # Conclude  
      
      
     # PS:  In this exercise, we neglected the colinearity of predictors. 
     #      Also, in these first exercises, we have not used distiguished between 
     #      training and validation datasets. We used only the training data set to 
     #      estimate our models and interpret the parameters. 
     #      This was the classsical approach.
     
      
     
      
       
  ########            
  # Ex. 10
  ########    
      # Objective: What predicts and/or explains the sales of cars in different stores? 
      # Is it Price, Income, Advertising, Population, salesman, time of the year, .. 
      # This information is important for managers because.. Briefly state the reasons
      # for selecting the predictors.
              # * because they can better plan the budgets
              # * target certain customers
              # * focus on offering specific type of cars
              # * ..
      
      
      library(ISLR)
      View(Carseats)
      ?Carsearts
      str(Carseats)
      
      # For initial data exploration, visualization, summarization, 
      # see ex. 15 or other applications.
      # Understanding the data (variables type, scales, missings, distributions, etc.) 
      # is very important. 
      
      # a) Befroe running,reflect on IV type
      mulreg_price = lm(Sales ~ Price + Urban + US, data=Carseats)
      summary(mulreg_price)
  
      # b)
      #   Write the findings as a discussion. 
      #  "Based on our results, we found singnificant evidence that higher prices 
      #   lead to less sales. We also show that stores in the US sell more car seats 
      #   than those which are not from US. 
      #   Finally, we do not find sufficient evidence at 5% regarding the effect of 
      #   urban variable, meaning that whether the store is an an urban location is 
      #   statistically insignificant. 
      #   More evidence is required to test is such hypothesis hold in reality [...]" 
      #   You may complement the discussion with the magnitude of the coefficients.
  
      # c)
    
   # Sales = \hat{\beta_0} + \hat{\beta_1}*Price + \hat{\beta_2}*isUrban + \hat{\beta_3}*inUS 
      # Where 
      #   - \hat{\beta_0} = 13.043469 
      #   - \hat{\beta_1} = -0.05449 
      #   - \hat{\beta_2} = -0.021916 
      #   - \hat{\beta_3} = 1.200573
      
      
      # d) For Price and USYes.
  
      # e)
        mulreg_price = lm(Sales ~ Price + US, data=Carseats)
        summary(mulreg_price)

       # f) both fit poorly
        # - only ~23.5% of the variance in sales is explained using the models
        # - compare RSE as well.
        # - for prediction this model is not good. i.e. it will make a lot of errors. 
        # However, if someone is interested in explanation (inference), 
        # knowing that price and location explains 23% of the variance in sales may be important.
  
        # g)
        confint(mulreg_price, level=0.95)

        # h)
        par(mfrow=c(2,2))
        plot(mulreg_price)
        plot(rstudent(mulreg_price))
        # No clear outliers; std. res. fall within +-3; no pattern either. 
       
        # Compute the average leverage statistic: # $\overline{h} = \frac{p+1}{n}$.
        (2+1)/nrow(Carseats)
        # We can see that there are a few leverage points which are significantly 
        # higher than the average leverage.
           
        # ** 
        # Collinearity check: corr matrix, VIF 
        # cor(Carseats[, -c(7, 10, 11)], method = c("pearson"))
        library(car)
        vif(mulreg_price)
        #**
        
        
        
  ########            
  # Ex. 11 - Investigate the t-statistic in SLR without an intercept
  ########    
        # Simulate some data
        set.seed(1)
        x=rnorm(100)
        hist(x)
        y=2*x+rnorm(100)
        hist(y)
        plot(x,y)
        
        
        
        # a) x->y
        lm.fit = lm(y ~ x + 0)
        summary(lm.fit)
        
        # b) y->x
        lm.fit = lm(x ~ y + 0)
        summary(lm.fit)
        # c) Given the truth would be x = 1/2 of y, the est. coefficient of .39 is further off
        # Though interestingly the t-value and hence corresponding p-value and R^2 statistics are identical 
        # This makes conceptual sense given that it should represent how much variance in the output the predictor 
        # can account for, which, given there is only one predictor, is logically invertible.
        
       
        #d) -not required.
       
        #e)
        # Simply put, it's trivial to see that by setting y=x, and x=y, that the resulting t equation is the same.
        
        #f)
        lm.fit = lm(y~x)
        coef(summary(lm.fit))[, "t value"]
       
        lm.fit = lm(x~y)
        coef(summary(lm.fit))[, "t value"]
        
      
  ########            
  # Ex. 14 
  ######## 
      # A theoretical exercise to review the fundamental concept of collinearity
      # a) Simulating the data
          set.seed(123)
          x1 = runif(100)
          x2 = 0.5*x1 + rnorm(100)/10
          y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
          # Function: Y = 2 + 2X_1 + 0.3X_2 + epsilon
          # Coefficients: 
          #     - beta_0 or intercept = 2
          #     - beta_1 = 2 
          #     - beta_2 = 0.3
          # Note that the data is simulated so that we know the true betas (coefficients) that link y with x1 and x2. 
        
    
      # b)
          plot(x1, x2) 
          # We observe the linear relationship between x1 and x2 from the scatterplot.
          # We can also check their correlation
          cor(x1, x2)
          # Concl.: 0.82 is very high, so adding these two variables in the regression will lead to multicolineaerity problems
  
      # c)
          lm.fit = lm(y ~ x1 + x2)
          summary(lm.fit)
          # Note, the estimates for beta_0, beta_1 and beta_2 are different from the true values. Why? 
          # We can't reject the null hypothesis for beta_2, meaning that x2 is not a relevant predictor of y.
          # We can reject it for beta_1 at a significance level of 5%, meaning that x1 is relevant.
          # But are these effects trustful, given the high level of collinearity between predictors? 
  
      # d)
        lm.fit = lm(y ~ x1)
        summary(lm.fit)
        # We can comfortably reject the null hypothesis for beta_1.
        
      # e)
        lm.fit = lm(y ~ x2)
        summary(lm.fit)
        # Again, we can reject the null hypothesis for beta_1 in this scenario.
  
        
      # f) Do the results obtained in multiple LR and simples LR contradict each other? 
        # Let´s discuss. You may add to the below answer: 
        # They are definitly different, but the result is expected. 
        # A correlation means, that there's some direct association between two predictors. 
        # In our case this correlation is 0.80!
        # When a correlation between predictors exists, in reality the true form will simplify 
        # to using only one of the predictors. 
        # So we would expect fitting a linear regression with only one of the two predictors to 
        # both result in statistically significant coefficients. 
        # But when we fit the model with both predictors, one will not be significant, in essence 
        # because the other already is able to capture the information 
        # it contributes to the model.
        
 
      # g) Introducing an outlier in the data and see the effects
        x1 = c(x1, 0.1)
        x1 
        x2 = c(x2, 0.8)
        x2
        y = c(y, 6)
        y
        plot (x2, y)
        
        lm.fit1 = lm(y ~ x1 + x2)
        summary(lm.fit1)
        plot(lm.fit1, which=5) # plot no.5 displays leverage
        # First we see that now X_2 has become a significant predictor 
        # So, one single extreme observation has or can change the whole story!
        # We also see obs. 101 has high leverage.
        
        lm.fit2 = lm(y ~ x1)
        summary(lm.fit2)
        plot(lm.fit2, which=5)
        # Also in this model 
        
        lm.fit3 = lm(y ~ x2)
        summary(lm.fit3)
        plot(lm.fit3, which=5)
        # in the third model, the new point has also high leverage.
        
     # To conclude, to avoid wrong conclusions, one needs to delete the obs. with extreme leverage when 
     # analysing the data
      
       
  ########            
  # Ex. 15 
  ########  
      
     # Objective: What predicts and/or explains per capita crime rate in Boston? 
      library(MASS)
      View(Boston)
      ?Boston 
      str(Boston) 
      # chas is treated as integer, but it should be a factor 
      # chas is Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
      # this transformation is particularly important when we have a qualitative predictor 
      # with 3 or more levels. The software need to know this is not a metric variable. 
       Boston$chas = as.factor(Boston$chas)
      # the remaining predictors are quantitative
      
    
      # Check for missings 
        table(is.na(Boston)) 
        summary(Boston) 
        # or graphically
        library(DataExplorer) 
        plot_missing(Boston)
        # we have no misssing values in Boston dataset
                
                    ###################################
                    # In case there are missing values:
                    ###################################
        
                    # 1st Option: Create a new dataset without missing values using na.omit() 
                    data.omitted <- na.omit (data)
                    # or
                    filter(data, !complete.cases(data))
                    # NOTE: 
                    # If missing data has a particular label/code in the original file 
                    # (such as, unknown, 999, etc.), we can recode that label class 
                    # to missing value using:
                        # data[data =="unknown"] <- NA 
                        # data[data == 999] <- NA 
                    
                    
                    
                    # 2nd Option: Impute specific values (if the number of missings is not 
                    # significantly high (< 5% of the data))
                    # In Hmisc package, the default values for imputation are: 
                    # a) MEDIAN for numeric variables 
                    # b) MODE (most frequent state) for factors
                    # install Hmisc package 
                    library (Hmisc) 
                    data.impute <- data 
                    data.impute$A <- impute (data.impute$A) # impute values to variable A 
                    # We can also impute explicitly a random number or a constant
                    # impute (data.impute$A, "random")
                    # impute (data.impute$A, 20.1)
                    
                    
                    # 3rd Option: Impute the missing values using a trained model 
                    # This option can be more accurate than the previous options
                    # In ML2, we have an application using Random Forest for inputation
                    
                    # 4th Option: Assign a label to missing values and treat the missings 
                    # as informative values in the model
                    # data = as.matrix(data)
                    # data[is.na(data)] <-"NoAnswer"
                    # data = as.data.frame(data)
                    
                    
      
      # Data visualization and summarization
      # Standard plots, ggplot2 library, DataExplorer library, GGally library 
      # for data vizualization
      
          # Univariate plots
          # standard plot
          boxplot(Boston$crim, ylab = "Crime per capita") 
           # or 
          library(ggplot2)
          ggplot(Boston, mapping = aes (x=factor(0), y=crim)) + geom_boxplot() + xlab("") + ylab("Crime per capita")
          # or
          library (DataExplorer)
          plot_density(Boston)
          plot_histogram(Boston)
          # look at this library for many more useful functions
          
          
          # Bivariate maps (scatterplots)
          # standard
          pairs(Boston[, 1:10])
          # or
          library (GGally) 
          ggpairs(Boston, columns = 1:5) # a selection 
          
          # Concl: We notice that some variables are not linearly (straight line) 
          # related with crime per capita (crim, is our DV). 
          # Shall we try some transformations of the predictors? 
          # Advantages: for prediction purposes, we may get better fit
          # The main disadvantage of transformations is that it makes it more 
          # difficult to interpret the beta coefficients.
  
          
      
     # (a) Requirement: Perform a SLR for each predictor
          # We may do it one by one; but here we´ll use a loop instead:
          
          pvals = c() # define an array to extract the p-value from summary
          for (predictor in Boston[-1]) {
            lm.fit = lm(crim ~ predictor, data=Boston)
            p = summary(lm.fit)$coefficients[2,4]
            pvals = c(pvals, p)
          }
          names(Boston[-1])[which(pvals<0.05)]
        # Concl: the predictors displayed have an individual statistically significant 
        #        association with the response. 
        #       "chas" is not displayed in the list, thus it is not sig at 5%. 
        #        Based on the plots and correlation matrix in the data summarization, 
        #        these results are somehow expected.

      
          
     # (b) MLR for all predictors
          lm.fit = lm(crim ~ ., data=Boston)
          summary(lm.fit)
       
          # Check F-tests and the beta coef. 
          # Recall, in Statistics, the null hypothesis is "no relation exists";  that is beta = 0. 
          # Where can we reject the null hypothesis H0? 
          # Write here a short conclusion of the results. 
          
          
          # ***
          # NOTE:
          # Many researchers would consider the cut-off of 5% as "the Law of God", by saying:  
          # "we can only reject the null hypothesis for zn, dis, rad, black, medv."
          # But, it is recomendable to be skeptical, because this 5% limit is selected a bit random 
          # by researchers. Is it sufficiently small? 
          # Therefore, always say "at 5% level we cannot reject ... something" and acknowledge 
          # and ther might be other relationships significant at 10 % (for example),
          # that may deserve attention. That is, 5% is just a probability, and between 5% and 5.5% 
          # there is not much difference, right? 
          # We cannot be absolutly sure that "nox" variable, whose p-value is 0.051, 
          # is not significant in reality, can we? 
          # Therefore, be careful when concluding particularly when the level of significance 
          # is so close to the cut-off point. 
          # In medicine and bio-statistics the researchers only reject H0 when the p-values are 
          # below 1% ! (not 5%).  
          # This is a long story and I recommend you to read more about it in your free time. 
          # All I can say is that you should not have a blind trust in the p-value just because 
          # it is below a certain cut-off.
          # ***
          
          
          
     # (c) Compare the coefficients from SLR and MLR. Are they the same? 
          # If the predictors are correlated, they will change, 
          # because MLR takes into consideration all the effects at once.
          
          slm_coeffs = c() # define slr vector of coeficients 
          for (predictor in Boston[-1]) {
            lm.fit = lm(crim ~ predictor, data=Boston)
            c = coef(lm.fit)[2]
            slm_coeffs = c(slm_coeffs, c)
          }
          
          lm.fit = lm(crim ~ ., data=Boston) 
          
          # plot the coef minus de intercept
          plot(slm_coeffs, coef(lm.fit)[-1], xlab="Simple Regression Coef.", ylab="Multiple Regression Coef.") 
         
          library(calibrate)
          textxy(slm_coeffs, coef(lm.fit)[-1], names(Boston[-1])) # add the labels
          # Concl. Here you will see the (bad) effect of collinearity, 
          # particularly for nox variable.
          # Before setting up a regression, particularly for inference reasons, 
          # take a logical look at what each predictor represents and whether 
          # it overrlaps with other predictor.
    
          
           # (d) Fit a cubic model (poly degree 3)
            # analyze visually the scatterplots crim vs. each predictor; 
            # notice some relationship tend to be nonlinear
            # the exercise requires to set a cubic model for each predictor
            # below we exclude the DV and the qualitative predictor chas
            lapply(Boston[-c(1, 4)], function(predictor) {
            fit = lm(crim ~ poly(predictor, 3), data=Boston)
            summary(fit)
             })
            # For which predictor we require a cubic model to predict crime?
            # Write briefly the conclusion here and discuss whether it make sense in practice. 

            
            
     
            
            
    
      