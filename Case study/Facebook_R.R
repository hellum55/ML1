 # *******************************************
 #       Advertising on Facebook Source        
 #             Data source: Kaggle.com             
 #         Description in the .doc file 
 #          Ana Alina Tudoran, 2023
 # *******************************************
  #install.packages('tidyverse')
  library(visdat)   # for visualizations
  library(tidyverse) # for data manipulation (meta-library including ggplot2, dplyr, tidyr, readr,...) 
  library(caret)    # for various ML tasks
  library(recipes)  # for feature engineering tasks
  library(forecast) # for data exploration
  library(DataExplorer) # for data exploration
  library(visdat) # for data exploration

  
  # Uploading external data in R (KAG_conversion_data.csv file) 
  KAG_conversion_data <- read.csv("KAG_conversion_data.csv")
  View(KAG_conversion_data)
  dim(KAG_conversion_data)
  glimpse(KAG_conversion_data)
  
  
  # Read the case study to get an idea about the variables and their meaning 
  
  
  # Create a copy and abbreviate the variables names with rename() 
  dataTf <- KAG_conversion_data 
  dataTf <- dataTf %>% 
            rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
            conv = Total_Conversion, appConv = Approved_Conversion) 
  
  
  #____________________________________________________________________________
  # ROAS Return on Advertising Spend 
  #____________________________________________________________________________
  # ROAS is the revenue as a percentage of advertising spent
  # ROAS (DV) is not present in the dataset
  # Calculate ROAS as it follows
  
      # Click-through-rate (CTR): % of how many of the impressions became clicks
      # CTR = Clicks/Impressions
  
      # Cost Per Click (CPC): how much on average each click costs 
      # CPC = Spent/Clicks
  
      # Total conversion: total conv = conv + appConv
  
      # Total conversion value: 
      # conval = 5 *  conv; 
      # appConVal = 100 * appConv; 
      # totConVal = conval + appConVal 
  
      # Cost per conversion: 
      # costPerCon = Spent/totConv
      
      # Return on Advertising Spend (ROAS): 
      # ROAS = totConVal/Spent
  
      
      # dplyr::mutate() adds new variables and preserves existing; 
      # dplyr::transmute() drops all the existing variables
  
        dataTf <- dataTf %>%
                      mutate(CTR = ((Clicks/impr) * 100), CPC = Spent/Clicks) 
        dataTf$CTR <- round(dataTf$CTR, 4) # round decimals
        dataTf$CPC <- round(dataTf$CPC, 2)

        dataTf <- dataTf %>%
                      mutate(totConv = conv + appConv,
                             conVal = conv * 5,
                             appConVal = appConv * 100) %>%
                      mutate(totConVal = conVal + appConVal) %>%
                      mutate(costPerCon = round(Spent / totConv, 2),
                              ROAS = round(totConVal / Spent, 2))
                      
      # Cost Per Mile (CPM) = Spent/Impr *1000. 
      # This number is the cost of one thousand impressions. 
      # If the  objective is ad exposure to increase brand awareness, 
        # this might be important to target, too.
        dataTf <- dataTf %>% 
                        mutate(CPM = round((Spent / impr) * 1000, 2))
  
        
   # Because some division on 0 => inf values => that will be replaced by NA
        dataTf[dataTf == "Inf"] <- NA
        dataTf[dataTf == "NaN"] <- NA        
        dataTf[dataTf == "-Inf"] <- NA  
        
        
  # chr as factors
        dataTf$xyzCampId <- as.factor(dataTf$xyzCampId)
        dataTf$interest <- as.factor(dataTf$interest)
        dataTf$age <- as.factor(dataTf$age)
        dataTf$gender<- as.factor(dataTf$gender)
        str(dataTf)
  
  # The qualitative predictor "interest" has too many categories to handle and 
  # too few obs. per category; we may consider lumping it with step_other() 
        count(dataTf, interest) %>% arrange(n)
  
  
  # We exclude:
        # - ad_id and fb_campaign_id (1 & 3 - as they are not relevant) 
        # - the variables that contribute to calculating ROAS (Clicks, Spent,conv
        # appConv, totConv, conVal, AppConVal, costPerCon - 8:11 and 14:18 - as
        # they have a deterministic relation with ROAS)
        # - CPM (20 - it is another target)
        
        dataTf_new <- dataTf[, -c(1,3,8:11,14:18,20)]
        str(dataTf_new)
        # check the remaining variables
  
  #__________________      
  # Partitioning data
  #__________________
    set.seed(123) 
    smp_size <- floor(0.75 * nrow(dataTf_new))
    train_ind <- sample(seq_len(nrow(dataTf_new)), size = smp_size)
    traind <- dataTf_new[train_ind, ]
    testd <- dataTf_new[-train_ind, ]
  
  # ________________  
  # Data exploration
  # ________________  
  # Target ROAS distribution
  par(mfrow=c(1,3))
  hist(traind$ROAS, breaks = 20, col = "red", border = "red", ylim = c(0, 800))
  log_ROAS <- log(traind$ROAS)
  BC_ROAS <- forecast::BoxCox(traind$ROAS, lambda="auto") 
  hist(log_ROAS, breaks = 20, col = "lightgreen", border = "lightgreen",ylim = c(0, 800) )
  hist(BC_ROAS, breaks = 20, col = "lightblue", border = "lightblue", ylim = c(0, 800))
  
  # log transformation seems to work better for this data
  # but attention to log transformer when ROAS=0
  which(traind[,8] == 0)
  # [1] 211 324 332 539
  which(testd[,8] == 0)
  # [1] 166
  # We exclude those records (rows)
  traind <- traind[-c(211, 324, 332, 539),]
  testd <- testd[-c(166),]
  
  
  # Missing 
  sum(is.na(traind))
  plot_missing(traind)
  # Below we impute this missing values. An alternative is to delete them and 
  # see the effect on model performance. 
      
       
  # Check of feature with nzv filtering 
  caret::nearZeroVar(traind, saveMetrics = TRUE) %>% 
                tibble::rownames_to_column() %>% 
                filter(nzv)
  
# ____________________  
# Create the blueprint
#_____________________
  set.seed(123)
  facebook_recipe <- recipe(ROAS ~ ., data = traind) %>%
      step_impute_knn(all_predictors(), neighbors = 6) %>%
      step_impute_knn(all_outcomes(), neighbors = 6) %>%
      step_log(all_outcomes()) %>%
      step_other(interest, threshold = 0.01, other = "other") %>%
      step_nzv(all_nominal()) %>%
      step_center(all_numeric(), -all_outcomes()) %>%
      step_scale(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)
  facebook_recipe

  # prepare
  prepare <- prep(facebook_recipe, training = traind)
  prepare
  # bake
  baked_train <- bake(prepare, new_data = traind)
  baked_test <- bake(prepare, new_data = testd) 
  dim(baked_train)
  dim(baked_test)
  # evaluate the size of the new datasets
  
  
#___________
# Modeling
#___________

  # model 1
    model1 <- train(
      ROAS ~ ., 
      data = baked_train, 
      method = "lm",
      trControl = trainControl(method = "cv", number = 10)
    )
    model1
    summary(model1)
    
  # model 2   
    model2 <- train(
      ROAS ~ ., 
      data = baked_train, 
      method = "lmStepAIC", direction="both", # lmStepAIC is a stepwise regression discussed later in the course
      trControl = trainControl(method = "cv", number = 10)
    )
    summary(model2)
    
   
  # model PCR   
    set.seed(123)
    cv_model_pcr <- train(
      ROAS ~ ., 
      data = baked_train, 
      method = "pcr",
      trControl = trainControl(method = "cv", number = 10),
      tuneLength = 38 
    )
    cv_model_pcr$bestTune
    cv_model_pcr$results %>%
      dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))
    ggplot(cv_model_pcr)  
    
    
  # model PLS
    set.seed(123)
    cv_model_pls <- train(
      ROAS ~ ., 
      data = baked_train, 
      method = "pls",
      trControl = trainControl(method = "cv", number = 10),
      tuneLength = 38
    )
    cv_model_pls$bestTune
    cv_model_pls$results %>%
      dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
    ggplot(cv_model_pls) 
    
             
    
    #  Summarizing the results
    results <- resamples(list(reg1 = model1,
                              reg2 = model2,
                              pcr = cv_model_pcr,
                              pls = cv_model_pls))
    summary(results)
    bwplot(results)
    dotplot(results)
 
       
# _______________________________________   
# Estimate the test RMSE for all models
#________________________________________    
      
    # model reg1
      predictions <- predict(model1, baked_test) 
      predictions 
      RMSE1 = sqrt(sum((baked_test$ROAS - predictions)^2/nrow(baked_test)))
      RMSE1
      
      # model reg2
      predictions <- predict(model2, baked_test) 
      predictions 
      RMSE2 = sqrt(sum((baked_test$ROAS - predictions)^2/nrow(baked_test)))
      RMSE2
                
      # model pcr
      predictions_pcr <- predict(cv_model_pcr, baked_test) 
      RMSE3 = sqrt(sum((baked_test$ROAS - predictions_pcr)^2/nrow(baked_test)))
      RMSE3
       
      # model pls
      predictions_pls <- predict(cv_model_pls, baked_test) 
      RMSE4 = sqrt(sum((baked_test$ROAS - predictions_pls)^2/286))
      RMSE4
             
      # Summary     
      results_newtest <- list(RMSE1,RMSE2, RMSE3, RMSE4)                       
      names(results_newtest) <- c("Test_er_reg1", "Test_er_reg2", "Test_er_PCR", "Test_er_PLS")
      results_newtest
# conclude.

      
# _____________________    
# Variable importance 
# PS: vip() does not work for model pcr and reg2 (stepwise)
# _____________________
    par(mfrow=c(1,2))  
    vip::vip(model1, method = "model") 
    # conclude.


pip install -U radian
remotes::install_github("ManuelHentschel/vscDebugger")
install.packages("httpgd")
