######################################################
## Classification: LDA, QDA                   
## Course: ML 1 for BI                
## Lecturer: Ana Alina Tudoran
## Oct 2023
######################################################

#____________________________________________________________________________
# LDA with DiscriMiner library - THIS LIBRARY HAS BEEN REMOVED FROM R IN 2023
#____________________________________________________________________________
library(DiscriMiner)
mowers.df <- read.csv("~/Cloud/Documents/Alina Tudoran/TEACHING/Postgraduate/Machine Learning 2020-2021/ML1/2023/New lectures/4. Ch4_Classification/CASE STUDIES/LDA_Database Riding mowers /Ridingmowers.csv")
da.reg <- linDA(mowers.df[,1:2],mowers.df[,3])
da.reg
da.reg$functions 
da.reg$classification
da.reg$scores 
# calculate probabilities 
propensity.owner <- exp(da.reg$scores[,2])/(exp(da.reg$scores[,1])+exp(da.reg$scores[,2])) 
propensity.owner
data.frame(Actual=mowers.df$Ownership,
           da.reg$classification, 
           da.reg$scores,  
           propensity.owner=propensity.owner)


#___________________________________________________________________________
# LDA with library MASS - like in the ISL book
#___________________________________________________________________________
library(MASS)
da.reg_beta <- lda(mowers.df$Ownership ~ ., data = mowers.df)
da.reg_beta # coefficients of linear discriminant; 
# they reflect the discriminatory power of each variable; 
# in this case, Lot_size is the most important in discriminating between classes.
# they are also used to create the LDA decision rule
# predictions
predict(da.reg_beta, mowers.df) # we get directly probabilities 
names(predict(da.reg_beta, mowers.df))
predict(da.reg_beta, mowers.df)$posterior #probabilities
predict(da.reg_beta, mowers.df)$x # linear discriminant scores
# NOTE: using the linear discriminant scores for each of the training observation
# is the second approach to classifying observations
# these values are compared against an optimum cutting score (average for each group)
# if the score is lower than the cutting score then we classify in one group
# if it is higher then it is classified in the other group
par(mar = rep(2, 4))
plot(da.reg_beta) # the plot of the linear discriminants
