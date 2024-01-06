####################################
# File: TA Problem Set 1 Guiding Solution
# Course: Machine Learning for BI 1 
# TA: Camille Pedersen
# Aarhus University, Fall 2023
####################################

### 1. IMPORTING DATA INTO R
# 1.1 
setwd("~/Documents/Phd TA og undervisning/Machine Learning for BI/2023/Tutorial 1")

# 1.2
# save WestRoxbury.csv file in the same folder as your working directory
housing.df <- read.csv("~/Documents/Phd TA og undervisning/Machine Learning for BI/2023/Tutorial 1/WestRoxbury.csv", header=TRUE, na.strings = c("  ","*"))

### 2. BASIC DATA EXPLORATION
# 2.1
# a
dim(housing.df)  # find the dimension of data frame

# b
head(housing.df)  # show the first six rows

# c
View(housing.df)  # show all the data in a new tab

# 2.2
housing.df[1:10, 1]  # show the first 10 rows of the first column only

# 2.3
housing.df[1:10, ]  # show the first 10 rows of each of the columns 

# 2.4
housing.df[5, 1:10]  # show the fifth row of the first 10 columns

# 2.5
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column

# 2.6
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column

# 2.7
summary(housing.df)  # find summary statistics for each column     

# 2.8
str(housing.df)
# Notice that REMODEL is the only factor variable.

# 2.9
housing.df$REMODEL= as.factor(housing.df$REMODEL)
levels(housing.df$REMODEL)

# 2.10
# You can export the data from R into a variety of forms: 
# a tab-delimited file using write.table(), as .csv file using write.csv (), e.g.,
write.csv(housing.df, "housing.df_new.csv")


### 3. PROGRAMMING

### 3.1

v1 <- c(1, 2, 2, 1)
v2 <- c(2, 3, 3, 2)

v1 + v2 # addition
v1 - v2 # subtraction 
v1 * v2 # multiplication
v3 <- c(v1, v2) # concatenation

### 3.2
mA <- matrix(c(9,8,7,6,5,4), nrow = 2, ncol = 3)
mA

# a
min(mA[1,])
min(mA[2,])

max(mA[1,])
max(mA[2,])

# b
sum_col_1 <- sum(mA[,1])
sum_col_2 <- sum(mA[,2])
sum_col_3 <- sum(mA[,3])

c(sum_col_1, sum_col_2, sum_col_3)

#Alternative:
colSums(mA)

# c
mA[order(mA[,1]),1]

#Alternative:
sort(mA[,1])


### 3.3
f_x <- function(x){
  if (x <= 0) {
    y=-x^3
  } else if (x>0 & x<=1) {
    y=x^2
  } else if (x>1) {
    y=sqrt(x)
  }
  return(y)
}

# e.g
f_x(-2)
f_x(0.5)
f_x(4)


### 3.4

# for loop
h_xn <- function(x,n){
  h=0
  for (i in 0:n){
    h = h + x^i
  }
  return(h)
}

#e.g
h_xn(2,3) 


### 3.5

# while loop
h_xn2 <- function(x,n){
  h=0
  i=0
  while (i <= n){
    h = h + x^i
    i = i+1
  }
  return(h)
}

#e.g
h_xn2(2,3) 


# Extra: what if it has to be done without loops?
# vector-operations (no loops)
h_xn3 <- function(x,n){
  vX <- c(rep(x,n+1))
  vi <- c(seq(0,n))
  h = vX^vi
  return(sum(h))
}

#e.g
h_xn3(2,3) 


### 3.6
mB <- 1-diag(1,4,4) # diagonal matrix. 4x4 matrix, element wise subtraction
mB

index <- c(2,3,6,9,16)
mC <- matrix(1:16,4,4)
mC <- t(mC)
mC [!(mC %in% index)] <- 0 # the %in% operator is often convenient
mC


### 3.7

## Solution example 1:
iN <- 100
vToggle <- rep(0,iN)

for(i in 1:iN){
  vToggle[seq(i,iN,i)] = vToggle[seq(i,iN,i)]+1 
}

which(vToggle %% 2 != 0) # check which toggles are on (odd number)

# In line 181 every ith element is selected and 1 is added (switched)
# Toggles being switched an even number of times will at the end be off
# Toggles being switched an odd number of times will at the end be on


## Solution example 2:
vToggle <- rep(0, iN)

for (i in 1:iN) {
  
  vSelect = seq(i, iN, i) # toggles to switch
  #vSelect = seq(0, iN, i)[-1] # alternative to "toggles to switch"
  
  vToggle_off <- vToggle[vSelect] == 0 #toggles to switch that are off
  
  vToggle[vSelect][vToggle_off]   <- 1 #toggles being switched on
  vToggle[vSelect][!vToggle_off]  <- 0 #toggles being switched off
  
}

which(vToggle == 1)

# In the lines 201-202 note that we "double" subset vToggle.
# First by the sequence in vSelect and from that i.e. "vToggle[vSelect]"
# Then we can subset the vector subset further by [][].
# Try to print the result. Otherwise try to uncomment the command below
# and figure out what it does.
# c(1:10)[2:6][3:4][2]






