#Code samples for R-Lectures

###Installing and loading packages:
install.packages("ggplot2")
library("ggplot2")
a<-150  

###Working directory
#get and setup
getwd()
dir <- "G:/foo" #this path needs to exist
setwd(dir)
getwd()


#source
source("foo.r") #tell R to Source/Run the .R file/script, file needs to exist!


###Objects and Manipulations
#Variable assignment

iX = 100

ifoo <- 2020L
class(ifoo)
dfoo <- 2020
class(dfoo)
cfoo <- "Machine Learning"
class(cfoo)
bfoo <- TRUE
class(bfoo)

##Vectors, Matrices, and Arrays
#Vectors
vX
vY <- c(cfoo,"Business Intelligence")
vY
length(vX)
rep(vX,3)

#Matrices and Arrays
mX <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
mX
dim(mX)

aX <- array(1:18, dim = c(3,3,2))
aX
dim(aX)

cbind(mX,mX)
rbind(mX,mX)

#Indexing
vX <- c(1,2,3,4)
mX <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)

vX[1]
vX[1:2]
vX[c(1,4)]

mX[1,2]
mX[1,]
mX[1,][2]

#Arithmetic Manipulations
iX <- 100L
(1 + 1/iX)^iX
iX <- 2*iX - 2
iX

vX <- c(1,2,3)
vY <- c(4,5,6)

vX*vY
vX+vY
vY^vX

c(1,2,3) + c(1,2)
(1:10)^c(1,2)

2 + c(1,2,3)
2 * c(1,2,3)

#Character Manipulations
sText <- "Machine Learning for BI"
nchar(sText)
substr(sText, start = 9, stop = nchar(sText))

vStrings <- strsplit(sText, split = " ")
vStrings

paste("Machine","Learning")

#Logical Operators and Expressions
vX <- c(1,2,3,4)
vX == 2
vX != 2

vX <- c(1,3,4,18)
vX > 2

vX[vX > 2 & vX < 10]
subset(vX,subset = vX > 2)

###Loops
#If statements
dX <- sample(-10:10, 1) #one random integer between -10 and 10
if (dX > 0){
  print("x is positive")
  } else {
    print("x is non-positive")
}

#think about this loop. Try a value smaller than -1. 
if (dX < -1){
  print("x is less than -1")
  } else if (dX < 5) {
  print("x is less than 5")
  } else {
    print("x is greater than or equal to 5")
}

#for loops
for (iX in 1:50){
  print(iX)
}   # iX is a dummy variable here and can be replaced by another variable, say b or k. 

vX <- seq(1,9,by=2)
vX
dSum_x <- 0
for (iX in vX) {
  dSum_x <- dSum_x + iX
  cat("The current loop element is", iX, "\n")
  cat("The cumulative total is", dSum_x, "\n")
}

sum(vX)
cumsum(vX)

#while loops
iN <- 1
while (iN <= 6){
  print(iN^2)
  iN <- iN + 1
}


###Custom Functions
f_test <- function(x){
  return(x^2)
}

f_test(5)

f_test2 <- function(x=1, myoption=c("pos","neg"))
  if (myoption == "pos"){
    return(x^2)
  } else {
    return(-x^2)
  }

f_test2(3, myoption="neg")
f_test2(myoption="pos")


#Dataframes
mData <- data.frame(Plot = c(1,2,2,5,8,8),
                    Tree = c(2,2,3,3,3,2),
                    Species = c("DF","WL","GF","WC","WC","GF"),
                    Diameter = c(39,48,52,35,37,30),
                    Height = c(20.5, 33.0, 30.0, 20.7, 22.5, 20.1) )

mData
mData$Diameter
mData[["Diameter"]]
mData[[4]]
mData[,4]
mData[,"Diameter"]

mData$newdata <- c(1,2,3,4,5,6)
mData$newdata2 <- TRUE
mData$newdata3 <- c("one","two")

mData

#Lists
my.list <- list(first = "one", second = TRUE, third = 3, fourth = c("f","o","u","r"))
my.list[[1]]
mode(my.list[[1]])
mode(my.list[1])

names(my.list)
my.list$second

my.list <- list("one", TRUE, 3 , c("f","o","u","r"))
names(my.list) <- c("first","second","third","fourth")

x <- list(1, c(2,3), c(4,5,6))
unlist(x)










