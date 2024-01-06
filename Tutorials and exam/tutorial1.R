dataset = read.csv('WestRoxbury - Copy.csv')

# 1: Explore the following functions or commands in your dataset
dim(dataset)
head(dataset)
View(dataset)

# 2: Show the first 10 rows of the first column only
dataset[1:10, 1:1]

# 3. Show the first 10 rows of each of the columns
dataset[1:10, ]

#4. Show the fifth row of the first 10 columns
dataset[5, 1:10]

#5 Show the whole first column
dataset[, 1]

#6 Find the mean of the first column
mean_1col = mean(dataset[, 1])
print(mean_1col)

#7 Find summary statistics for each column
sum_stats = summary(dataset)

#8  Check the type of variables using the command str(). Which of the variables are categorical variables?
str(dataset)

#9 Convert the categorical variables to factors and identify the levels of the factor variables using the commands
dataset$REMODEL = factor(dataset$REMODEL,
                         levels = c('None', 'Recent', 'Old'),
                         labels = c(0, 1, 2))

#3.1 Define v1:
v1 = c(1, 2, 2, 1)
v2 = c(2, 3, 3, 2)

v1+v2
v1-v2
v1*v2

v3 = paste(v1, v2)
print(v3)

#3.2a define a vector of your choice
mA = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)

#3.2b compute the sum of each column
first = sum(mA[,1])
second = sum(mA[,2])
third = sum(mA[,3])

vector = c(first, second, third)

sorted_matrix <- mA[c(mA[, 1]), ]

#3: Consider the function y = f(x) defined by:
iX <- sample(-10:10, 1)

if (iX <= 0) {
  print("f(x) = -x^3")
} else if (iX > 0 & iX <= 1) {
  print("f(x) = sqrt(x)")
} else {
  print("f(x) = x^2")
}


ff <- function(x, n) {
  h=0
for (i in 0:n) {
  h = h +x^i
}
  return(h)
}


#3.5 Write a function that achieves the same result as in 4 but using a while loop
f2 <- function(x, n){
  h=0
  i=0
  while(i<=n){
    h = h + x^i
    i = i+1
  }
  return(h)
}

#3.6 Give R expressions that return the following matrices mB and mC without explicitly writingit element by element:

x <- c(0, 1, 1, 1)
y <- c(1, 0, 1, 1)
z <- c(1, 1, 0, 1)
v <- c(1, 1, 1, 0)

# creating matrix
n <- rbind(x, y, z, v)

# print matrix
print(n)


n <- 4

# Create matrix mC
mC <- matrix(0, n, n)
mC[1, 2:4] <- c(2, 3, 0)
mC[2, 2] <- 6
mC[3, 1] <- 9
mC[4, 4] <- 16

# Print the matrices
print(mC)

#3.7 THINK!
my_matrix <- matrix(0, nrow = 1, ncol = 100)
for (i in 1:100) {
  for (j in seq(i, 100, i)) {
    my_matrix[j] <- 1 - my_matrix[j]
  }
} 

on_switches <- which(my_matrix == 1)

cat("Switches turned on at the end:", on_switches, "\n")


#########################################################################

vector_of_zeros <- rep(0, 100)

for (i in 1:100) {
  for (j in seq(i, 100, i)) {
    vector_of_zeros[j] <- 1 - vector_of_zeros[j]
  }
} 

on_switches <- which(vector_of_zeros == 1)

cat("Switches turned on at the end:", on_switches, "\n")


