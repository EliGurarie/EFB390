# R is a calculator

1+2

3^6
sqrt((20-19)^2 + (19-19)^2 + (19-18)^2)/2
12345*54312

# Assigning variable names
X <- 5      # sets X equal to 5 
X
x # doesn't work not capitalized

X*2
X^X

Fred <- 5
Nancy <- Fred*2
Fred + Nancy

# Vectors
X <- c(3,4,5)   # sets X equal to the vector (3,4,5)
X
?c
?boxplot
X + 1
X*2
X^2
((X+X^2/2)/X)^2

# Simple estimates of abundance
#counts <- c(150,125,105,110,140)
counts <- c(97, 103, 68)

k <- length(counts)
sum(counts)/k

mean(counts)

counts - mean(counts)
sum((counts - mean(counts))^2)
sqrt(sum((counts - mean(counts))^2) / (k-1))

sd(counts)

mean(counts) + c(-2,2)*sd(counts)

# Multiple vectors and data frames
Y <- c(1,2,3)
data.frame(X,Y)

mydata <- data.frame(X,Y)
ncol(mydata) # ncol() gives us a number of columns that this 
names(mydata) # names() lists all column names that this data
mydata$X
mydata$Y

# Part 2 

SeaLions <- read.csv("SeaLions.csv")
is(SeaLions) # tells what type of files we have
names(SeaLions) # tells us the names of all the columns

head(SeaLions) # shows the first several rows of the dataframe

Length <- SeaLions$Length
Weight <-SeaLions$Weight
Island <- SeaLions$Island
Sex <- SeaLions$Sex

# Summary Statistics
range(Length)
median(Length)
mean(Length)
var(Length)
sd(Length)

hist(Length)

max(Weight)
min(Weight)
mean(Weight)

boxplot(Weight ~ Sex)
