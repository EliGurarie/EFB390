
# Part 1: Basics
## Calculator
1+2
3^6
sqrt((20-19)^2 + (19-19)^2 + (19-18)^2)/2
12345*54312

## Assign variable names
X <- 5      # sets X equal to 5 
X
X*2

Fred <- 5
Nancy <- Fred*2
Fred + Nancy

## Vectors
X <- c(3,4,5)   # sets X equal to the vector (3,4,5)
X

X + 1
X*2
X^2
((X+X^2/2)/X)^2

## Abundance estimates w SE

counts <- c(150,125,105,110,140)
k <- length(counts)
sum(counts)/k

mean(counts)

counts - mean(counts)
sum((counts - mean(counts))^2)
sqrt(sum((counts - mean(counts))^2) / (k-1))

sd(counts)
mean(counts) + c(-2,2)*sd(counts)

## Exercise 1
counts <- c(49, 50, 50)
k <- length(counts)
sum(counts)/k

mean(counts)

counts - mean(counts)
sum((counts - mean(counts))^2)
sqrt(sum((counts - mean(counts))^2) / (k-1))

sd(counts)
mean(counts) + c(-2,2)*sd(counts)

## Multiple vectors and data frames
Y <- c(1,2,3)
data.frame(X,Y)

mydata <- data.frame(X,Y)

ncol(mydata) # ncol() gives us a number of columns that this data frame has
names(mydata) # names() lists all column names that this data frame has 

mydata$X
mydata$Y

# Part 2: Loading and exploring data

## Loading data
getwd()
SeaLions <- read.csv("SeaLions.csv")

is(SeaLions) # tells what type of files we have
names(SeaLions) # tells us the names of all the columns

head(SeaLions) # shows the first several rows of the dataframe

Length <- SeaLions$Length
Weight <-SeaLions$Weight
Island <- SeaLions$Island
Sex <- SeaLions$Sex

## Summary stats
range(Length) # range
median(Length) # median
mean(Length) # mean
var(Length) # variance
sd(Length) # standard deviation

## Graphical summaries

### Ex 2, histogram
hist(SeaLions$Weight)
min(SeaLions$Weight)
max(SeaLions$Weight)
mean(SeaLions$Weight)

### Ex 3, boxplot
?boxplot

boxplot(Weight ~ Island)
