# install package “VIM”
install.packages("VIM") # Hint: use multiple cores. It goes much faster
# To use the package in an R session, we need to load it in an R session via
library()
library(VIM)
# Load dataset “sleep”, which comes within the package “VIM”
data(sleep, package ="VIM")
# call function head() to get a feeling about data, or call sleep to see all values
head(sleep)
# download package “mice” and load it into R
install.packages("mice")
library(mice)

First, we need to know how many rows in “sleep”
nrow(sleep)
## [1] 62
# We use complete.cases() or na.omit() to see tuples without missing value.
sleep[complete.cases(sleep),]
# or try
na.omit(sleep)
# Count the number of rows without missing value
nrow(sleep[complete.cases(sleep),])
## [1] 42
# To reverse the condition logic (rows containing one or more missing value), we
use the exclamation mark highlighted in Red
sleep[!complete.cases(sleep),]
nrow(sleep[!complete.cases(sleep),])
## [1] 20

# Check how many observations contain missing value in column “Dream”
sum(is.na(sleep$Dream))
## [1] 12
# About 19% of obs (observations) in column Dream contain missing value
mean(is.na(sleep$Dream))
## [1] 0.1935484
# 32% obs in data frame sleep containing one or more missing value
mean(!complete.cases(sleep))
## [1] 0.3225806
# call function md.pattern(). Make sure you loaded package mice into R first
md.pattern(sleep)
# call function aggr (), prop = FALSE convert percentage value into counts
aggr(sleep, prop = FALSE, numbers = TRUE)

# call function marginplot (), pch indicates notation of obs, col tells R how you
would like to see results in different color
marginplot(sleep[c("Gest", "Dream")], pch=c(20),
col = c("darkgray","red","blue") )

boxplot(mpg ~ cyl, # mpg is the target variable
# cyl is the explanatory variable
data = mtcars,
col = "grey",
main = "Mileage Data",
ylab = "MPG",
xlab = "Number of Cylinders" )

install.packages(“vioplot”)
library(vioplot)
v1 <- mtcars$mpg[mtcars$cyl == 4]
v2 <- mtcars$mpg[mtcars$cyl == 6]
v3 <- mtcars$mpg[mtcars$cyl == 8]
# draw violin plots for vectors
vioplot(v1,v2,v3,
names=c(“4 cylinders”, “6 cylinders”, “8 cylinders”),
col=“gold”)
plot(mpg ~ wt, data = mtcars)


# Naive Bayes

getwd()
setwd('/xdisk/chrisreidy/workshops')
library(e1071)
mushroom <- read.csv('Mushroom.csv', na.strings = '?')
summary(mushroom)
nrow(mushroom[!complete.cases(mushroom),])
mushroom = mushroom[complete.cases(mushroom),]
nrow(mushroom)
sample_size <- floor(0.7 * nrow(mushroom))
training_index <- sample(nrow(mushroom), size = sample_size, replace = FALSE)
train <- mushroom[training_index,]
test <- mushroom[-training_index,]
mushroom.model <- naiveBayes(classes ~ . , data = train)
mushroom.model
mushroom.predict <- predict(mushroom.model, test, type = "class")
results <- data.frame(actual = test[,'classes'], predicted = mushroom.predict)

library(ISLR)
print(head(College, 2))
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)
scaled.data <- as.data.frame(scale(College[,2:18], center = mins, 
                                   scale = maxs - mins))
print(head(scaled.data,2))
Private = as.numeric(College$Private)-1
data = cbind(Private, scaled.data)
library(caTools)
set.seed(101)
split = sample.split(data$Private, SplitRatio = 0.70)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
feats <- names(scaled.data)
f <- paste(feats,collapse=' + ')
f <- paste('Private ~', f)
# Convert to formula
f <- as.formula(f)
f
library(neuralnet)
nn <- neuralnet(f, train, hidden = c(10,10,10), linear.output = FALSE)
predicted.nn.values <- compute(nn, test[2:18])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,
                                         round, digits = 0)
