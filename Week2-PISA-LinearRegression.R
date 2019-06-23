## Read data from files
pisaTest = read.csv('pisa2009test.csv')
pisaTrain = read.csv('pisa2009train.csv')
str(pisaTrain)

## the average reading test score of males and females
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

## missing data in at least one observation in the training set
summary(is.na(pisaTrain))

## Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

str(pisaTrain)
str(pisaTest)

summary(pisaTrain$raceeth)

## Set the reference level of the factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

str(pisaTrain)

## linear regression model using to predict readingScore 
## using all the remaining variables.
lmScore = lm(readingScore ~., data = pisaTrain)
summary(lmScore)

## Computing the root-mean squared error of the model
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

## Predicting on unseen data
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)
max(predTest) - min(predTest)

## Test set SSE and RMSE on testing set
SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

## Baseline prediction and test-set SSE
## We call the sum of squared errors 
## for the baseline model the total sum of squares (SST).
baseline = mean(pisaTrain$readingScore)

SST = sum((mean(pisaTest$readingScore) - pisaTest$readingScore)^2)
SST
R2 = 1 - SSE/SST
R2
