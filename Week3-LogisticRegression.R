# The Analytics Edge
# Week 3
# Logistic Regression

quality = read.csv("quality.csv")
str(quality)
summary(quality)

# separe data set into test and train subsets
library(caTools)
set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

train = subset(quality, split == TRUE)
test = subset(quality, split == FALSE)

# Create a logistic regression model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=train, family=binomial)
summary(QualityLog)


predictTest = predict(QualityLog, type="response", newdata=test)

library(ROCR)
ROCRpredTest = prediction(predictTest, test$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

#==========================================
# Understanding the data

songs = read.csv("songs.csv")
str(songs)

table(songs$year)
length(songs$year[songs$year == 2010])

length(songs$artistname[songs$artistname == "Michael Jackson"])
table(songs$Top10[songs$artistname == "Michael Jackson"])

songs$songtitle[songs$Top10 == 1 & songs$artistname == "Michael Jackson"]

table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

# creating prediction model
# split the data into training and testing subsets
table(songs$year)

SongsTrain = subset(songs, songs$year <= 2009)
SongsTest = subset(songs, songs$year == 2010)

nrow(SongsTrain)

# remove variables from model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# build a logistic regression model to predict Top10
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ .-loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ .-energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Test set predictions
Top10Pred = predict(SongsLog3, newdata = SongsTest, family = "response")
table(SongsTest$Top10, Top10Pred >= 0.45)

# Model 3 accuracy
mean(SongsTest$Top10 == (Top10Pred >= 0.45))

# Baseline accuracy
# Most frequent outcome
table(SongsTest$Top10)
max(table(SongsTest$Top10))/nrow(SongsTest)

#==============================================
# Predicting Parole Violators

parole = read.csv("parole.csv")
str(parole)
table(parole$violator)

table(parole$state)
table(parole$crime)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

summary(parole)

# devide data set into test and training sets

set.seed(144)
library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE )

ViolatorLog1 = glm(violator~., data = train, family = "binomial")
summary(ViolatorLog1)

table(parole$state)
summary(parole$state)

# Odds for 1 person
ViolatorLog1$coefficients

beta = ViolatorLog1$coefficients
x = c(1, 1, 1, 50, 0, 0, 0, 3, 12, 0, 1, 0, 0)
exp(sum(beta*x))

# Probability for 1 person
1/(1+exp(sum(-beta*x)))

# Prediction on test set
PredictViol1 = predict(ViolatorLog1, newdata = test, type = "response")

summary(PredictViol1)
max(PredictViol1)

# Confusion Matrix
ConfusionMatrix = table(test$violator, PredictViol1 >= 0.5)

sensitivity =  ConfusionMatrix[2,2]/(ConfusionMatrix[2,2] + ConfusionMatrix[2,1])
specifity = ConfusionMatrix[1,1]/(ConfusionMatrix[1,1] + ConfusionMatrix[1,2])
sensitivity
specifity

# model accuracy
mean(test$violator == (PredictViol1 >= 0.5))

# accuracy of a simple model
table(test$violator)
max(table(test$violator))/length(test$violator)

# ROC Curve, AUC value
library(ROCR)
pred = prediction(PredictViol1, test$violator)

# Performance function
ROCRperf = performance(pred, "tpr", "fpr")
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
as.numeric(performance(pred, "auc")@y.values)

# Predicting loan repayment

loans = read.csv("loans.csv")
str(loans)
summary(loans)

mean(loans$not.fully.paid)


# Variables that have missing values
Names <- c()
for (x in names(loans)) {
  if (anyNA(loans[x])) {
    Names = c(Names, x)
  }
}

Names
rm(x)

# Imputation of missing values

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# to check the results of imputation compare with given imputed data
loans_imputed = read.csv("loans_imputed.csv")

for (x in Names) {
  print( summary(loans[x]) )
  print(summary(loans_imputed[x]) )
}

# splitting into test and train data sets
library(caTools)
set.seed(144)
split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)

train = subset(loans_imputed, split == TRUE)
test = subset(loans_imputed, split == FALSE)

notPaidLog = glm(not.fully.paid ~., data = train, family = "binomial")
summary(notPaidLog)

# prediction on test set
notPaidPred = predict(notPaidLog, newdata = test, type = "response")
predicted.risk = notPaidPred

# add predcited.risk to test data set
test1 = cbind(test, predicted.risk)

# accuracy of the logistic regression model
mean(test$not.fully.paid == (notPaidPred >=0.5))

# accuracy of the baseline model
max(table(test$not.fully.paid))/length(test$not.fully.paid)
table(test$not.fully.paid)

# confusion matrix
table(test$not.fully.paid, notPaidPred >=0.5)

#Use the ROCR package to compute the test set AUC

library(ROCR)
ROCRPred = prediction(notPaidPred, test$not.fully.paid)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")

plot(ROCRPerf, colorize = TRUE)
as.numeric(performance(ROCRPred, "auc")@y.values)

#  build a bivariate logistic regression model 
notPaidLog2 = glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(notPaidLog2)

notPaidPred2 = predict(notPaidLog2, newdata = test, type = "response")

max(notPaidPred2)

mean(notPaidPred2 >= 0.5)

ROCRPred2 = prediction(notPaidPred2, test$not.fully.paid)
ROCRPerf2 = performance(ROCRPred2, "tpr", "fpr")
plot(ROCRPerf2, colorize = TRUE)

as.numeric(performance(ROCRPred2, "auc")@y.values)

# profit calculation

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

max(10*test$profit)

highInterest = subset(test, test$int.rate >= 0.15)
summary(test$int.rate)

mean(highInterest$profit)

table(highInterest$not.fully.paid)
mean(highInterest$not.fully.paid)

# we will determine the 100th smallest predicted probability of not paying in full 
#by sorting the predicted risks in increasing order 
#and selecting the 100th element of this sorted list.

test1$profit = exp(test$int.rate*3) - 1
test1$profit[test$not.fully.paid == 1] = -1
highInterest = subset(test1, test1$int.rate >= 0.15)

summary(highInterest)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highInterest, highInterest$predicted.risk <= cutoff)

sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
