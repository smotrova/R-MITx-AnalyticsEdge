## Read data from files
fluTrain = read.csv('./DataFiles/FluTrain.csv')
str(fluTrain)
summary(fluTrain)

## 
fluTrain$Week[which.max(fluTrain$Queries)]
fluTrain$Week[which.max(fluTrain$ILI)]

## histogram of the dependent variable, ILI

hist(fluTrain$ILI)

plot(log(fluTrain$ILI), fluTrain$Queries)

model1 = lm(fluTrain$Queries ~ log(fluTrain$ILI))
summary(model1)
