#  cluster-then-predict methodology
#  read the data
Stocks <- read.csv("./DataFiles//StocksCluster.csv")

#  explore data
str(Stocks)
head(Stocks)

#  What proportion of the observations have positive returns in December?
table(Stocks$PositiveDec)
6324/(6324+5256)


#  What is the maximum correlation between any two return variables in the dataset? 
cor(Stocks)

#  Which month (from January through November) has the largest mean return across all observations in the dataset?
summary(Stocks)


#  Initial Logistic Regression Model

#  split data set
library(caTools)

set.seed(144)
spl = sample.split(Stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(Stocks, spl == TRUE)
stocksTest = subset(Stocks, spl == FALSE)

#  Logistic regression model
stocksLog = glm(PositiveDec ~., data = stocksTrain, family = "binomial")
summary(stocksLog)

# Make predictions on training set
predictTrainLog = predict(stocksLog, type = "response")
summary(predictTrainLog)

table(stocksTrain$PositiveDec, predictTrainLog > 0.5)
(990+3640)/nrow(stocksTrain)

#  Now obtain test set predictions from stocksLog
predictTestLog = predict(stocksLog, newdata = stocksTest, type = "response")

table(stocksTest$PositiveDec, predictTestLog > 0.5)
(417+1553)/nrow(stocksTest)

#  What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
1897/(nrow(stocksTest))


#### Clustering Stocks

#  The first step in this process is to remove the dependent variable 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL

limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#  preProcess command to normalize variables
library(caret)

preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

#  What is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#  k-menas clustering
set.seed(144)
k = 3

km = kmeans(normTrain, k)
str(km)

#  Which cluster has the largest number of observations?
table(km$cluster)

# apply clusters to test set
install.packages("flexclust")
library(flexclust)

km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

# How many test-set observations were assigned to Cluster 2?
table(clusterTest)


# Which training set data frame has the highest average value of the dependent variable?

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# Build logistic regression models StocksModel1, StocksModel2, and StocksModel3, 
# which predict PositiveDec using all the other variables as independent variables. 
# StocksModel1 should be trained on stocksTrain1, StocksModel2 should be trained 
# on stocksTrain2, and StocksModel3 should be trained on stocksTrain3.

stocksModel1 = glm(PositiveDec ~., data = stocksTrain1, family = "binomial")
stocksModel2 = glm(PositiveDec ~., data = stocksTrain2, family = "binomial")
stocksModel3 = glm(PositiveDec ~., data = stocksTrain3, family = "binomial")

#  which variable differ in sign between the models
summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel3)

# Make predictions on testing set
predictTest1= predict(stocksModel1, newdata = stocksTest1, type = "response")
predictTest2= predict(stocksModel2, newdata = stocksTest2, type = "response")
predictTest3= predict(stocksModel3, newdata = stocksTest3, type = "response")

#  What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
table(stocksTest1$PositiveDec, predictTest1 >0.5)
(30+774)/nrow(stocksTest1)

#  What is the overall accuracy of StocksModel2 on the test set stocksTest2, using a threshold of 0.5?
table(stocksTest2$PositiveDec, predictTest2 >0.5)
(388+757)/nrow(stocksTest2)


#  What is the overall accuracy of StocksModel3 on the test set stocksTest3, using a threshold of 0.5?
table(stocksTest3$PositiveDec, predictTest3 >0.5)
(49+13)/nrow(stocksTest3)

# To compute the overall test-set accuracy of the cluster-then-predict approach,
# we can combine all the test-set predictions into a single vector and 
# all the true outcomes into a single vector:

AllPredictions = c(predictTest1, predictTest2, predictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllPredictions >0.5, AllOutcomes)

(467+1544)/(467+1544+353+1110)
