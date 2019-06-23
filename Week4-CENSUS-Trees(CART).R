# read in data
census = read.csv("census.csv")
str(census)

# split the data into test and training set
library(caTools)
set.seed(2000)

spl = sample.split(census$over50k, SplitRatio = 0.6)

censusTrain = subset(census, spl == T)
censusTest = subset(census, spl == F)

# Logistic regression model
library(caTools)
over50kLog = glm(over50k ~ . , data = censusTrain, family = binomial)
summary(over50kLog)

# Confusing matrix
over50Pred = predict(over50kLog, type = "response")
summary(over50Pred)
table(censusTrain$over50k, over50Pred > 0.5)

# accuracy of the model on the testing set
(13561+2768)/nrow(censusTrain)

# baseline accurasy on the testing set
table(censusTest$over50k)
9713/(9713+3078)  # more frequent outcome

# Area-under-the-curve (AUC) for this model on the test set
ROCRPred = prediction(over50Pred, censusTrain$over50k)

# Performance function
ROCRPerf = performance(ROCRPred, 'tpr', 'fpr')

# AUC value
as.numeric(performance(ROCRPred, "auc")@y.values)

#======================================================================
# CART model

library(rpart)
library(rpart.plot)

over50kTree = rpart(over50k ~ . , data = censusTrain, method = "class")
prp(over50kTree)

# Accuracy of the model on the testing set. Threshold  = 0.5
PredictTree = predict(over50kTree, newdata = censusTest, type = "class")
table(censusTest$over50k, PredictTree)
(9243+1596)/nrow(censusTest)

# ROC curve, AUC
library(ROCR)

# prediction function
PredictROC = predict(over50kTree, newdata = censusTest)

Pred = prediction(PredictROC[,2], censusTest$over50k)
Pref = performance(Pred, 'tpr', 'fpr')
plot(Pref)

# AUC value
as.numeric(performance(Pred, "auc")@y.values)

# =================================================================================
# Random Forest

# Create a smaller data set for the Random Forest Model
set.seed(1)
trainSmall = censusTrain[sample(nrow(censusTrain), 2000), ]

# build a random forest model to predict "over50k"
library(randomForest)
set.seed(1)
over50Forest = randomForest(over50k ~ . , data = trainSmall)

# Make predictions
predictForest = predict(over50Forest, newdata = censusTest)
table(censusTest$over50k, predictForest)
(9586+1093)/nrow(censusTest)

# compute metrics that give us insight into which variables are important in the Random Forest model
vu = varUsed(over50Forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(over50Forest$forest$xlevels[vusorted$ix]))

# impurity metric, which measures how homogenous each bucket or leaf of the tree is
varImpPlot(over50Forest)

# ==================================================================================
# CART, cross-validation parameters

# install cross-validation packages
library(caret)
library(e1071)

# select the cp parameter for our CART model
set.seed(2)

numFolds = trainControl(method = 'cv', number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# perform the cross-validation
train(over50k ~ . , data = censusTrain, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

# Create a new CART model
over50TreeCV = rpart(over50k ~ . , data = censusTrain, method = 'class', cp = 0.002)

# make predictions
PredictCV = predict(over50TreeCV, newdata = censusTest, type = "class")
table(PredictCV, censusTest$over50k)
(9178+1838)/nrow(censusTest)

# plot a tree for the new CART-CV model
prp(over50TreeCV)
