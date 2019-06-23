# read in data
letters = read.csv("./DataFiles/letters_ABPR.csv")
str(letters)

# create a new variable isB in the dataframe, 
  # which takes the value "TRUE" if the observation corresponds to the letter B, 
    # and "FALSE" if it does not
letters$isB = as.factor(letters$letter == "B")
str(letters)
table(letters$isB)

# split the data set into a training and testing set
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)

lettersTrain = subset(letters, spl == T)
lettersTest = subset(letters, spl == F)

# Baseline method (check for the test set)
table(lettersTest$isB)
1175/(1175+383)

# build a classification tree to predict whether a letter is a B or not
  # Remember to remove the variable "letter" out of the model, 
    # as this is related to what we are trying to predict! 
CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")  # using the default parameters
prp(CARTb)

# Make predictions
PredictTest = predict(CARTb, newdata = lettersTest, type = "class")
table(lettersTest$isB, PredictTest)

(1118+340)/nrow(lettersTest)

# biuld a random forest model
library(randomForest)
ForestB = randomForest(isB ~ . - letter, data=lettersTrain)

# make predictions based on random forest model
PredictForestB = predict(ForestB, newdata = lettersTest)
table(lettersTest$isB, PredictForestB)
(1164+373)/nrow(lettersTest)

# predict whether or not a letter is one of the four letters A, B, P or R.
table(letters$letter)

# converting letter in the original data set (letters) to a factor
letters$letter = as.factor( letters$letter )

# split the data accoding to letter factor
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)

lettersTrain = subset(letters, spl == T)
lettersTest = subset(letters, spl == F)

# Baseline accuracy on test set
table(lettersTest$letter)

# In a multiclass classification problem, 
  # a simple baseline model is to predict 
    # the most frequent class of all of the options.
401/nrow(lettersTest)

library(rpart)
library(rpart.plot)

CART = rpart(letter ~ . -isB, data = lettersTrain, method = "class")
prp(CART)

# make predictions
PredictTest1 = predict(CART, newdata = lettersTest, type = "class")
table(PredictTest1, lettersTest$letter)

# accuracy
(348+318+363+340)/nrow(lettersTest)

# biuld a random forest model
library(randomForest)
Forest = randomForest(letter ~ . - isB, data=lettersTrain)

# make predictions based on random forest model
PredictForest = predict(Forest, newdata = lettersTest)
table(lettersTest$letter, PredictForest)
(390+380+393+369)/nrow(lettersTest)
