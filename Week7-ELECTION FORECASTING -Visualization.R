# load the ggplot2, maps, and ggmap packages
library(ggplot2)
library(maps)
library(ggmap)

# load the US map and save it to the variable statesMap
statesMap = map_data("state")
str(statesMap)

# How many different groups are there?
table(statesMap$group)

# draw a map of the United States
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# read the data
polling = read.csv("./DataFiles/PollingImputed.csv")
str(polling)

# devide into Train and Test sets
# Test set = observations from 2012
# Train set = observations from 2004 and 2008
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# create a logistic regression model and make predictions on the test set 
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

# TestPrediction gives the predicted probabilities for each state
# create a vector of Republican/Democrat predictions 
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# put the predictions and state labels in a data.frame 
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)

# For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(predictionDataFrame$TestPredictionBinary)

# What is the average predicted probability of our model (on the Test set, for 2012)?
mean(predictionDataFrame$TestPrediction)


#  convert the Test.State variable to lowercase
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

# merge the two data frames
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

str(predictionMap)

predictionMap = predictionMap[order(predictionMap$order),]


# color the states according to our binary predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black", size = 5) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=0.3, alpha = 0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# What was our predicted probability for the state of Florida
table(predictionDataFrame$region, predictionDataFrame$TestPrediction)
