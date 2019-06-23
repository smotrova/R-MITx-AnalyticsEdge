# Read in Data
gerber = read.csv("./DataFiles/gerber.csv")
str(gerber)

# familiar with data
# proportion of people in this dataset voted in this election
table(gerber$voting)
108696/(235388 + 108696)

# Voting in the four "treatment groups" 
table(gerber$voting, gerber$civicduty)

table(gerber$voting[gerber$civicduty == 1])
12021/(26197+12021)

table(gerber$voting[gerber$hawthorne == 1])
12316/(25888+ 12316)

table(gerber$voting[gerber$self == 1])
13191/(25027 + 13191)

table(gerber$voting[gerber$neighbors == 1])
14438/(23763 + 14438)

# logistic regression model for voting

voting.log = glm(voting ~ civicduty+hawthorne+self+neighbors, data = gerber, family = binomial)
summary(voting.log)

# Make predictions
voting.predict = predict(voting.log, type = "response")

# Analyze predictions
summary(voting.predict)
tapply(voting.predict, gerber$voting, mean)

# Confusion matrix for threshold = 0.3
table(gerber$voting, voting.predict > 0.3)
(134513+51966)/(134513+100875+56730+51966)

# Confusion matrix for threshold = 0.5
table(gerber$voting, voting.predict > 0.5)
235388/(235388 + 108696)

# Classification and regression tree
voting.tree = rpart(voting ~ civicduty+hawthorne+self+neighbors, data = gerber)
prp(voting.tree)

# group treatment
voting.tree = rpart(voting ~ civicduty  +hawthorne + self + neighbors, data = gerber, cp = 0.0)
prp(voting.tree, digits = 6)

# group treatment + sex
voting.tree.sex = rpart(voting ~ civicduty+hawthorne + self + neighbors + sex, data = gerber, cp = 0.0)
prp(voting.tree.sex, digits = 6)

#control group
voting.tree.control = rpart(voting ~ control, data = gerber, cp = 0.0)
prp(voting.tree.cntrol, digits = 6)
abs(0.296638-0.34)

# control + sex
voting.tree.sex1 = rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(voting.tree.sex1, digits = 6)

# logistic regression model using "sex" and "control"
voting.log.sex = glm(voting ~ control+sex, data = gerber, family = binomial)
summary(voting.log.sex)

# Problem 3.4 - Interaction Terms
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(voting.log.sex, newdata=Possibilities, type="response")

0.290456 - 0.2908065

# add a new term to our logistic regression now, 
  # that is the combination of the "sex" and "control" variables
    # 1 means the person is a woman AND in the control group

voting.log.sex1 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(voting.log.sex1)

predict(voting.log.sex1, newdata=Possibilities, type="response")
0.29046 - 0.290456 
