# read in data
trials = read.csv("./DataFiles/clinical_trial.csv", stringsAsFactors = F)
str(trials)
summary(trials)

# How many characters are there in the longest abstract?
nchar(trials$abstract[which.max(nchar(trials$abstract))])

# How many search results provided no abstract? 
length(trials$abstract[nchar(trials$abstract) == 0])

# Find the observation with the minimum number of characters in the title 
trials$title[which.min(nchar(trials$title))]

# Preparing the Corpus
# both title and abstract information for trials, we need to build two corpera 
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

# Convert to lowercase
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

# view results
corpusTitle[[1]]
corpusAbstract[[5]]

# convert to PlainTextDocument
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# remove punctuation, stop words and stemming
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("en"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("en"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

#  Build a document term matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (5% of documents)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# Convert dtmTitle and dtmAbstract to data frames 
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# What is the most frequent word stem across all the abstracts? 
# Hint: you can use colSums() to compute the frequency of a word across all the abstracts.
sort(colSums(dtmAbstract))

# Building a model
# We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions.
# some of the variables in these data frames have the same names
# To fix this issue, run the following commands:
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# combine dtmTitle and dtmAbstract into a single data frame called dtm
dtm = cbind(dtmTitle, dtmAbstract)
head(dtm)

# add the dependent variable "trial" to dtm, copying it from the original data frame called trials
dtm$trial = trials$trial
head(dtm)

# how many columns in dtm
ncol(dtm)

# split data set to train and test sets
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)

train = subset(dtm, split == T)
test = subset(dtm, split == F)

# Baseline accuracy
table(test$trial)
313/(313+245)   # most frequent outcome / total outcome number

# CART model
trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)

predTest = predict(trialCART, newdata = test)
pred.prob = predTest[,2]
summary(predTest)

# Compute accuracy
table(test$trial, pred.prob >= 0.5)
(261+162)/(261+162+52+83)

# ROC Curve
predROCR = prediction(pred.prob, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values
