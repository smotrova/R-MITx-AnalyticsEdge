# Loading data
emails = read.csv("./DataFiles/emails.csv", stringsAsFactors = F)
str(emails)

# How many emails are spam
table(emails$spam)
strwrap(emails$text[1:5])

# How many characters in longest e-mail
nchar(emails$text[which.max(nchar(emails$text))])

# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))

# Build a new corpus variable called corpus
corpus = Corpus(VectorSource(emails$text))
corpus[[3]]

# Using tm_map, convert the text to lowercase
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

# Using tm_map, remove all punctuation from the corpus.
corpus = tm_map(corpus, removePunctuation)

# Using tm_map, remove all English stopwords from the corpus
corpus = tm_map(corpus, removeWords, stopwords("en"))

# Using tm_map, stem the words in the corpus
corpus = tm_map (corpus, stemDocument)

# Build a document term matrix from the corpus, called dtm
dtm = DocumentTermMatrix(corpus)
dtm

# To obtain a more reasonable number of terms, 
# limit dtm to contain terms appearing in at least 5% of documents, 
# and store this result as spdtm
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# Build a data frame called emailsSparse from spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))

# Make all variables name R-friendly
head(emailsSparse)
colnames(emailsSparse) = make.names(colnames(emailsSparse))
head(emailsSparse)

# What is the word stem that shows up most frequently across all the emails in the dataset?
sort(colSums(emailsSparse))

# Add a variable called "spam" to emailsSparse containing the email spam labels
emailsSparse$spam = emails$spam

# How many word stems appear at least 1000 times in the spam emails in the dataset?
sort(colSums(emailsSparse[emailsSparse$spam == 1, ]))
sort(colSums(subset(emailsSparse, spam == 1)))

# convert the dependent variable to a factor with 
emailsSparse$spam = as.factor(emailsSparse$spam)

# split into train and test sets
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split==T)
test = subset(emailsSparse, split == F)

# logistic regression model called spamLog
spamLog = glm(spam ~ ., data = train, family = "binomial")

predLog = predict(spamLog, type = "response")
table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog > 0.00001 & predLog < 0.99999)

summary(spamLog)

# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, predLog > 0.5)
(3052+954)/(3052+954+4)

# What is the training set AUC of spamLog?
predROCR = prediction(predLog, train$spam)
# Compute AUC
performance(predROCR, "auc")@y.values

# CART model called spamCART
spamCART = rpart(spam~., data = train, method = "class")
prp(spamCART)
predCART = predict(spamCART, type = "class")
table(train$spam, predCART)
(2885+894)/(2885+167+64+894)


predCART = predict(spamCART, data=train)
predROCR1 = prediction(predCART[,2], train$spam)
# Compute AUC
performance(predROCR1, "auc")@y.values

# random forest model called spamRF
spamRF = randomForest(spam~., data = train)
predRF = predict(spamRF, data = train)

table(train$spam, predRF)
(3015+920)/(3015+920+37+38)

summary(predRF)

predRF = predict(spamRF, data = train, type="prob")
predROCR2 = prediction(predRF[,2], train$spam)
# Compute AUC
performance(predROCR2, "auc")@y.values

# ======== Evaluating on the Test Set ============

# Obtain predicted probabilities for the testing set for each of the models
# ensuring that probabilities instead of classes are obtained.

predLog = predict(spamLog, newdata = test, type = "response")
table(test$spam, predLog > 0.5)
(1257+376)/(1257+376+51+34)

# What is the testing set AUC of spamLog?
predROCR = prediction(predLog, test$spam)
# Compute AUC
performance(predROCR, "auc")@y.values

# ------------------------------------

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
predCART = predict(spamCART, newdata = test, type = "prob")
table(test$spam, predCART[,2] > 0.5)
(1228+386)/(1228+386+80+24)

# What is the testing set AUC of spamCART?
predROCR1 = prediction(predCART[,2], test$spam)

# Compute AUC
performance(predROCR1, "auc")@y.values

# --------------------------------

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
predRF = predict(spamRF, newdata = test, type="prob")
table(test$spam, predRF[,2] >0.5)
(1289+386)/(1289+386+19+24)

# What is the testing set AUC of spamRF?
predROCR2 = prediction(predRF[,2], test$spam)

# Compute AUC
performance(predROCR2, "auc")@y.values
