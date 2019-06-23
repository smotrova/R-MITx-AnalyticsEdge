# VISUALIZING TEXT DATA USING WORD CLOUDS

# load data
tweets = read.csv("./DataFiles/tweets.csv", stringsAsFactors = FALSE)
str(tweets)

# Create a corpus using the Tweet variable
library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet)) # create a list

# Convert the corpus to lowercase
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

# Remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)

# Remove stopwords and Apple
corpus = tm_map(corpus, removeWords, c("apple",stopwords("en")))

# Build a document-term matrix out of the corpus
frequencies = DocumentTermMatrix(corpus)

frequencies

# Convert the document-term matrix to a data frame called allTweets

allTweets = as.data.frame(as.matrix(frequencies))
str(allTweets)

# How many unique words are there across all the documents?
ncol(allTweets)

# Install worldcloud packadge
install.packages("wordcloud")
library(wordcloud)

# Building a Word Cloud
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

library(RColorBrewer)
display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), color=brewer.pal(9, "Blues"))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), color=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
