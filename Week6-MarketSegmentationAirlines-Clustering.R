# import data set
Airlines <- read.csv("D:/LENA/edX/MITx-AnalyticEdge-15.071x/Assignments/DataFiles/AirlinesCluster.csv")
str(Airlines)
summary(Airlines)

# Normalizing the Data before clustering

# If we don't normalize the data, the clustering will be dominated by the variables
# that are on a larger scale.

# normalize the variables in a data frame by using the preProcess function in the "caret" package. 
library("caret")

preproc = preProcess(Airlines) # Pre-processing
airlinesNorm = predict(preproc, Airlines) # Normalization

# normalization ==> mean = 0, SD = 1
summary(airlinesNorm)

# Hierarchical clustering

# compute the distance
distances = dist(airlinesNorm, method = "euclidean")

# clustering
clusterAirlinesNorm = hclust(distances, method = 'ward.D')

# plot dendrogramm
plot(clusterAirlinesNorm)

#  Divide the data points into 5 clusters
clusterGroup = cutree(clusterAirlinesNorm, k = 5)

# how many objects in each cluster group
table(clusterGroup)

# the average values in each of the variables for the 5 clusters (the centroids of the clusters)
tapply(airlinesNorm$Balance, clusterGroup, mean)
tapply(airlinesNorm$QualMiles, clusterGroup, mean)
tapply(airlinesNorm$BonusMiles, clusterGroup, mean)
tapply(airlinesNorm$BonusTrans, clusterGroup, mean)
tapply(airlinesNorm$FlightMiles, clusterGroup, mean)
tapply(airlinesNorm$FlightTrans, clusterGroup, mean)
tapply(airlinesNorm$DaysSinceEnroll, clusterGroup, mean)

# k-means cluastering

# number of clustering
k = 5

# ---------------------------------------------------
set.seed(88)
kmcAirlinesNorm = kmeans(airlinesNorm, k, iter.max = 1000)
str(kmcAirlinesNorm)

table(kmcAirlinesNorm$cluster)


# compare the cluster centroids to each other 
kmcAirlinesNorm$centers
