# Read data
dailykos <- read.csv("./DataFiles/dailykos.csv", header = FALSE)
str(dailykos)

# Compute distanses
distance = dist(dailykos, method = "euclidean")

# Hierarchical clustering
clustersDailykos = hclust(distance, method = "ward.D")

# Plot the dendrogram
plot(clustersDailykos)

# Asign point to clusters
clusterGroup = cutree(clustersDailykos, k = 7)
str(clusterGroup)

# How many observations in clusters
table(clusterGroup)


# Create a new data set with elements from cluster 1
Cluster1 = subset(dailykos, clusterGroup == 1)

# This computes the mean frequency values of each of the words in cluster 1
# outputs the 6 words that occur the most frequently
tail(sort(colMeans(Cluster1)))

Cluster2 = subset(dailykos, clusterGroup == 2)
Cluster3 = subset(dailykos, clusterGroup == 3)
Cluster4 = subset(dailykos, clusterGroup == 4)
Cluster5 = subset(dailykos, clusterGroup == 5)
Cluster6 = subset(dailykos, clusterGroup == 6)
Cluster7 = subset(dailykos, clusterGroup == 7)

tail(sort(colMeans(Cluster2)))
tail(sort(colMeans(Cluster3)))
tail(sort(colMeans(Cluster4)))
tail(sort(colMeans(Cluster5)))
tail(sort(colMeans(Cluster6)))
tail(sort(colMeans(Cluster7)))


# k-means clustering
# Read data
dailykos <- read.csv("./DataFiles/dailykos.csv", header = FALSE)
str(dailykos)

# Turn data into matrix and than into vector
dailykosVector = as.vector(dailykos)
str(dailykosVector)

# spesify number of clusters
k = 7
set.seed(1000)

# Create 7 clusters
KMClust = kmeans(dailykosVector, centers = k)
str(KMClust)
table(KMClust$cluster)

# six most frequent word in each cluster

HierCluster1 = subset(dailykos, KMClust$cluster == 1)
HierCluster2 = subset(dailykos, KMClust$cluster == 2)
HierCluster3 = subset(dailykos, KMClust$cluster == 3)
HierCluster4 = subset(dailykos, KMClust$cluster == 4)
HierCluster5 = subset(dailykos, KMClust$cluster == 5)
HierCluster6 = subset(dailykos, KMClust$cluster == 6)
HierCluster7 = subset(dailykos, KMClust$cluster == 7)

tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))


# compare how observations were assigned to clusters in the two different methods
# which Hierarchical Cluster contains at least half of the points in K-Means Cluster

table(clusterGroup, KMClust$cluster)
