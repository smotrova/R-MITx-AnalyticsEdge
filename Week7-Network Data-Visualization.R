# load data
edges = read.csv("./DataFiles/edges.csv")
users = read.csv("./DataFiles/users.csv")

# investigate data sets
str(users)
str(edges)

# igraph package to visualize networks
install.packages(igraph)
library(igraph)

# create a new graph object using the graph.data.frame() function
g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)

# How many users are friends with 10 or more other Facebook users in this network?
table(degree(g)>=10)

# verticles of the graph
V(g)

# specify the vertex size of each vertex
V(g)$size = degree(g)/2+2
min(V(g)$size)

plot(g, vertex.label=NA)

# Coloring Vertices
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "B"] = "gray"

plot(g, vertex.label=V(g)$school )

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)

str(V(g)$locale)
