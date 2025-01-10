## actors network example
library(igraph)
### GRAPH
## read in a graph in the `graphml' format: xml for graphs.
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)
actnet <- read.graph("actors.graphml",format="graphml")
### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.
movies <- read.table("movies.txt", sep="\t",
                     row.names=1, as.is=TRUE, comment.char="", quote="")
## it's a 1 column matrix.
#treat it like a vector
movies <- drop(as.matrix(movies))
## each element is a comma-separated set of actor ids.
## use `strsplit' to break these out
movies <- strsplit(movies,",")
## and finally, match ids to names from actnet
casts <- lapply(movies,
                function(m) V(actnet)$name[match(m,V(actnet)$id)])
## check it
casts['True Romance']