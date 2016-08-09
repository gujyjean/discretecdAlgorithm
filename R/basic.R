# to get a list of element for sparsebnFit object from the adjacency matrix.
# "graph" is the adjacency matrix
get.summary <- function(graph, dataSize, lambda, time) {
  lambda = lambda
  nedge <- sum(graph)
  pp <- ncol(graph)
  nn <- dataSize
  graph_list <- lapply(seq_len(ncol(graph)), function(i) graph[,i])
  edges <- lapply(graph_list, function(x){
    return(which(x==1))
  })
  edges <- sparsebnUtils::edgeList(edges)
  return(list(edges = edges, lambda = lambda, nedge = nedge, pp = pp, nn = nn, time = time))
}

# to convert an output solution path to a list of nlam elements
# each element is a list contains all required items for sparsebnFit
get.edgeList <- function(edgeMatrix, dataSize, lambda, time) {
  n_node <- ncol(edgeMatrix)
  n_graph <- nrow(edgeMatrix)/n_node
  edgeList <- vector("list", n_graph)
  for (i in 1:n_graph) {
    graph <- edgeMatrix[((i-1)*n_node+1):(i*n_node), ]
    edgeList[[i]] <- get.summary(graph, dataSize, lambda[i], time[i])
  }
  return(edgeList)
}

# to get obsIndex from a given intervention list (ivn).
# if for a node, it is under intervention for all observations, return 0.
get_obsIndex <- function(ivn, node) {
  obsIndex <- vector("list", length = node)
  ind <- 1:node
  obsIndex <- lapply(ind, function(x, ivn) {
    if_in <- sapply(ivn, function(y) {x %in% y})
    observation <- 1:length(ivn)
    obsIndex_one <-observation[!if_in]
    if (length(obsIndex_one)==0) {obsIndex_one = 0L}
    obsIndex_one
  }, ivn)
  return(obsIndex)
}
