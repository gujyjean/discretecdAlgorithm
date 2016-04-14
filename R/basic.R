# to get sparsebnFit object from the adjacency matrix
get.summary <- function(graph, dataSize, lambda, time) {
  lambda = lambda
  nedge <- sum(graph)
  pp <- ncol(graph)
  nn <- dataSize
  edges <- apply(graph, 2, function(x){
    return(which(x==1))
  })
  if (length(edges) == 0) {
    edges <- vector("list", length = pp)
  }
  edges <- sparsebnUtils::edgeList(edges)
  return(list(edges = edges, lambda = lambda, nedge = nedge, pp = pp, nn = nn, time = time))
}

# to change a output solution path of matrix to a list
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

# to get obsIndex from a given intervention list (ivn)
get_obsIndex <- function(ivn, node) {
  obsIndex <- vector("list", length = node)
  ind <- 1:node
  obsIndex <- lapply(ind, function(x, ivn) {
    if_in <- sapply(ivn, function(y) {x %in% y})
    observation <- 1:length(ivn)
    obsIndex_one <-observation[!if_in]
    if (length(obsIndex_one)==0) {obsIndex_one == 0L}
    obsIndex_one
  }, ivn)
  return(obsIndex)
}
