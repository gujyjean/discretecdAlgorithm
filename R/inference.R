# a function to convert the coefficient vector into a list of coefficients
get_coef_matrix <- function(coef_vec, n_levels) {
  # a vector to index each independant varibles
  node <- 1:length(n_levels)
  # a vector to index each coefficient in coef_vec
  node_index <- rep(node, (n_levels-1))
  if (!is.matrix(coef_vec)) {coef_vec <- matrix(coef_vec, nrow=1)}
  # check if number of columns of coef_vec is compatible with n_levels
  if (ncol(coef_vec)!=sum(n_levels-1)) stop("number of columns of coef_vec is not compatible with n_levels")
  if (length(n_levels) >=2) {
    coef_matrix <- lapply(node, function(x, node_index, coef_vec){matrix(coef_vec[, which(node_index==x)], nrow=nrow(coef_vec))}, node_index, coef_vec)
  } else {
    coef_matrix <- list(coef_vec)
  }

  return(coef_matrix)
}

#' A function to do inference in Bayesian network.
#' @param parents An edgeList, sparsebnFit object or adjacency matrix.
#' @param n_levels A vector indicating number of levels for each variable
#' @param dat Data, a dataframe or matrix
#' @return
#' List, of length the number of nodes. ith entry of the list is another list that contains parents of node i and the intercept coefficient for the ith node. For each parent of node i, it is a list of index of parent and the coefficient matrix of influence of parent has on node i.
#' @export
fit_multinom_dag <- function(parents, # rename to something else
                    n_levels,
                    dat
                    ) {
  data <- as.data.frame(dat)

  node <- ncol(data)
  ### add check that the number of node and the what has been input in parents are consistent!

  # factorize each observation
  for (i in 1:node){
    level <- 0:(n_levels[i]-1)
    data[,i] <- factor(data[,i],levels=level)
  }

  # get adjacency matrix
  ### if parents is an edgeList object
  if (sparsebnUtils::is.edgeList(parents) || sparsebnUtils::is.sparsebnFit(parents)) {
    adjMatrix <- sparsebnUtils::get.adjacency.matrix(parents)
  }
  ### if parents is an adjacency matrix
  if (is.matrix(parents) || class(parents) == "dgCMatrix") {
    if (sum(((parents!=0)+(parents!=1))!=1)) stop("input matrix must be an adjacency matrix, where 1 means there exists an edge and 0 means there is no edge!")
    adjMatrix <- parents
  }
  ### throw an error if parents is neither an adjacency matrix nor an edgeList object
  if (!(sparsebnUtils::is.edgeList(parents) || sparsebnUtils::is.sparsebnFit(parents) || is.matrix(parents) || class(parents) == "dgCMatrix")) stop("parents must be an edgeList object or sparsebnFit object or an adjacency matrix!")

  # subtract dependent and independent variables for each regression
  # coef <- vector("list", length = node)
  coef <- lapply(seq_len(node), function(i){integer(0)})
  for (i in 1:node){
    x_ind <- which(adjMatrix[, i]==1) # index for independant variable
    if (length(x_ind)!=0) { # do nothing if a node has no parents
      fit <- nnet::multinom(data[, c(i, x_ind)], trace = FALSE) # Why does this work / should we do this?
      coef_vec <- coef(fit)
      temp_n_levels <- n_levels[x_ind]
      intercept <- coef_vec[1]
      coef_vec <- coef_vec[-1]
      coef_seq <- get_coef_matrix(coef_vec, temp_n_levels)
      node_index <- 1:length(x_ind)
      coef[[i]] <- lapply(node_index, function(x, coef_seq, x_ind){list(parent=x_ind[x], coef=coef_seq[[x]])}, coef_seq, x_ind)
      coef[[i]][[length(x_ind)+1]] <- list(intercept=intercept)
    }
  }
  return(coef)
}
