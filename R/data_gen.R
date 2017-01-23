#' data_gen
#'
#' data generating function
#'
#' @param edge_list a \code{\link[sparsebnUtils]{edgeList}} object.
#' @param n size of the data set, a scaler
#' @param ivn, a list of intervention for each data point.
#' @param n_levels, a list of number of levels for each node, default is binary data set.
#' @param coef, coefficient list (optional).
#' @return data matrix
#' @export
data_gen <- function(edge_list,
                     n,
                     ivn = NULL,
                     n_levels = NULL,
                     coef = NULL)
{
  datGen_call(edge_list = edge_list,
              dataSize = n,
              ivn = ivn,
              nlevels = n_levels,
              coef = coef)
}

datGen_call <- function(edge_list,
                        dataSize,
                        ivn,
                        nlevels,
                        coef)
{
  # check input
  if(!sparsebnUtils::is.edgeList(edge_list)) stop("edge_list must be a edgeList object!")

  ts = NULL

  dag_igraph <- sparsebnUtils::to_igraph(edge_list)
  ts <- as.integer(names(igraph::topo_sort(dag_igraph)))

  if (is.null(ts)) stop("Need topological sort for the graph!")

  edge_list <- as.list(edge_list)
  edge_list <- lapply(edge_list, as.integer)

  maxdeg <- max(sapply(edge_list, length))
  maxdeg <- as.integer(maxdeg)

  node <- length(edge_list)
  node<- as.integer(node)

  ordex <- sapply(edge_list, function(x, maxdeg){
    as.integer(c(x, rep(0, maxdeg-length(x))))
  }, maxdeg)
  ordex <- matrix(ordex, nrow = maxdeg)

  if(!is.numeric(dataSize) || length(dataSize) > 1) stop("data_size must be a scaler!")
  if(dataSize < 1) stop("data_size must be a positive integer!")
  dataSize <- as.integer(dataSize)

  if (is.null(ivn)) {
    ivn <- as.list(rep(0, dataSize))
  }
  if(!is.list(ivn)) stop("ivn must be a list!")
  if(length(ivn)!=dataSize) stop("length of ivn not compatible with data_size")
  ivn <- lapply(ivn, function(x){as.integer(x-1)})

  if (is.null(nlevels)) {
    nlevels <- rep(2, node)
  }
  if(!is.vector(nlevels)) stop("n_levels must be a vector!")
  if(sum(nlevels<2)) stop("number of levels must be at least 2!")
  if(length(nlevels)!=node) stop("length of n_levels not compatible with edge_list!")
  nlevels <- as.integer(nlevels)

  if(is.null(coef)) stop("coef must have some value!")
  if(!is.list(coef)) stop("coef must be a list!")
  # check type of list element
  if (sum(sapply(coef, function(x){!is.matrix(x) && !is.null(x)}))) stop("element of coeff must be matrix!")
  # check dimension of the list element
  flag=FALSE
  for (i in 1:node) {
    if (length(edge_list[[i]])) {
      if (nrow(coef[[i]]) != nlevels[i])
        flag = TRUE
      if (ncol(coef[[i]]) != sum(nlevels[edge_list[[i]]]-1)+1) {
        flag = TRUE
      }
    } else {
      if (!is.null(coef[[i]])){
        flag = TRUE
      }
    }
  }
  if (flag == TRUE) stop("coef does not compatible with edge_list!")
  coef_list <- lapply(1:node, function(x, coef, edge_list, nlevels){
    out <- NULL
    if (length(edge_list[[x]])) {
      index <- rep(1:(length(edge_list[[x]])+1), c(1, nlevels[edge_list[[x]]]-1))
      m_to_list <- vector("list", length = length(index))
      for (i in 1:length(index)) {
        m_to_list[[i]] <- coef[[x]][, index==i, drop = FALSE]
      }
      out = m_to_list
    }
    out
  }, coef, edge_list, nlevels)

  # coef_length
  coef_length <- sapply(coef_list, length)
  coef_length <- as.integer(coef_length)

  # call DatGen_cpp
  DatGen_cpp(maxdeg, node, ordex, ts, dataSize, ivn, nlevels, coef_list, coef_length)
}


# function that directly calls from cpp
# type check, no converting type of an input at this point

DatGen_cpp <- function(maxdeg,
                   node,
                   ordex,
                   ts,
                   dataSize,
                   ivn,
                   nlevels,
                   coef_list,
                   coef_length)
{
  # check for maxdeg
  if(!is.integer(maxdeg)) stop("maxdeg must be an integer!")
  if(maxdeg <= 0) stop("maxdeg must be a positive integer!")

  # check for node
  if(!is.integer(node)) stop("node must be an integer!")
  if(node <= 0) stop("node must be a positive integer!")

  # check for ordex
  if(!is.matrix(ordex)) stop("ordex must be a matrix!")
  if(sum(sapply(ordex, function(x){!is.integer(x)}))!=0) stop ("ordex has to be a matrix with integer entries!")
  if(nrow(ordex) != maxdeg) stop("Incompatible size! Number of rows of ordex should maxdeg!")
  if(ncol(ordex) != node) stop("Incompatible size! Number of columns of ordex should be node!")

  # check for ts
  if(!is.vector(ts)) stop("ts must be a vector!")
  if(length(ts)!=node) stop("ts must have length node!")
  if(sum(!is.integer(ts))) stop("ts must be a vector of integer elements!")

  # check for dataSize
  if(!is.integer(dataSize)) stop("dataSize must be an integer!")
  if(dataSize <= 0) stop("dataSize must be a positive integer!")

  # check for ivn
  if(!is.list(ivn)) stop("ivn must be a list")
  if(sum(sapply(ordex, function(x){sum(!is.integer(x)!=0)}))) stop("ivn must be a list of integer vectors!")
  if(length(ivn)!=dataSize) stop("ivn must be a list of length dataSize!")

  # check for nlevels
  if(!is.vector(nlevels)) stop("nlevels must be a vector!")
  if(sum(!is.integer(nlevels))) stop("nlevels must be a vector of integers!")
  if(length(nlevels)!=node) stop("length of nlevels must be node!")

  # check for coef
  if(!is.list(coef_list)) stop("coef must be a list!")
  if(sum(sapply(coef_list, function(x){!is.list(x) && !is.null(x)}))) stop("coef_list must be a list of list")
  for (i in 1:node) {
    if (length(coef_list[[i]])) {
      for (j in 1:length(coef_list[[i]])) {
        if (!is.matrix(coef_list[[i]][[j]])) {
          # cat("error!")
          stop("element of coef_list must be NULL or a matrix!")
        }
      }
    }
  }
  for (i in 1:node) {
    if (length(coef_list[[i]])) {
      for (j in 1:length(coef_list[[i]])) {
        if (!is.numeric(coef_list[[i]][[j]])) {
          cat("error!")
          # stop("element of coef_list must be NULL or a numeric matrix!")
        }
      }
    }
  }

  if (length(coef_length)!= node) stop("length of coef_length should be node!")
  if (!is.integer(coef_length)) stop("coef_length must be an integer!")

  # call function from cpp
  DatGen(maxdeg, node, ordex, ts, dataSize, ivn, coef_length, nlevels, coef_list)
}

#' coef_gen
#'
#' coefficient generating function
#'
#' @param edge_list a \code{\link[sparsebnUtils]{edgeList}} object.
#' @param n_levels, a list of number of levels for each node.
#' @param FUN, a probability distrubution to generate coefficients
#' @param flip, a bool parameter. If true, will randomly flip the sign of coefficients.
#' @return A list of coefficient matrix
#' @export
coef_gen <- function(edge_list, n_levels, FUN=NULL, flip=TRUE) {
  if (is.null(FUN)) {
    FUN <- function(n) {
      stats::runif(n, 1, 3)
    }
  }
  coef <- lapply(1:length(edge_list), function(x, edge_list, flip){
    if (length(edge_list[[x]])==0) {
      coef_matrix <- NULL;
    }
    else {
      ncol_coef <- 1
      for (i in 1:length(edge_list[[x]])) {
        ncol_coef = ncol_coef + n_levels[edge_list[[x]][i]]-1
      }
      n_coef <- n_levels[x]*ncol_coef
      coef_vector <- replicate(n_coef, FUN(n=1))
      if (flip) {
        flip_flag <- sample(c(-1, 1), size = n_coef, replace = TRUE)
        coef_vector <- coef_vector * flip_flag
      }
      coef_matrix <- matrix(coef_vector, nrow = n_levels[x])
    }
    return(coef_matrix)
  }, edge_list, flip)
  return(coef)
}

#' generate_discrete_data
#'
#' A function that generate discrete data set.
#'
#' @param edge_list a \code{\link[sparsebnUtils]{edgeList}} object.
#' @param n size of the data set, a scaler
#' @param ivn, a list of intervention for each data point.
#' @param n_levels, a list of number of levels for each node, default is binary data set.
#' @param coef, coefficient list (optional).
#' @param FUN, a function to generate magnitude of influence (optional).
#' @param flip, a bool parameter. If true, when generating coefficients, will randomly flip the sign of coefficients.
#' @return data matrix
#' @export
generate_discrete_data <- function(edge_list,
                                   n,
                                   ivn = NULL,
                                   n_levels = NULL,
                                   coef = NULL,
                                   FUN = NULL,
                                   flip = TRUE)
{
  # check n_levels
  if(is.null(n_levels)) {
    n_levels <- rep(2, length(edge_list))
  }

  # check coef
  if(is.null(coef)) {
    coef <- coef_gen(edge_list, n_levels, FUN, flip)
  }

  # call data_gen
  data_gen(edge_list = edge_list, n = n, ivn = ivn, n_levels = n_levels, coef = coef)
}
