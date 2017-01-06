#' data_gen
#'
#' data generating function
#'
#' @param edge_list a \code{\link[sparsebnUtils]{edgeList}} object.
#' @param data_size size of the data set, a scaler
#' @param ivn, a list of intervention for each data point.
#' @param n_levels, a list of number of levels for each node.
#' @param magnitude, a function to generate magnitude of influence
#' @return data matrix
#' @export
data_gen <- function(edge_list,
                     data_size,
                     ivn = NULL,
                     n_levels = NULL,
                     magnitude = NULL)
{
  datGen_call(edge_list = edge_list,
              dataSize = data_size,
              ivn = ivn,
              nlevels = n_levels,
              FUN = magnitude)
}

datGen_call <- function(edge_list,
                        dataSize,
                        ivn,
                        nlevels,
                        FUN)
{
  # check input
  if(!sparsebnUtils::is.edgeList(edge_list)) stop("edge_list must be a edgeList object!")

  edge_list <- as.list(edge_list)
  edge_list <- lapply(edge_list, as.integer)

  maxdeg <- max(sapply(edge_list, length))
  maxdeg <- as.integer(maxdeg)

  node <- length(edge_list)
  node<- as.integer(node)

  ordex <- sapply(edge_list, function(x, maxdeg){
    as.integer(c(x, rep(0, maxdeg-length(x))))
  }, maxdeg)

  V <- as.character(1:node)
  names(edge_list) <- V
  dag_graphNEL <- graph::graphNEL(node = V, edgeL = edge_list, edgemode = "directed")
  ts <- as.integer(rev(RBGL::tsort(dag_graphNEL)))

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
  if(sum(nlevels<1)) stop("n_levels must be a vector of positive integers!")
  if(length(nlevels)!=node) stop("length of n_levels not compatible with edge_list!")
  nlevels <- as.integer(nlevels)

  if(is.null(FUN)) {
    coef = 2
  }
  else
  {
    FUN <- match.fun(FUN)
    coef <- FUN(n=1)
  }

  # call DatGen
  DatGen_cpp(maxdeg, node, ordex, ts, dataSize, ivn, nlevels, coef)
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
                   coef)
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
  if(!is.numeric(coef)) stop("coef must be a scaler!")

  # call function from cpp
  DatGen(maxdeg, node, ordex, ts, dataSize, ivn, nlevels, coef)
}
