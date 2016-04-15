# You can learn more about package authoring with RStudio at: http://r-pkgs.had.co.nz/
# Some useful keyboard shortcuts for package authoring: Build and Reload Package: 'Cmd +
# Shift + B' Check Package: 'Cmd + Shift + E' Test Package: 'Cmd + Shift + T'

# ========================================================
# Note that this is only a trial version.
# There will be no such choice that we can input the initial beta.
# No documentation yet
# ========================================================

#' @useDynLib discretecdAlgorithm
#' @importFrom Rcpp sourceCpp
NULL

# ========================================================
# The main function CD.run
# ========================================================
## will be exported

#' Learn structure of a discrete network
#'
#' @param data A data matrix
#' @param nlevels A vector indicating number of levels for each variable
#' @param obsIndex_R Observation index, a vector, has length equal to the data size. Each element indicates which variable is under intervention, if none, use 0.
#' @param eor_nr A number indicate the length of the active set
#' @param eor Active set
#' @param weights weight matrix
#' @return A matrix, consist of all the graphs along the solution path.
#' @export
CD.run <- function(indata, nlevels, eor_nr, eor, weights, fmlam=0.1, nlam=30,
                   eps=0.0001, convLb=0.01, qtol = 0.0001, gamma=1.0, upperbound = 100.0) {
  dataSize <- nrow(indata)
  node <- ncol(indata)

  if(is.data.frame(indata)){
    warning(sparsebnUtils::alg_input_data_frame())
    ivn <- vector("list", length = dataSize)
    ivn <- lapply(ivn, function(x){
      return(c(0L))
    })
    data <- sparsebnUtils::sparsebnData(indata, ivn, type = "discrete")
  }
  if(!sparsebnUtils::is.sparsebnData(data)) stop(sparsebnUtils::input_not_sparsebnData(data))
  data_matrix <- data$data
  data_ivn <- data$ivn
  # data_ivn <- lapply(data_ivn, function(x) {
  #   return(as.integer(x))
  # })
  length_ivn <- sapply(data_ivn, function(x) {
    return(length(x))
  })

  obsIndex_R <- get_obsIndex(data_ivn, node)
  ind <- 1:node
  is_obs_zero <- sapply(obsIndex_R, function(x){length(x)==0})
  if(length(ind[is_obs_zero])!=0) {
    stop(sprintf("%d th node has been intervened in all observations, remove this node \n", ind[is_obs_zero]))
  }

  obsIndex_R <- lapply(obsIndex_R, function(x) {x-1})

  estimate <- CD(as.integer(node), as.integer(dataSize),
                 as.matrix(data_matrix),
                  as.integer(nlevels), obsIndex_R,
                   as.integer(eor_nr), as.matrix(eor), as.numeric(fmlam),
                   as.integer(nlam), as.numeric(eps), as.numeric(convLb),
                   as.numeric(qtol), as.matrix(weights),
                   as.numeric(gamma), as.numeric(upperbound))

  lambda <- estimate$lambdas
  estimateG <- estimate$estimateG
  time = rep(NA, nlam)

  fit <- get.edgeList(estimateG, dataSize, lambda, time)
  # delete null graphs
  if_remove <- sapply(fit, function(x) {x$nedge == 0})
  fit[if_remove] = NULL

  fit <- lapply(fit, sparsebnUtils::sparsebnFit)
  fit <- sparsebnUtils::sparsebnPath(fit)

  return(fit)
}


