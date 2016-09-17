#' get.adaptive.weights
#'
#' A function that will return adaptive weights, and log-likelihood of the estimation.
#'
#' This function takes a dag as an input
#'
#' @param DAG A \code{\link[sparsebnUtils]{sparsebnFit}} object.
#' @param databn A \code{\link[sparsebnUtils]{sparsebnData}} object.
#' @param error.tol Error tolerance for the algorithm, used to test for convergence.
#' @param convLb Small positive number used in Hessian approximation.
#'
#' @return A list of the adaptive weight matrix, and log-likelihood of estimation.

#' @export
get.adaptive.weights <- function(DAG,
                                 databn,
                                 error.tol=0.0001,
                                 convLb=0.01) {
  adaptiveWeights_call(DAG = DAG,
                       databn = databn,
                       eps = error.tol,
                       convLb = convLb,
                       qtol = error.tol)
}

adaptiveWeights_call <- function(DAG, databn, eps, convLb, qtol) {
  # Check data format
  if (!sparsebnUtils::is.sparsebnFit(DAG)) stop("DAG must be a sparsebnFit object!")
  if(!sparsebnUtils::is.sparsebnData(databn)) stop(sparsebnUtils::input_not_sparsebnData(data))
  if (!is.numeric(eps)) stop("error.tol must be a small numeric positive number!")
  else {
    if (eps <= 0) {
      stop("error.tol must be a positive number!")
    }
  }
  if (!is.numeric(convLb)) stop("convLb must be a numeric positive number!")
  else {
    if (convLb <= 0) {
      stop("convLb must be a positive number!")
    }
  }

  # Extract the data and the intervention list.
  data_matrix <- databn$data
  data_matrix <- as.data.frame(sapply(data_matrix, function(x){as.integer(x)}))
  data_ivn <- databn$ivn
  data_level <- databn$levels
  data_names <- names(databn$data)

  # Get the dimensions of the data matrix
  dataSize <- nrow(data_matrix)
  node <- ncol(data_matrix)

  # the input data_matrix should be a matrix
  data_matrix <- as.matrix(data_matrix)

  # get n_levels.
  n_levels <- as.integer(sapply(databn$levels, function(x){length(x)}))

  # get observational index (obsIndex_R) from interventional list (ivn)
  obsIndex_R <- get_obsIndex(data_ivn, node)

  # make sure that for each observation, at least on node is not under intervention. If all nodes are under intervention, stop and require user to remove that observation.
  ind <- 1:node
  is_obs_zero <- sapply(obsIndex_R, function(x){(length(x)==1 && x == 0)})
  if(length(ind[is_obs_zero])!=0) {
    stop(sprintf("%d th node has been intervened in all observations, remove this node \n", ind[is_obs_zero]))
  }

  # minus 1 from all elements in obsIndex_R to incorporate with C++.
  obsIndex_R <- lapply(obsIndex_R, function(x) {as.integer(x-1)})

  # generate eor and eor_nr
  edges <- DAG$edges
  parents <- unlist(edges)
  child_index <- sapply(edges, length)
  children <- rep(1:node, child_index)
  eor <- cbind(parents, children)
  colnames(eor) <- NULL
  eor_nr <- nrow(eor)

  eor_nr <- as.integer(eor_nr)
  eor <- matrix(as.integer(eor), ncol = 2)

  # type conversion for tunning parameters
  node = as.integer(node)
  dataSize = as.integer(dataSize)
  eps = as.numeric(eps)
  convLb = as.numeric(convLb)
  qtol = as.numeric(qtol)

  # run parameter reestimation function
  cd_estimate(node,
              dataSize,
              data_matrix,
              n_levels,
              obsIndex_R,
              eor_nr,
              eor,
              eps,
              convLb,
              qtol)
}

cd_estimate <- function(node,
                        dataSize,
                        data_matrix,
                        n_levels,
                        obsIndex_R,
                        eor_nr,
                        eor,
                        eps,
                        convLb,
                        qtol) {

  CD_learning(node,
              dataSize,
              data_matrix,
              n_levels,
              obsIndex_R,
              eor_nr,
              eor,
              eps,
              convLb,
              qtol)
}
