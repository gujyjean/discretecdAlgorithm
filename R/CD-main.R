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
#' @param indata A sparsebnData object
#' @param n_levels A vector indicating number of levels for each variable
#' @param eor Active set
#' @param weights weight matrix
#' @return A matrix, consist of all the graphs along the solution path.
#' @export
CD.run <- function(indata, n_levels, eor = NULL,
                   weights = NULL, fmlam=0.1, nlam=30,
                   eps=0.0001, convLb=0.01, qtol = 0.0001,
                   gamma=1.0, upperbound = 100.0) {

  CD_call(indata = indata, n_levels = n_levels,
          eor = eor, weights = weights, fmlam = fmlam,
          nlam = nlam, eps = eps, convLb = convLb,
          qtol = qtol, gamma = gamma, upperbound = upperbound)

}

# Convert input to the right form.
CD_call <- function(indata, n_levels, eor, weights, fmlam, nlam, eps, convLb, qtol, gamma, upperbound) {

  # Allow users to input a data.frame, but kindly warn them about doing this.
  # if the input is a dataframe, the data set is treated as an observational data set.
  # ivn will be initialized to be a list of length dataSize, and every element is 0.
  if(is.data.frame(indata)){
    warning(sparsebnUtils::alg_input_data_frame())
    ivn <- vector("list", length = dataSize)
    ivn <- lapply(ivn, function(x){
      return(c(0L))
    })
    data <- sparsebnUtils::sparsebnData(indata, ivn, type = "discrete")
  }
  else {
    data <- indata
  }

  # Check data format
  if(!sparsebnUtils::is.sparsebnData(data)) stop(sparsebnUtils::input_not_sparsebnData(data))

  # Extract the data and the intervention list.
  data_matrix <- data$data
  data_ivn <- data$ivn

  # Get the dimensions of the data matrix
  dataSize <- nrow(data_matrix)
  node <- ncol(data_matrix)

  # the input data_matrix should be a matrix
  data_matrix <- as.matrix(data_matrix)

  # check n_levels.
  if (length(n_levels) != node) {
    stop("Length of n_levels does not compatible with the input data set. Length of nlevels should be the number of variables.")
  }
  # element of n_levels should be integer.
  n_levels <- as.integer(n_levels)

  # get observational index (obsIndex_R) from interventional list (ivn)
  obsIndex_R <- get_obsIndex(data_ivn, node)

  # make sure that for each observation, at least on node is not under intervention. If all nodes are under intervention, stop and require user to remove that observation.
  ind <- 1:node
  is_obs_zero <- sapply(obsIndex_R, function(x){length(x)==0})
  if(length(ind[is_obs_zero])!=0) {
    stop(sprintf("%d th node has been intervened in all observations, remove this node \n", ind[is_obs_zero]))
  }

  # minus 1 from all elements in obsIndex_R to incorporate with C++.
  obsIndex_R <- lapply(obsIndex_R, function(x) {as.integer(x-1)})

  # check/generate eor and eor_nr
  if(is.null(eor)) {
    eor_nr <- node*(node-1)/2
    eor <- matrix(0, nrow=eor_nr, ncol=2)
    cnt1=1
    for (i in 1:(node-1)) {
      for (j in (i+1):node) {
        eor[cnt1, 1] = i;
        eor[cnt1, 2] = j;
        cnt1 = cnt1+1;
      }
    }
  }

  eor_nr <- as.integer(eor_nr)
  eor <- matrix(as.integer(eor), ncol = 2)

  # check/generate weight matrix
  if(is.null(weights)) {
    weights <- matrix(1, node, node)
  }

  # type conversion for tunning parameters
  node = as.integer(node)
  dataSize = as.integer(dataSize)
  fmlam = as.numeric(fmlam)
  nlam = as.integer(nlam)
  eps = as.numeric(eps)
  convLb = as.numeric(convLb)
  qtol = as.numeric(qtol)
  gamma = as.numeric(gamma)
  upperbound = as.numeric(upperbound)

  # run CD algorithm
  estimate <- CD_path(node, dataSize, data_matrix,
                 n_levels, obsIndex_R, eor_nr,
                 eor, fmlam, nlam,
                 eps, convLb, qtol,
                 weights, gamma, upperbound)

  # extract lambdas
  lambda <- estimate$lambdas
  # extract adjacency matrix
  estimateG <- estimate$estimateG
  # timing data is not available yet. Fill in NA tempararily.
  time = rep(NA, nlam)

  # convert each element in fit to sparsebnFit object
  fit <- get.edgeList(estimateG, dataSize, lambda, time)

  # delete null graphs along the solution path due to upper bound on the number of edges. Now the upper bound is fixed, will let the user to decide the number of maximum number of edges in the future
  if_remove <- sapply(fit, function(x) {x$nedge == 0})
  fit[if_remove] = NULL
  # convert element of fit to sparsebnFit object
  fit <- lapply(fit, sparsebnUtils::sparsebnFit)

  # convert fit to sparsebnPath object
  fit <- sparsebnUtils::sparsebnPath(fit)

  return(fit)
}

# a function that directly calls from cpp
# type check, no converting type of an input at this point
CD_path <- function(node, dataSize, data_matrix,
                    n_levels, obsIndex_R, eor_nr,
                    eor, fmlam, nlam,
                    eps, convLb, qtol,
                    weights, gamma, upperbound) {
  # check node parameter
  if(!is.integer(node)) stop("node must be a integer!")
  if(node <= 0) stop("node must be a positive integer!")

  # check dataSize parameter
  if(!is.integer(dataSize)) stop("dataSize must be a integer!")
  if(dataSize <= 0) stop("dataSize must be a positive integer!")

  # check data_matrix
  if(node!=ncol(data_matrix) || dataSize!=nrow(data_matrix)) stop("dimension does not match. node should be the number of columns of data matrix, and dataSize should be numbe of rows of data matrix.")

  # check n_levels
  if (!is.integer(n_levels)) stop("n_levels must be a vector of integers!")

  # check obsIndex_R
  if (!is.list(obsIndex_R)) stop("obsIndex_R must be a list!")
  if (sum(sapply(obsIndex_R, function(x){!is.integer(x)}))!=0) stop("element of obsIndex_R must be a vector of integers!")

  # check eor_nr and eor
  if (!is.integer(eor)) stop("eor must be a vector of integers!")
  if (nrow(eor)==0) stop("eor cannot be empty!")
  if (!is.integer(eor_nr)) stop("eor_nr must be an integer!")
  if (eor_nr != nrow(eor)) stop("eor_nr must be the numebr of rows of eor!")

  # check fmlam
  if (!is.numeric(fmlam)) stop("fmlam must be a numeric number!")
  if (fmlam<0) stop("fmlam cannot be a negative number!")
  if (fmlam>1) stop("fmlam cannot be bigger than 1!")

  # check nlam
  if(!is.integer(nlam)) stop("nlam must be an integer!")
  if(nlam<=0) stop("nlam must be a positive integer!")

  # check eps
  if(!is.numeric(eps)) stop("eps must be a numeric number!")
  if(eps<=0) stop("eps must be a positive number!")

  # check convLb
  if (!is.numeric(convLb)) stop("convLb must be a numeric number!")
  if (convLb<=0) stop("convLb must be a positive number!")

  # check qtol
  if (!is.numeric(qtol)) stop("qtol must be a numeric number!")
  if (qtol<=0) stop("qtol must be a positive number!")

  # check weights
  if (!is.numeric(weights)) stop("weights must be a numeric matrix!")
  if (ncol(weights)!=node || nrow(weights)!=node) stop("weigths must be a squred matrix with both number of rows and columns equal to number of variables!")

  # check gamma
  if (!is.numeric(gamma)) stop("gamma must be a numeric number!")
  if (gamma<=0) stop ("gamma must be a positive number!")

  # check upperbound
  if (!is.numeric(upperbound)) stop("upperbound must be a numeric number!")
  if (upperbound <= 0 && upperbound!=-1) stop("upperbound must be a large positive integer to truncate the adaptive weights. Or it can be -1, which indicates that there is no truncation!")

  CD.out <- CD(node, dataSize, data_matrix,
               n_levels, obsIndex_R, eor_nr,
               eor, fmlam, nlam,
               eps, convLb, qtol,
               weights, gamma, upperbound)

  return(CD.out)
}

