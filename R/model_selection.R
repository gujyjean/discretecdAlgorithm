# a function that calculate log-likelihood of a single graph
# parents is an edgeList object
# dat is a data.frame
multinom_loglikelihood <- function(parents,
                                   data
) {
  # check parents
  if(!sparsebnUtils::is.edgeList(parents)) stop("parents should be a edgeList object!")

  # check data
  if(!is.data.frame(data)) stop("dat should be a data.frame object!")

  # data <- as.data.frame(data)
  n_levels <- unlist(sparsebnUtils::auto_count_levels(data))

  node <- ncol(data)
  # check that the number of node and the what has been input in parents are consistent
  if (length(parents) != ncol(data)) {stop(sprintf("Incompatible graph and data! Data has %d columns but graph has %d nodes.", ncol(data), length(parents)))}

  # factorize each observation
  for (i in 1:node){
    data[,i] <- factor(data[,i])
  }

  # subtract dependent and independent variables for each regression
  loglikelihood <- 0
  loglikelihood_path <- rep(0, node)
  for (i in 1:node){
    x_ind <- parents[[i]] # if inputs are only edgeList object
    if (length(x_ind)!=0) { # do nothing if a node has no parents
      sub_dat <- cbind(data[, c(i, x_ind)])
      fu <- stats::as.formula(paste0(colnames(sub_dat)[1], "~", paste(colnames(sub_dat)[c(-1)], collapse = "+")))
      fit <- nnet::multinom(fu, data = sub_dat, trace = FALSE)
      loglikelihood_path[i] <- as.numeric(stats::logLik(fit))
      loglikelihood <- loglikelihood + as.numeric(stats::logLik(fit))
    }
    else {
      sub_dat <- data[, i, drop = FALSE]
      fu <- stats::as.formula(paste0(colnames(sub_dat)[1], "~ 1"))
      fit <- nnet::multinom(fu, data = sub_dat, trace = FALSE)
      # loglikelihood_path[i] <- as.numeric(stats::logLik(fit))
      loglikelihood <- loglikelihood + as.numeric(stats::logLik(fit))
    }
  }
  return(loglikelihood)
}

#' discrete_model_selection
#'
#' Choose the best DAG model according to the criterion described in \href{https://arxiv.org/pdf/1403.2310.pdf}{Gu, Fu and Zhou (2016)}
#' (Section 3.4).
#'
#' @param dag_path \code{\link{sparsebnPath}} object.
#' @param datbn    \code{\link{sparsebnData}} containing the original data.
#' @param alpha    tuning parameter for selection between 0 and 1, default value is 0.3.
#' @return A scaler, indicating which lambda is chosen
#'
#' @export
discrete_model_selection <- function(dag_path, datbn, alpha = 0.3) {
  # check dag_path
  if(!sparsebnUtils::is.sparsebnPath(dag_path)) stop("dag_path must be a sparsebnPath object!")
  if(length(dag_path)==1) return(1)
  # check dat
  if(!sparsebnUtils::is.sparsebnData(datbn)) stop("dat should be a sparsebnData object!")
  dat <- datbn$data

  edge_path <- lapply(dag_path, function(x){x$edges})
  ll_path <- sapply(edge_path, function(x, dat){multinom_loglikelihood(x, dat)}, dat)
  n_edge_path <- sapply(dag_path, function(x){x$nedge})
  bool_index <- c(TRUE, diff(n_edge_path)!=0)
  unique_edge_path <- n_edge_path[bool_index]
  unique_ll_path <- ll_path[bool_index]
  index_candidate <- 1:length(dag_path)
  index_candidate <- index_candidate[bool_index]
  diff_ll <- diff(unique_ll_path)
  diff_edge <- diff(unique_edge_path)

  ratio <- diff_ll/diff_edge
  threshold <- alpha*max(ratio)
  choice <- index_candidate[utils::tail(which(ratio>threshold), 1)+1]

  return(choice)
}

