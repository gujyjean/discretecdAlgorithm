context("CD_call")

# set up input variable
data <- matrix(c(1, 1, 0, 0, 1, 1,
                 1, 1, 0, 1, 1, 1,
                 0, 0, 1, 0, 0, 1,
                 0, 0, 1, 0, 0, 1,
                 0, 0, 0, 1, 1, 0,
                 0, 0, 0, 1, 1, 1,
                 1, 1, 1, 1, 0, 0,
                 1, 0, 1, 1, 0, 1,
                 0, 0, 0, 0, 1, 0,
                 1, 1, 1, 1, 0, 1,
                 1, 1, 0, 1, 1, 1,
                 0, 0, 1, 0, 0, 1,
                 1, 1, 0, 1, 0, 0,
                 1, 0, 1, 1, 0, 1,
                 1, 1, 1, 1, 1, 0,
                 1, 0, 1, 1, 1, 1,
                 0, 0, 1, 0, 0, 0,
                 1, 1, 0, 1, 1, 1,
                 1, 1, 1, 0, 0, 0,
                 1, 1, 1, 1, 0, 0,
                 0, 0, 0, 0, 1, 0,
                 0, 0, 1, 0, 0, 0,
                 1, 0, 0, 0, 1, 1,
                 0, 0, 1, 0, 0, 0,
                 1, 0, 1, 1, 0, 1,
                 0, 0, 0, 1, 1, 0,
                 0, 0, 0, 0, 0, 0,
                 0, 0, 1, 0, 1, 0,
                 0, 0, 1, 0, 0, 0,
                 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 6)
dataSize <- dim(data)[1]
node <- dim(data)[2]
ivn <- vector("list", length = dataSize)
ivn <- lapply(ivn, function(x){return(as.integer(0))})
databn <- sparsebnUtils::sparsebnData(data, ivn = ivn, type = "discrete")

# test
test_that("CD_call runs as expected", {
  ### no error
  expect_error(CD_call(databn, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3), NA)
})

test_that("Check input for indata", {
  ### input with data.frame object
  data_frame_obj <- as.data.frame(data)
  expect_warning(CD_call(data_frame_obj, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))

  ### input wrong indata should throw error
  data_list <- as.list(data)
  expect_error(CD_call(indata = data_list, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))

  data_oneLevel <- databn
  data_oneLevel$data[, 1] <- rep(0, nrow(data))
  expect_error(CD_call(indata = data_oneLevel, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))
})

test_that("Check input ivn", {
  ### Input ivn
  ivn_list <- vector("list", length = dataSize)
  ivn_list <- lapply(1:length(ivn_list), function(x){return(as.integer(x%%7))})
  databn_ivn <- sparsebnUtils::sparsebnData(data, ivn = ivn_list, type = "discrete")
  expect_error(CD_call(databn_ivn, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3), NA)

  ### Throw error when length of ivn list is wrong
  ivn_list <- vector("list", length = (dataSize+10))
  ivn_list <- lapply(1:length(ivn_list), function(x){return(as.integer(x%%7))})
  databn_ivn <- sparsebnUtils::sparsebnData(data, ivn = ivn_list, type = "discrete")
  expect_error(CD_call(databn_ivn, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))

  ### Works if ivn list contains NA
  ivn_list <- vector("list", length = dataSize)
  ivn_list <- lapply(1:length(ivn_list), function(x){return(ifelse(!x%%7, NA, as.integer(x%%7)))})
  databn_ivn <- sparsebnUtils::sparsebnData(data, ivn = ivn_list, type = "discrete")
  expect_error(CD_call(databn_ivn, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3), NA)

  ### Throw error if ivn list indicate intervention on a node for every observation
  ivn_list <- vector("list", length = dataSize)
  ivn_list <- lapply(1:length(ivn_list), function(x){return(as.integer(c(x%%6, 6)))})
  databn_ivn <- sparsebnUtils::sparsebnData(data, ivn = ivn_list, type = "discrete")
  expect_error(CD_call(databn_ivn, eor = NULL, permute = TRUE, weights = NULL, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))

})

test_that("Check input weights", {
  ### Throw error if weight matrix is not a square matrix
  weights_notSquare <- matrix(1, node, node+1)
  expect_error(CD_call(databn, eor = NULL, permute = TRUE, weights = weights_notSquare, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))

  ### Throw error if weight matrix has wrong dimension
  weight_wrongDim <- matrix(1, node+1, node+1)
  expect_error(CD_call(databn, eor = NULL, permute = TRUE, weights = weights_wrongDim, lambda_seq = NULL, fmlam = 0.1, nlam = 30, eps = 0.0001, convLb = 0.01, qtol = 0.0001, gamma = 1, upperbound = 100, threshold = 3))

})


