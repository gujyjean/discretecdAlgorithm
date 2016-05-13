context("CD_path")

# set up input variable
data_matrix <- matrix(c(1, 1, 0, 0, 1, 1,
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
data_matrix <- matrix(as.integer(data_matrix), ncol = 6)
node = ncol(data_matrix); node <- as.integer(node)
dataSize = nrow(data_matrix); dataSize <- as.integer(dataSize)
n_levels <- rep(2, node)
n_levels <- as.integer(n_levels)
obs <- 1:30
obsIndex_R <- vector("list", length = 6)
obsIndex_R <- lapply(obsIndex_R, function(x, obs){as.integer(obs-1)}, obs)
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
eor_nr <- as.integer(eor_nr)
eor <- matrix(as.integer(eor), ncol = 2)
fmlam = 0.1; fmlam <- as.numeric(fmlam)
nlam = 30; nlam = as.integer(nlam)
eps = 0.0001; eps <- as.numeric(eps)
convLb = 0.01; convLb <- as.numeric(convLb)
qtol= 0.0001; qtol = as.numeric(qtol)
weights = matrix(rep(1, 6*6), ncol = 6)
gamma = 1; gamma <- as.numeric(gamma)
upperbound = 100; upperbound <- as.numeric(upperbound)

# test
test_that("CD_path runs as expected", {
  ### throw error if parameter and initial values not explicitly specified
  expect_error(CD_path(node, dataSize, data_matrix, n_levels))

  ### no error
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound), NA)
})

test_that("Check input: node", {
  ### Throw an error if node is not an integer
  expect_error(CD_path(node = 6, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### Throw an error if node <= 0
  expect_error(CD_path(node = as.integer(-1), dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: dataSize", {
  ### throw an error if dataSize is not an integer
  expect_error(CD_path(node, dataSize = 30, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### Throw an error if dataSize <= 0
  expect_error(CD_path(node, dataSize = as.integer(-10), data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Ckeck input: data_matrix", {
  ### throw an error if the input data_matrix is not an integer matrix.
  data_num <- data_matrix+1
  data_num <- data_num-1
  expect_error(CD_path(node, dataSize, data_matrix = data_num, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if the input data_matrix has a wrong number of rows.
  data_wrong_dataSize <- data_matrix[1:10, ]
  expect_error(CD_path(node, dataSize, data_matrix = data_wrong_dataSize, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if the input data_matrix has the wrong number of columns.
  data_wrong_node <- data_matrix[, 1:3]
  expect_error(CD_path(node, dataSize, data_matrix = data_wrong_node, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: n_levels", {
  ### throw an error if the input n_levels is not an integer vector
  expect_error(CD_path(node, dataSize, data_matrix, as.numeric(n_levels), obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if the input n_levels has the wrong dimension
  n_level_wrong_dim <- rep(2, 10)
  expect_error(CD_path(node, dataSize, data_matrix, n_levels = n_level_wrong_dim, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: obsIndex_R", {
  obsIndex_noList <- 1:100
  obsIndex_noInteger <- lapply(obsIndex_R, function(x){as.numeric(x)})
  obsIndex_negative <- obsIndex_R
  obsIndex_negative[[1]][1] = -1
  obsIndex_wrong_dim <- obsIndex_R[1:4]

  ### throw an error if obsIndex_R is not a list
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R = obsIndex_noList, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if element in obsIndex_R is not a integer vector
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R = obsIndex_noInteger, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if element in obsIndex_R is negative
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R = obsIndex_negative, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if obsIndex_R has wrong dimension
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R = obsIndex_wrong_dim, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: eor_nr and eor", {
  eor_noInteger <- as.numeric(eor)
  eor_empty <- rep()
  eor_nr_noInteger <- as.numeric(eor_nr)
  eor_nr_wrong_dim <- 4

  ### throw an error if element in eor is not integer
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor = eor_noInteger, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if eor is empty
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor = eor_empty, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if eor_nr is not integer
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr = eor_nr_noInteger, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if eor_nr does not equal to the number of row of eor
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr = eor_nr_wrong_dim, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: fmlam", {
  ### throw an error if fmlam is not numeric
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam = as.character(1), nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if fmlam is negative
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam = -0.1, nlam, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if fmlam is bigger than 1
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam = 1.5, nlam, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: nlam", {
  ### throw an error if nlam is not an integer
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam = 10, eps, convLb, qtol, weights, gamma, upperbound))

  ### throw an error if nlam is non-positive
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam = 0L, eps, convLb, qtol, weights, gamma, upperbound))
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam = -10L, eps, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: eps", {
  ### throw an error if eps is not an numeric number
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps <- as.character(0.00001), convLb, qtol, weights, gamma, upperbound))

  ### throw an error if eps is non-positive
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps <- -0.00001, convLb, qtol, weights, gamma, upperbound))
})

test_that("Check input: convLb", {
  ### throw an error if convLb is not an numeric number
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb <- as.character(0.01), qtol, weights, gamma, upperbound))

  ### throw an error if convLb is non-positive
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb <- -0.01, qtol, weights, gamma, upperbound))
})

test_that("Check input: qtol", {
  ### throw an error if qtol is not an numeric number
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol <- as.character(0.00001), weights, gamma, upperbound))

  ### throw an error if qtol is non-positive
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol <- -0.00001, weights, gamma, upperbound))
})

test_that("Check input: weights", {
  weights_noNumeric <- matrix(as.character(weights), nrow = node)
  weights_integer <- matrix(as.integer(weights), nrow = node)
  weights_wrong_dim <- matrix(1, nrow=node-1, ncol = node+1)

  ### throw an error if weights is not a numeric matrix
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights <- weights_noNumeric, gamma, upperbound))

  ### throw an error if weights is an integer matrix
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights <- weights_integer, gamma, upperbound))

  ### throw an error if weights has a wrong dimension
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights <- weights_wrong_dim, gamma, upperbound))
})

test_that("Check input: gamma", {
  ### throw an error if gamma is not a numeric number
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma <- as.character(1), upperbound))

  ### throw an error is gamma is a non-positive number
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma <- -1, upperbound))
})

test_that("Check input: upperbound", {
  ### throw an error if upperbound is not a numeric number
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound <- as.character(100)))

  ### throw an error if upperbound is negative and it is not -1
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound <- -100))

  ### if upper bound is -1, run as usual
  expect_error(CD_path(node, dataSize, data_matrix, n_levels, obsIndex_R, eor_nr, eor, fmlam, nlam, eps, convLb, qtol, weights, gamma, upperbound <- -1), NA)
})