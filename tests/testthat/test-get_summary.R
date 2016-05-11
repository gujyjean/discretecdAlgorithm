context("get.summary")

# set up input of function get.summary
adj_matrix <- matrix(c(0, 1, 1, 0,
                       0, 0, 1, 0,
                       0, 0, 0, 1,
                       0, 0, 0, 0), byrow = TRUE, ncol = 4)
dataSize = 50
lambda = 3.5
time = 1

# test get.summary
test_that("input adjacency matrix can be converted to the right edge list.", {
  ### Trivial case
  adj_matrix_trivial <- matrix(rep(0, 16), nrow = 4)
  expect_equal(get.summary(adj_matrix_trivial, dataSize, lambda, time)$nedge, 0)
  expect_equal(get.summary(adj_matrix_trivial, dataSize, lambda, time)$edges, sparsebnUtils::edgeList(vector("list", length = 4)))

  ### Non-trivil case
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time)$nedge, 4)
  true_edgeList <- vector("list", length = 4)
  true_edgeList[[1]] = which(adj_matrix == 2)
  true_edgeList[[2]] = 1
  true_edgeList[[3]] = c(1, 2)
  true_edgeList[[4]] = 3
  true_edgeList <- sparsebnUtils::edgeList(true_edgeList)
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time)$edges, true_edgeList)
})

test_that("output the right lambda.", {
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time)$lambda, 3.5)
})

test_that("output the right time.", {
  ### Trivial case
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time=NA)$time, NA)

  ### Non-trivial case
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time)$time, 1)
})

test_that("output the rigth nn.", {
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time)$nn, 50)
})

test_that("output the right pp.", {
  expect_equal(get.summary(adj_matrix, dataSize, lambda, time)$pp, 4)
})
