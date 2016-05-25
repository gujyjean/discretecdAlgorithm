context("generate.lambdas")

max <- 5
ratio <- 0.01
length <- 10

test_that("generate.lambdas works fine with default initiation", {
  lambda_seq <- generate.lambdas(max)
  ### the output sequence has the right length
  expect_equal(length(lambda_seq), 30)
  ### the output sequence has the right max and min lambda
  expect_equal(lambda_seq[1], max)
  expect_equal(lambda_seq[30]/lambda_seq[1], 0.01)
})

test_that("generate.lambdas works fine with mannual settings", {
  lambda_seq <- generate.lambdas(max, ratio, length)
  ### the output sequence has the right length
  expect_equal(length(lambda_seq), length)
  ### the output sequence has the right max and min lambda
  expect_equal(lambda_seq[1], max)
  expect_equal(lambda_seq[length]/lambda_seq[1], ratio)
})
