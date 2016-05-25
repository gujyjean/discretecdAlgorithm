context("gen_lambdas")

max <- 5
min <- 0.3
length <- 15

lambda_seq <- gen_lambdas(max, min, length)

test_that("If gen_lambdas output the correct number of lambds",  {
  expect_equal(length(lambda_seq), length)
})

test_that("If the output of gen_lambdas has the right maxmimum lambda", {
  expect_equal(lambda_seq[1], max)
})

test_that("If the output of gen_lambdas has the right minimum lambda", {
  expect_equal(lambda_seq[length], min)
})
