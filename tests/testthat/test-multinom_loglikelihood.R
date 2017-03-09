context("multinom_loglikelihood")

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
ivn_obs <- lapply(ivn, function(x){return(as.integer(0))})
databn_obs <- sparsebnUtils::sparsebnData(data, ivn = ivn_obs, type = "discrete")
fit_path <- cd.run(databn_obs)
parent <- fit_path[[length(fit_path)]]$edges

# test
test_that("multinom_loglikelihood run as expected", {
  expect_error(multinom_loglikelihood(parents = parent, data = databn_obs$data), NA)
})

test_that("Check input parent", {
  ### Throw error if parent is not edgeList object
  parent_list <- as.list(parent)
  expect_error(multinom_loglikelihood(parents = parent_list, data = databn_obs$data))

})

test_that("Check input data", {
  ### Throw error if data is not data.frame
  expect_error(multinom_loglikelihood(parents = parent_list, data = data))

  ### Throw error if data is not compatible with parent
  parent_short <- parent[1:5]
  expect_error(multinom_loglikelihood(parents = parent_short, data = data))

})


