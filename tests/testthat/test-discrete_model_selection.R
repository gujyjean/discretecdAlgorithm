context("discrete_model_selection")

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

# test
test_that("Check discrete_model_selection runs as expected", {
  expect_error(discrete_model_selection(dag_path = fit_path, datbn = databn_obs), NA)
  expect_error(discrete_model_selection(dag_path = fit_path, datbn = databn_obs, alpha = 0.1), NA)

})

test_that("Check input dag_path", {
  ### Throw error if dag_path is not a sparsebnPath object
  expect_error(discrete_model_selection(dag_path = fit_path[[10]], datbn = databn_obs, alpha = 0.1))

  ### Run as expected when dag_path only has one graph
  expect_error(discrete_model_selection(dag_path = fit_path[10], datbn = databn_obs, alpha = 0.1), NA)

})

test_that("Check input datbn", {
  ### Throw error if datbn is not a sparsebnData object
  expect_error(discrete_model_selection(dag_path = fit_path, datbn = data, alpha = 0.1))

})

test_that("Check output", {
  ### Check algorithm work when input path ony has one element
  out_one <- discrete_model_selection(dag_path = fit_path[10], datbn = databn_obs, alpha = 0.1)
  expect_equal(out_one, 1)
})
