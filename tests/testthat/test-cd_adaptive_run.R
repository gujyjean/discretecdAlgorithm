context("cd_adaptive_run")

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
ivn_int <- lapply(1:dataSize, function(x){return(as.integer(x/6))})
databn_obs <- sparsebnUtils::sparsebnData(data, ivn = ivn_obs, type = "discrete")
databn_int <- sparsebnUtils::sparsebnData(data, ivn = ivn_int, type = "discrete")

# test
test_that("Testing cd_adaptive_run with observation data ", {
  weights <- matrix(1, nrow=node, ncol=node)
  final <- cd_adaptive_run(databn_obs, eor = NULL, weights=weights, lambda_seq=NULL, fmlam = 0.01, nlam = 10, eps=0.0001, convLb=0.02, qtol = 0.00001, gamma=1, upperbound=100.0, adaptive = TRUE)

  ### check output type
  expect_is(final, "list")
  expect_is(final, "sparsebnPath")

  ### check element type of final
  for(i in seq_along(final)){
    expect_is(final[[i]], "sparsebnFit")
  }

  ### Check consistency of nedge
  for(i in seq_along(final)){
    matrix.nedge <- sum(sparsebnUtils::get.adjacency.matrix(final[[i]]$edges))
    edgeL.nedge <- sparsebnUtils::num.edges(final[[i]]$edges)
    expect_equal(final[[i]]$nedge, edgeL.nedge, matrix.nedge)
  }
})


