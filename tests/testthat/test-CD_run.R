context("cd.run")

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
test_that("Testing default behaviour of cd.run", {
  final <- cd.run(databn_obs)
  # n_length <- length(final)
  # print(n_length)
  # print(final[[n_length]]$edges)
  # print(final[[n_length]]$lambda)
  # print(final[[n_length]]$nedge)

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

test_that("Testing cd.run with manual settings", {
  weights <- matrix(1.5, nrow=node, ncol=node)
  final <- cd.run(databn_obs, weights=weights, lambdas=NULL, lambdas.length=10, error.tol=0.0003, convLb=0.02, weight.scale=1.5, upperbound=300.0)

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

test_that("Testing cd.run with intervention", {
  final <- cd.run(databn_int)
  # n_length <- length(final)
  # print(n_length)
  # print(final[[n_length]]$edges)
  # print(final[[n_length]]$lambda)
  # print(final[[n_length]]$nedge)

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
