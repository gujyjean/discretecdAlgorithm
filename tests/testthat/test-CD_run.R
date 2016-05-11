context("CD.run")

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
nlevels <- rep(2, node)
ivn <- vector("list", length = dataSize)
ivn <- lapply(ivn, function(x){return(as.integer(0))})
databn <- sparsebnUtils::sparsebnData(data, ivn, type = "discrete")

# test
test_that("Testing default behaviour of CD.run", {
  final <- CD.run(databn, nlevels)
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

test_that("Testing ccdr.run with manual settings", {
  weights <- matrix(1.5, nrow=node, ncol=node)
  final <- CD.run(databn, nlevels, weights = weights, minlam_over_maxlam =0.3, lambdas.length=10, error.tol=0.0003, convLb=0.02, gamma=1.5, upperbound = 300.0)

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
