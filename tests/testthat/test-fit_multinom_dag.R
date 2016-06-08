context("fit_multinom_dag")

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
n_levels <- rep(2, node)
ivn <- vector("list", length = dataSize)
ivn <- lapply(ivn, function(x){return(as.integer(0))})
databn <- sparsebnUtils::sparsebnData(data, ivn = ivn, type = "discrete")

final <- cd.run(databn)
final.dag <- final[[length(final)]]
edge_list<- final.dag$edges
adj_matrix <- sparsebnUtils::get.adjacency.matrix(final.dag)
matrix <- matrix(adj_matrix, nrow = 6)

# test
test_that("fit_multinom_dag can take empty graphs", {
  empty_graph <- final[[1]]
  expect_error(fit_multinom_dag(empty_graph, n_levels, data), NA)

  ### test if I input a graph with a single node, will the algorithm work.
  data_single <- as.data.frame(matrix(c(0, 1, 2, 0, 0, 3, 3, 2, 2, 1), nrow=10))
  ### Generate fixed objects for empty graphs
  generate_empty_edgeList <- function(){
    sparsebnUtils::edgeList(list(integer(0)))
  }
  generate_empty_sparsebnFit <- function(){
    li <- list(edges = generate_empty_edgeList(), lambda = 1, nedge = 0, pp = 1, nn = 10, time = 1)
    sparsebnUtils::sparsebnFit(li)
  }
  single_node <- generate_empty_sparsebnFit()
  expect_error(fit_multinom_dag(single_node, n_levels=4, data_single), NA)
})

test_that("fit_multinom_dag can run with different types of input", {
  ### fit_multinom_dag can accept a sparsebnFit object as an input
  expect_error(fit_multinom_dag(final.dag, n_levels, data), NA)
  ### fit_multinom_dag can accept an edgeList object as an input
  expect_error(fit_multinom_dag(edge_list, n_levels, data), NA)
  ### fit_multinom_dag can accept an adjacency matrix as an input
  expect_error(fit_multinom_dag(matrix, n_levels, data), NA)
  ### fit_multinom_dag can accept an adjacency matrix of class dgCMatrix as an input
  expect_error(fit_multinom_dag(adj_matrix, n_levels, data), NA)

  adj_list <- apply(matrix, 1, function(x){list(x)})
  ### throw an error if fit_multinom_dag has the wrong input
  expect_error(fit_multinom_dag(adj_list, n_levels, data))

  ### four outputs should all be same
  out_sparsebnFit <- fit_multinom_dag(final.dag, n_levels, data)
  out_edgeList <- fit_multinom_dag(edge_list, n_levels, data)
  out_matrix <- fit_multinom_dag(matrix, n_levels, data)
  out_adjMatrix <- fit_multinom_dag(adj_matrix, n_levels, data)

  expect_equal(out_sparsebnFit, out_edgeList)
  expect_equal(out_edgeList, out_matrix)
  expect_equal(out_matrix, out_adjMatrix)
})

test_that("fit_multinom_dag output the right result", {
  out <- fit_multinom_dag(final.dag, n_levels, data)

  ### length of output should be the number of variables
  expect_equal(length(out), node)
  ### legnth of each element should be the numeber of parents
  for (i in 1:node) {
    expect_equal(length(out[[i]]), sum(matrix[, i])+(sum(matrix[, i])!=0))
  }
  ### randomly check some entries
  expect_equal(dim(out[[2]][[1]]$coef), c(1, 1))
  expect_equal(out[[2]][[1]]$parent, 1)
  expect_equal(dim(out[[2]][[2]]$coef), c(1, 1))
  expect_equal(out[[2]][[2]]$parent, 3)
})
