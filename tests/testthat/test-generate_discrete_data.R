context("generate_discrete_data")

# set up input variables
edge_list <- vector("list", length = 5)
edge_list[[1]] <- integer(0)
edge_list[[2]] <- integer(0)
edge_list[[3]] <- 1
edge_list[[4]] <- c(1, 3)
edge_list[[5]] <- c(1, 2)
edge_list <- sparsebnUtils::as.edgeList(edge_list)
names(edge_list) <- c("V1", "V2", "V3", "V4", "V5")
nlevels <- c(3, 5, 2, 2, 3)
data_params <- coef_gen(edge_list = edge_list, n_levels = nlevels)
ivn_list <- rep(1:5, rep(10, 5))
ivn_list <- as.list(ivn_list)
ivn_list <- lapply(ivn_list, function(x){paste0("V", x)})
n_obs <- length(ivn_list)

# # test
# test_that("generate_discrete_data runs as expected", {
#   expect_error(generate_discrete_data(graph = edge_list, params = data_params, n = n_obs, n_levels = nlevels), NA)
#
#   expect_error(generate_discrete_data(graph = edge_list, params = data_params, n = n_obs, n_levels = nlevels, ivn = ivn_list), NA)
#
# })
#
# test_that("Check output", {
#   generated_data <- generate_discrete_data(graph = edge_list, params = data_params, n = n_obs, n_levels = nlevels, ivn = ivn_list)
#   expect_equal(class(generated_data), "matrix")
#   expect_equal(nrow(generated_data), n_obs)
#   expect_equal(ncol(generated_data), length(edge_list))
# })
