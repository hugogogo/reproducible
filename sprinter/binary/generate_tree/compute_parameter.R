rm(list = ls())
source("./binary_tree.R")
## @knitr models
compute_parameter <- function(d, prob, num_rep) {
  # d is the depth of the perfect binary tree
  # i.e., there are p = 2^(d + 1) - 1 nodes in the tree
  # which is the number of main effects.
  btree <- make_perfect_binary_tree(depth = d)
  p <- nrow(btree)
  q <- (p^2 + p) / 2
  leaf_nodes <- btree[btree$Depth == max(btree$Depth), ]$NodeId
  btree_avg <- btree
  btree_avg$Value <- 0
  
  idx <- rbind(cbind(seq(p), seq(p)), t(combn(p, 2)))
  # to get W
  # we need to know Cov(X) and Cov(X, Z)
  # hard to get explicit formula for non-Gaussian
  # we will use large-sample average
  xx <- matrix(NA, nrow = num_rep, ncol = p)
  zz <- matrix(NA, nrow = num_rep, ncol = q)
  n_leaf_node <- length(leaf_nodes)
  leaf_idx <- which(btree$NodeId %in% leaf_nodes)
  for(i in seq(num_rep)){
    if (i %% 100 == 0)
      print(paste0(i, "->"))
    # make sure that every time values in tree nodes are zero
    btree$Value <- 0
    # randomly draw 0, 1 values in leaf nodes
    btree[leaf_idx, ]$Value <- rbinom(n = n_leaf_node, size = 1, prob = prob)
    btree <- compute_nonleaf_node_value(btree = btree)
    xx[i, ] <- btree$Value
    zz[i, ] <- xx[i, idx[, 1]] * xx[i, idx[, 2]]
    btree_avg$Value <- btree_avg$Value + btree$Value
  }
  btree_avg$Value <- btree_avg$Value / num_rep
  
  # true covariance among x
  Sigma <- cov(xx)
  # true covariance between x and z(sub I)
  Phi <- cov(xx, zz)
  
  parameter <- list(Sigma = Sigma, Phi = Phi, btree_avg = btree_avg)
  file_name <- paste0("parameter_d", d, ".Rdata", sep = "")
  save(parameter, file = file_name)
  return(parameter)
}

set.seed(123)
#compute_parameter(d = 5, prob = 0.1, num_rep = 10000)
compute_parameter(d = 6, prob = 0.05, num_rep = 10000)