make_binary_model <- function(n_tr, n_te, d, prob, snr, type) {
  set.seed(123)
  # d is the depth of the perfect binary tree
  # i.e., there are p = 2^(d + 1) - 1 nodes in the tree
  # which is the number of main effects.
  btree <- make_perfect_binary_tree(depth = d)
  p <- nrow(btree)
  q <- (p^2 + p) / 2
  leaf_nodes <- btree[btree$Depth == max(btree$Depth), ]$NodeId
  
  x <- matrix(NA, nrow = n_tr + n_te, ncol = p)
  n_leaf_node <- length(leaf_nodes)
  leaf_idx <- which(btree$NodeId %in% leaf_nodes)
  for(i in seq(nrow(x))){
    # make sure that every time values in tree nodes are zero
    btree$Value <- 0
    # randomly draw 0, 1 values in leaf nodes
    btree[leaf_idx, ]$Value <- rbinom(n = n_leaf_node, size = 1, prob = prob)
    btree <- compute_nonleaf_node_value(btree = btree)
    x[i, ] <- btree$Value
  }
  
  # compact is a matrix of coefficient in compact form
  # the first column is the first index of the interaction, and
  # the second column is the second index of the interaction
  # the third column is the values of coefficients
                   
  if(type == "main"){
    # most of the interactions are made by a pair of main effects
    # where one main effect is ancester/descendant of the other
    #compact_it <- rbind(c(4, 16), c(5, 22), c(26, 53), c(6, 12), c(13, 14))
    compact_it <- rbind(c(4, 16), c(5, 22), c(13, 14))
    ga <- rep(3, nrow(compact_it))
    compact_it <- cbind(compact_it, ga)
  }
  else if(type == "middle"){
    # half of the interactions are made by a pair of main effects
    # where one main effect is ancestor/descendant of the other
    # and half of the interactions are made by a pair of main effects
    # that are in different sub-trees
    #compact_it <- rbind(c(8, 17), c(10, 21), c(3, 50), c(26, 31), c(7, 17), c(32, 33))
    compact_it <- rbind(c(8, 17), c(10, 21), c(26, 31), c(7, 17))
    ga <- rep(3, nrow(compact_it))
    compact_it <- cbind(compact_it, ga)
  }
  else{
    # all interactions are made by a pair of main effects
    # that are in different sub-trees
    #compact_it <- rbind(c(2, 5), c(4, 7), c(9, 22), c(26, 31), c(7, 17), c(32, 33), c(61, 62))
    compact_it <- rbind(c(2, 5), c(4, 7), c(9, 22), c(61, 62))
    ga <- rep(3, nrow(compact_it))
    compact_it <- cbind(compact_it, ga)
  }
  
  pos <- list()
  pos[[2]] <- integer(0)
  pos[[3]] <- sprintr::extract_inter_indices(compact_it[, 1:2], p) - p

  gamma <- rep(0, q)
  gamma[pos[[3]]] <- ga
  
  # compute population signals
  # grab Monte-Carlo estimate of population covariances
  Sigma <- parameter$Sigma
  Phi <- parameter$Phi
  
  # vartheta
  vartheta <- as.numeric(solve(Sigma, Phi[, pos[[3]]] %*% gamma[pos[[3]]]))
  
  # specifying main effects
  # we define main effects to "counter" those from vartheta in order to control mir
  # and we make theta to be sparse
  num_main <- 5 
  oo <- order(abs(vartheta), decreasing = TRUE)
  pos[[1]] <- oo
  beta <- rep(0, p)
  beta[tail(oo, -num_main)] <- -vartheta[tail(oo, -num_main)]
  if(type == "main"){
    # there are a pair of independent main effects 
    beta[head(oo, num_main)] <- -vartheta[head(oo, num_main)] * 0.5
  }
  else if(type == "middle"){
    beta[head(oo, num_main)] <- -vartheta[head(oo, num_main)] * 0.7
  }
  else{
    beta[head(oo, num_main)] <- -vartheta[head(oo, num_main)] * 0.8
  }
  
  # theta
  theta <- beta + vartheta
  
  # combine compact representation
  compact <- rbind(cbind(rep(0, length(pos[[1]])), pos[[1]], beta[pos[[1]]]), compact_it)
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  
  tt <- t(combn(p, 2))
  # training signals
  x_tr <- x[1:n_tr, ]
  z_tr <- cbind(x_tr^2, x_tr[, tt[, 1]] * x_tr[, tt[, 2]])
  w_tr <- z_tr - x_tr %*% solve(Sigma) %*% Phi
  
  signal_beta <- as.numeric(x_tr[, pos[[1]]] %*% beta[pos[[1]]])
  signal_gamma <- as.numeric(z_tr[, pos[[3]]] %*% gamma[pos[[3]]])
  signal_main <- as.numeric(x_tr %*% theta)
  signal_int <- as.numeric(w_tr[, pos[[3]]] %*% gamma[pos[[3]]])
  
  # overall training signal
  mu_tr <- signal_main + signal_int 
  
  stopifnot(max(abs(mu_tr - signal_beta - signal_gamma)) < 1e-10)
  
  # main-interaction ration
  mir <- mean(signal_main^2) / mean(signal_int^2)
  
  # test x
  x_te <- x[(n_tr + 1):(n_tr + n_te), ]
  z_te <-  cbind(x_te^2, x_te[, tt[, 1]] * x_te[, tt[, 2]])
  mu_te <- as.numeric(x_te[, pos[[1]]] %*% beta[pos[[1]]]) + as.numeric(z_te[, pos[[3]]] %*% gamma[pos[[3]]])
  
  # taking snr = ||mu||^2 / (n * sigma^2)
  sigma <- sqrt(sum(mu_tr^2) / (n_tr * snr)) 
  
  # generate model
  new_model(name = type,
            label = paste(type, sprintf("d = %s, mir = %s, snr = %s, prob = %s",
                                        d, mir, snr, prob)),
            params = list(p = p, q = q, d = d, n_tr = n_tr, n_te = n_te,
                          mu_tr = mu_tr, mu_te = mu_te,
                          vartheta = vartheta, theta = theta,
                          beta = beta, gamma = gamma,
                          signal_beta = signal_beta, signal_gamma = signal_gamma,
                          signal_main = signal_main, signal_int = signal_int,
                          snr = snr, sigma = sigma, mir = mir,
                          x_tr = x_tr, x_te = x_te,
                          pos = pos, compact = compact),
            simulate = function(n_tr, mu_tr, sigma, nsim) {
              # this function must return a list of length nsim
              y <- mu_tr + sigma * matrix(rnorm(nsim * n_tr), n_tr, nsim)
              return(split(y, col(y)))
            })
}