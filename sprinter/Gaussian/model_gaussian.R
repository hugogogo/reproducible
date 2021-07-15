library(mvtnorm)
library(Matrix)
## @knitr models
make_gaussian_model <- function(n_tr, n_te, p, 
                                signal_main, signal_sq, signal_int,
                                snr, type) {
  # pos is a list of three components
  # [[1]] is the position of main effects
  # [[2]] is the position of squared effects
  # [[3]] is the interaction effects
  # with position derived by the order 1-2, 1-3, ..., (p-1)-p
  pos <- list()
  
  if(type == "mixed"){
    compact <- rbind(matrix(c(rep(0, 6), seq(6)), byrow = FALSE, ncol = 2),
                     matrix(rep(c(1, 5, 15), 2), ncol = 2),
                     matrix(c(1, 2, 6, 7, 10, 11, 8, 9, 1, 7, 8, 10),
                            byrow = TRUE, ncol = 2))      
  }
  else if(type == "hier"){
    compact <- rbind(matrix(c(rep(0, 6), seq(6)), byrow = FALSE, ncol = 2),
                     matrix(rep(c(1, 2, 3), 2), ncol = 2),
                     matrix(c(1, 3, 2, 4, 3, 4, 1, 8, 2, 8, 5, 10),
                            byrow = TRUE, ncol = 2))      
  }
  else if(type == "antihier"){
    compact <- rbind(matrix(c(rep(0, 6), seq(6)), byrow = FALSE, ncol = 2),
                     matrix(rep(c(11, 12, 13), 2), ncol = 2),
                     matrix(10 + c(1, 3, 2, 4, 3, 4, 1, 8, 2, 8, 5, 10),
                            byrow = TRUE, ncol = 2))      
  }
  else if(type == "interonly"){
    compact <- rbind(matrix(c(1, 3, 2, 4, 3, 4, 1, 8, 2, 8, 5, 10),
                            byrow = TRUE, ncol = 2))      
  }
  else if(type == "mainonly"){
    compact <- rbind(matrix(c(rep(0, 6), seq(6)), byrow = FALSE, ncol = 2))      
  }
  else if(type == "squareonly"){
    compact <- rbind(matrix(rep(seq(6), 2), ncol = 2))
  }
  
  idx_main <- which(compact[, 1] == 0)
  idx_sq <- which(compact[, 1] == compact[, 2])
  idx_int <- which(compact[, 1] != 0 & compact[, 1] != compact[, 2])
  
  # main effects design matrix
  # AR model
  x_tr <- scale(rmvnorm(n_tr, sigma = toeplitz(0.5^seq(0, p - 1))))
  x_te <- scale(rmvnorm(n_te, sigma = toeplitz(0.5^seq(0, p - 1))))
  
  # coefficients
  beta_main <- rep(signal_main, length(idx_main))
  beta_sq <- rep(signal_sq, length(idx_sq))
  beta_int <- rep(signal_int, length(idx_int))
  # construct training signal
  mu_tr <- as.numeric(x_tr[, compact[idx_main, 2]] %*% beta_main) + 
    as.numeric(scale(x_tr[, compact[idx_sq, 1]]^2) %*% beta_sq) + 
    as.numeric(scale(x_tr[, compact[idx_int, 1]] * x_tr[, compact[idx_int, 2]]) %*% beta_int)
  
  # construct test signal
  mu_te <- as.numeric(x_te[, compact[idx_main, 2]] %*% beta_main) + 
    as.numeric(scale(x_te[, compact[idx_sq, 1]]^2) %*% beta_sq) + 
    as.numeric(scale(x_te[, compact[idx_int, 1]] * x_te[, compact[idx_int, 2]]) %*% beta_int)
  
  # taking snr = ||mu||^2 / (n * sigma^2)
  sigma <- sqrt(sum(mu_tr^2) / (n_tr * snr)) 
  
  # generate model
  new_model(name = type,
            label = paste(type, 
                          sprintf("p = %s, snr = %s, type = %s", p, snr, type)),
            params = list(n_tr = n_tr, n_te = n_te, p = p,
                          mu_tr = mu_tr, mu_te = mu_te,
                          snr = snr, sigma = sigma, 
                          x_tr = x_tr, x_te = x_te, 
                          compact = compact, type = type),
            simulate = function(n_tr, mu_tr, sigma, nsim) {
              # this function must return a list of length nsim
              y <- mu_tr + sigma * matrix(rnorm(nsim * n_tr), n_tr, nsim) 
              return(split(y, col(y)))
            })
}
