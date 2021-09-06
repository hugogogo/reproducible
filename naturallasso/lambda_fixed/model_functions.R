## @knitr models
library(MASS)
library(distr)
N <- 100

make_sparse_model <- function(p, alpha, rho, snr) {
  # p: dimensions with values = 100, 200, 500, 1000
  # alpha controls the sparsity, n^\alpha = nnz
  # rho is the correlation between columns of the design matrix X
  # snr: signal-to-noise ratio
  # fix value of n = 100 for all simulation settings
  # construct the covariance matrix of the design matrix X
  # x_{ij} ~ N(0, 1)
  # columns of x have correlation rho
  Sigma <- matrix(rho, nrow = p, ncol = p)
  diag(Sigma) <- 1
  x <- mvrnorm(n = N, mu = rep(0, p), Sigma = Sigma)
  # standardize x so that it has colmeans 0 and ||x_j||^2 = n
  x <- scale(x, center = TRUE, scale = TRUE) / sqrt(N - 1) * sqrt(N)
  # number of nonzeros
  nnz <- ceiling(N^alpha)
  # random indices of nonzero elements in beta
  ind <- sample(p, nnz)
  # nonzero element values are set equals to a random sample from
  # Laplace(1)
  D <- DExp(rate = 1)
  beta <- rep(0, p)
  beta[ind] <- r(D)(1)
  # true sigma 
  sigma <- sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / snr))
  # true signal
  mu <- as.numeric(x[, ind] %*% beta[ind])
  # ||beta||_1 / sigma^2
  signal_th <- sum(abs(beta)) / sigma
  
  new_model(name = "slm",
            label = sprintf("p = %s, alpha = %s, rho = %s, snr = %s",
                            p, alpha, rho, snr),
            params = list(x = x, beta = beta, 
                          sigma = sigma, mu = mu,
                          signal_th = signal_th,
                          p = p, alpha = alpha, rho = rho, snr = snr),
            simulate = function(mu, sigma, nsim) {
              # this function must return a list of length nsim
              y <- mu + sigma * matrix(rnorm(nsim * N), N, nsim)
              return(split(y, col(y))) # make each col its own list element
            })
}