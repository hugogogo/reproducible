#### This is the source code for doing Table 3 (validating Theorem 1 holding for NHANES dataset)
#### and Figure 3 (now)
set.seed(123)
rm(list = ls())
library(glmnet)
library(cheapknockoff)
library(knockoff)
library(ROCR)
load("diabetes.RData")

# for simplicity, just consider all the real-valued features
x <- raw$features[, -raw$one_hot_idx]

# the response has three classes normal (0), pre-diabetes (1), and diabetes (2)
# for simplicity, combine pre-diabetes with diabetes and make it a 2-classification problem
y <- raw$targets
y[y == 2] <- 1

costs <- raw$costs[-raw$one_hot_idx]
name <- raw$name[-raw$one_hot_idx]
type <- raw$type[-raw$one_hot_idx]

# use sample mean / covariance as estimates
mu <- colMeans(x)
Sigma <- cov(x)
# make Sigma pd
Sigma <- Sigma + diag(0.1, ncol(Sigma))

## run multiple knockoff procedure
run_mk <- function(x, y, x_te, mu, Sigma, omega, family){
  # construct multiple knockoffs
  x_k <- cheapknockoff::multiple_knockoff_Gaussian(X = x,
                                      mu = mu,
                                      Sigma = Sigma,
                                      omega = omega)
  # compute knockoff statistics
  stat <- cheapknockoff::stat_glmnet_coef(X = x,
                                      X_k = x_k,
                                      y = y,
                                      omega = omega, nlam = 100, family = family)

  # mk filter: compute the path of select variables
  path <- cheapknockoff::generate_path(kappa = stat$kappa, tau = stat$tau)

  # given the fitted path of variables, do prediction on the left out data
  result <- cheapknockoff::refit(path = path, x = x, y = y, newdata = x_te, family = family)
  return(result)
}

## run regular knockoff procedure
run_rk <- function(x, y, x_te, mu, Sigma, family){
  knockoffs <- function(x)
    create.gaussian(x, mu, Sigma)

  w <- function(x, x_k, y)
    suppressWarnings(stat.glmnet_coefdiff(x, x_k, y, nfolds = 5, family = "binomial"))

  out = knockoff.filter(x, y, knockoffs = knockoffs, statistic = w)
  # compute the set of select variables
  idx = out$selected

  # added a tiny ridge
  mod <- glm(formula = y ~ ., family = family, data = data.frame(y = y, x = x[, idx]))
  pred <- as.numeric(predict(mod, newdata = data.frame(x = x_te[, idx]), type = "response"))

  result <- list(path = idx, mod = mod, pred = pred)
  return(result)
}

## run logistic regression
run_lr <- function(x, y, x_te, family){
  mod <- glm(formula = y~., family = family, data = data.frame(y = y, x = x))
  #mod <- glmnet(x = x, y = y, family = family, alpha = 0, lambda = 1e-4)
  # given the fitted path of variables, do prediction on the left out data
  pred <- as.numeric(predict(object = mod, newx = x_te, type = "response"))

  result <- list()
  result$mod <- mod
  result$pred <- pred
  return(result)
}

## compute weighted fdp
compute_measure <- function(out, costs, truth, alpha = 0.2){
  p <- length(out$path)
  wfdp <- rep(NA, length = p)
  fdp <- rep(NA, length = p)
  ub <- rep(NA, length = p)
  ubr <- rep(NA, length = p)
  ubo <- rep(NA, length = p)
  cost <- rep(NA, length = p)
  size <- rep(NA, length = p)
  
  for(i in seq(p)){
    select <- out$path[[i]]
    size[i] <- length(select)
    cost[i] <- sum(costs[select])
    if(length(select) > 0){
      # index of false discovery
      idx <- setdiff(select, truth)
      wfdp[i] <- sum(costs[idx]) / sum(costs[select])
      fdp[i] <- length(idx) / length(select)
    }
    else{
      wfdp[i] <- 0
      fdp[i] <- 0
    }
    # upper bounds for ours and Katesvich&Ramdas
    ub[i] <- (1 + (i - length(select))) / max(sum(costs[select]), 1)
    ubr[i] <- (1 + (i - length(select))) / max(length(select), 1)
  }
  ub <- min(max(costs / log(costs - (costs - 1) * (alpha))) * (-log(alpha)) * ub, 1)
  ubr <- min((-log(alpha)) / log(2 - alpha)  * ubr, 1)
  wo <- costs[-truth]
  ubo <- min(max(wo / log(wo - (wo - 1) * (alpha))) * (-log(alpha)) * ub, 1)

  return(list(wfdp = wfdp, fdp = fdp,
              size = size, ub = ub, ubr = ubr, ubo = ubo, cost = cost))
}

#################################################
# start the simulation
n <- nrow(x)
p <- length(costs)

n_tr <- 72062
n_te <- n - n_tr
# we use 72062 randomly selected sample and run LR on it to get the "truth"
idx_tr <- sample(n, n_tr)
x_tr <- x[idx_tr, ]
y_tr <- as.numeric(y[idx_tr])
# first run LR to get the "truth"
truth <- glm(formula = y~., family = "binomial", data = data.frame(y = y_tr, x = x_tr))
thresh <- 0.01
thresh_fwer <- thresh / p
#thresh_fwer <- thresh
# S is the set of variables that we "believe" to be the "truth"
S <- as.integer(which(summary(truth)$coefficients[-1, 4] <= thresh_fwer))
# beta is the corresponding coefficient estimates
beta <- rep(0, p)
beta[S] <- as.numeric(summary(truth)$coefficients[-1, 1][S])

a0 <- summary(truth)$coefficients[1, 1]

name[head(order(summary(truth)$coefficients[-1, 4]), length(S))]

# alternative method to approximate the true set
# backward selection using AIC
# back <- stepAIC(truth, trace = FALSE)

# we consider the partially-simulated data
# i.e., we consider S as the "truth", and simulate data from this truth
z = a0 + x %*% beta
pr = 1/(1+exp(-z))
y_sim = rbinom(n, 1, pr)

# now we randomly devide the rest 20000 samples into 50 test dataset, each consisting of 400 samples
idx_te <- seq(n)[-idx_tr]
size_te <- 400
nrep <- n_te / size_te
list_test <- split(sample(idx_te), ceiling(seq_along(idx_te)/size_te))

ours <- list()
uw <- list()
for(i in seq(length(list_test))){
  x_te <- x[list_test[[i]], ]
  y_te <- as.numeric(y_sim[list_test[[i]]])

  ## our method:
  ours[[i]] <- run_mk(x = x_te, y = y_te, x_te = x_te, mu = mu, Sigma = Sigma, omega = costs, family = "binomial")
  ## cheapknockoff with no weights
  uw[[i]] <- run_mk(x = x_te, y = y_te, x_te = x_te, mu = mu, Sigma = Sigma, omega = rep(2, length(costs)), family = "binomial")

  cat(i, fill = TRUE)
}


###############################################
# record result for TABLE 3
alpha_list <- seq(0.05, 0.5, by = 0.05)

res_ours <- rep(NA, length(alpha_list))
res_uw <- rep(NA, length(alpha_list))
for (j in seq(length(alpha_list))){
  out_ours <- list()
  out_uw <- list()
  for(i in seq(nrep)){
    out_ours[[i]] <- compute_measure(out = ours[[i]], costs = costs, truth = S, alpha = alpha_list[j])
    out_uw[[i]] <- compute_measure(out = uw[[i]], costs = costs, truth = S, alpha = alpha_list[j])
  }
  ratio_ours_w <- matrix(NA, nrep, p)
  ratio_ours_r <- matrix(NA, nrep, p)
  ratio_uw_r <- matrix(NA, nrep, p)
  for (i in seq(nrep)){
    ratio_ours_w[i, ] <- out_ours[[i]]$wfdp / out_ours[[i]]$ubo
    ratio_uw_r[i, ] <- out_ours[[i]]$fdp / out_ours[[i]]$ubr
  }
  res_ours[j] <- mean(apply(ratio_ours_w, 1, max) >=1 )
  res_uw[j] <- mean(apply(ratio_uw_r, 1, max) >= 1)
}
###############################################

# compare wfdp quantiles to draw FIGURE 3
out_ours <- list()
out_uw <- list()
wfdp_ours <- matrix(0, nrow = nrep, ncol = p)
wfdp_uw <- matrix(0, nrow = nrep, ncol = p)
cost_ours <- matrix(0, nrow = nrep, ncol = p)
cost_uw <- matrix(0, nrow = nrep, ncol = p)
for(i in seq(nrep)){
  out_ours[[i]] <- compute_measure(out = ours[[i]], costs = costs, truth = S, alpha = alpha_list[1])
  out_uw[[i]] <- compute_measure(out = uw[[i]], costs = costs, truth = S, alpha = alpha_list[1])
  
  wfdp_ours[i, ] <- out_ours[[i]]$wfdp
  wfdp_uw[i, ] <- out_uw[[i]]$wfdp
  
  cost_ours[i, ] <- out_ours[[i]]$cost
  cost_uw[i, ] <- out_uw[[i]]$cost
}

my_quantile <- function(x) quantile(x, probs = c(0.2, 0.5, 0.8))
quan_ours <- apply(wfdp_ours, 2, my_quantile)
quan_uw <- apply(wfdp_uw, 2, my_quantile)

pdf("data_wfdp.pdf", height = 5.5, width = 11)
par(mfrow = c(1, 2))
par(mar = c(4, 5, 1, 1))
# accuracy
xlim <- c(0, p)
ylim <- c(0, max(quan_uw))
plot(x = NULL, y = NULL, xlim = xlim, ylim = ylim, xlab = "k",
     ylab = expression(wFDP(R[k])), cex.axis = 2, cex.lab = 2)
lines(seq(p), quan_ours[1, ],
      type = "o", col = "black", ylim = ylim, lwd = 3, lty = 1,  pch = 4, cex = 1)

lines(seq(p), quan_ours[2, ],
      type = "o", col = "black", ylim = ylim, lwd = 3, lty = 3, pch = 16, cex = 1)

lines(seq(p), quan_ours[3, ],
      type = "o", col = "black", ylim = ylim, lwd = 3, lty = 6, pch = 17, cex = 1)

lines(seq(p), quan_uw[1, ],
      type = "o", col = "red", ylim = ylim, lwd = 3, lty = 1, pch = 4, cex = 1)

lines(seq(p), quan_uw[2, ],
      type = "o", col = "red", ylim = ylim, lwd = 3, lty = 3, pch = 16, cex = 1)

lines(seq(p), quan_uw[3, ],
      type = "o", col = "red", ylim = ylim, lwd = 3, lty = 6, pch = 17, cex = 1)




quan_ours <- apply(cost_ours, 2, my_quantile)
quan_uw <- apply(cost_uw, 2, my_quantile)
# accuracy
xlim <- c(0, p)
ylim <- c(0, max(quan_uw))
plot(x = NULL, y = NULL, xlim = xlim, ylim = ylim, xlab = "k",
     ylab = expression(cost(R[k])), cex.axis = 2, cex.lab = 2)
lines(seq(p), quan_ours[1, ],
      type = "o", col = "black", ylim = ylim, lwd = 3,lty = 1, pch = 4, cex = 1)

lines(seq(p), quan_ours[2, ],
      type = "o", col = "black", ylim = ylim, lwd = 3, lty = 3, pch = 16, cex = 1)

lines(seq(p), quan_ours[3, ],
      type = "o", col = "black", ylim = ylim, lwd = 3, lty = 6, pch = 17, cex = 1)

lines(seq(p), quan_uw[1, ],
      type = "o", col = "red", ylim = ylim, lwd = 3, lty = 1, pch = 4, cex = 1)

lines(seq(p), quan_uw[2, ],
      type = "o", col = "red", ylim = ylim, lwd = 3, lty = 3, pch = 16, cex = 1)

lines(seq(p), quan_uw[3, ],
      type = "o", col = "red", ylim = ylim, lwd = 3, lty = 6, pch = 17, cex = 1)

legend("topleft", title="Percentile",
       legend=c("20", "50", "80"), 
       col=c(rep("darkgrey",3)),
       pch = c(4, 16, 17),
       lwd = c(rep(3, 3)), lty = c(1, 3, 6),
       bty="n", border=F, cex = 1.5)

legend("bottomright", 
       legend=c("Cheap knockoffs", "Katsevich & Ramdas(2018)"), 
       col=c('black','red'),
       lwd = c(3, 3),
       bty="n", border=F, cex = 1.3)

dev.off()