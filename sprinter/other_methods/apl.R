#' Running all pairs lasso
#'
#' @import glmnet
#' @export
apl <- function(x, y){
  # x is the unstandardized design matrix
  p <- ncol(x)
  n <- nrow(x)
  q <- (p^2 + 3 * p) / 2

  x <- sprintr::myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")
  mean_y <- mean(y)
  # pairwise index of interactions
  idx <- t(combn(p, 2))
  # construct design matrix of pairwise interactions
  design <- sprintr::myscale(cbind(x^2, x[, idx[, 1]] * x[, idx[, 2]]))
  # now idx contains all index pairs
  idx <- rbind(cbind(rep(0, p), seq(p)), cbind(seq(p), seq(p)), idx)
  col_mean <- c(col_mean, attr(x = design, which = "scaled:center"))
  col_sd <- c(col_sd, attr(x = design, which = "scaled:scale"))
  design <- cbind(x, design)

  lambda <- sprintr::get_lambda(x = design, y = y - mean_y)
  fit <- glmnet::cv.glmnet(x = design, y = y - mean_y,
                           lambda = lambda,
                           intercept = FALSE,
                           standardize = FALSE)

  ibest <- which.min(fit$cvm)
  beta <- as.numeric(fit$glmnet.fit$beta[, ibest])
  fitted <- as.numeric(mean_y + design %*% beta)
  # scale estimates back to the original scale of x
  beta <- beta / col_sd
  a0 <- as.numeric(mean_y - crossprod(col_mean, beta))

  compact <- cbind(idx[beta != 0, , drop = FALSE], beta[beta != 0])
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  out <- list(a0 = a0,
              type = 1,
              compact = compact,
              fitted = fitted,
              lambda = lambda,
              cvm = fit$cvm,
              cvsd = fit$cvsd,
              ibest = ibest,
              call = match.call())

  class(out) <- "other"
  return(out)
}


#' Running lasso on main effects only
#' @import glmnet
#' @export
mel <- function(x, y){
  p <- ncol(x)
  q <- (p^2 + 3*p) / 2
  # x is the unstandardized design matrix
  x <- sprintr::myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")
  mean_y <- mean(y)

  lambda <- sprintr::get_lambda(x = x, y = y - mean_y)
  fit <- glmnet::cv.glmnet(x = x, y = y - mean_y,
                           lambda = lambda,
                           intercept = FALSE,
                           standardize = FALSE)

  ibest <- which.min(fit$cvm)
  beta <- as.numeric(fit$glmnet.fit$beta[, ibest])
  fitted <- as.numeric(mean_y + x %*% beta)
  # scale estimates back to the original scale of x
  beta <- beta / col_sd
  a0 <- as.numeric(mean_y - crossprod(beta, col_mean))

  idx <- cbind(rep(0, p), seq(p))
  compact <- cbind(idx[beta != 0, , drop = FALSE], beta[beta != 0])
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  out <- list(a0 = a0,
              type = 1,
              compact = compact,
              fitted = fitted,
              lambda = lambda,
              cvm = fit$cvm,
              cvsd = fit$cvsd,
              ibest = ibest,
              call = match.call())

  class(out) <- "other"
  return(out)
}
