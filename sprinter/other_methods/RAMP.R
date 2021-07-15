library(RAMP)
run_RAMP <- function(x, y){
  p <- ncol(x)
  # first fit using RAMP
  fit <- RAMP(X = x, y = y, refit = FALSE)
  # extract prediction in our simulation format
  fitted <- predict(object = fit, newdata = x)
  # extract parameter estimation
  a0 <- fit$a0
  if(!is.null(fit$interInd)){
    idx <- unlist(strsplit(fit$interInd, split = "X"))
    idx <- t(matrix(as.numeric(idx[idx != ""]), nrow = 2))
    idx <- rbind(cbind(rep(0, length(fit$mainInd)), fit$mainInd), idx)
    nz <- c(fit$beta.m, fit$beta.i)
  }
  else{
    nz <- fit$beta.m
    idx <- cbind(rep(0, length(fit$mainInd)), fit$mainInd)
  }

  # so beta is a nnz-by-3 matrix
  # the first two columns are the j,k indices of nonzero elements, and the third column is the estimate
  # main effect index is of form (0, k)
  # squared effect index is of form (k, k)
  # interaction effect index is of form (j, k) for j < k
  compact <- cbind(idx, nz)
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  out <- list(a0 = a0,
              type = 1,
              compact = compact,
              fitted = fitted,
              lambda = fit$lambda,
              call = match.call())
  class(out) <- "other"

  return(out)
}
