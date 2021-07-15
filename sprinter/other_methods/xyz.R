#' xyz method
#'
#' @import xyz Matrix
#' @export
run_xyz <- function(x, y, L = 10, foldid = NULL, nfold = 5){
  n <- nrow(x)
  p <- ncol(x)
  fit <- xyz_regression(X = x, Y = y, n_lambda = 100, alpha = 1, L = L)

  lambda <- fit[[5]]
  nlam <- length(lambda)
  # Do CV for both stage 2 and 3
  if (is.null(foldid)){
    # foldid is a vector of values between 1 and nfold
    # identifying what fold each observation is in.
    # If supplied, nfold can be missing.
    foldid <- sample(seq(nfold), size = n, replace = TRUE)
  }

  # mse of lasso estimate of beta
  mat_mse <- matrix(NA, nrow = nlam, ncol = nfold)
  for (i in seq(nfold)){
    # train on all but i-th fold
    id_tr <- (foldid != i)
    id_te <- (foldid == i)
    # standardize the training data
    fit_tr <- xyz_regression(X = x[id_tr, ], Y = y[id_tr], lambdas = lambda, alpha = 1, L = L)
    fitted <- predict(object = fit_tr, newdata = x[id_te, ])

    # get the fit using lasso on training data
    # and fit/obj on the test data
    mat_mse[, i] <- sqrt(colMeans(t(y[id_te] - t(fitted))^2))
  }

  # extract information from CV
  # the mean cross-validated error, a vector of length nlam
  cvm <- rowMeans(mat_mse)
  cvsd <- apply(mat_mse, 1, sd)
  # the index of best lambda
  ibest <- which.min(cvm)

  fitted <- predict(fit, newdata = x)[, ibest]

  main_idx <- fit[[1]][[ibest]]
  main_idx <- cbind(rep(0, length(main_idx)), main_idx)
  compact <- cbind(main_idx, fit[[2]][[ibest]])
  int_idx <- matrix(fit[[3]][[ibest]], ncol = 2, byrow = TRUE)
  compact <- rbind(compact, cbind(int_idx, fit[[4]][[ibest]]))

  out <- list(fit = fit,
              type = 1,
              a0 = 0,
              compact = compact,
              fitted = fitted,
              lambda = lambda,
              cvm = cvm,
              cvsd = cvsd,
              foldid = foldid,
              ibest = ibest)

  class(out) <- "other"
  return(out)
}
