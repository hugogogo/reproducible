run_ip <- function(x, y, num_keep = NULL, nfold = 5, foldid = NULL){
  # num_keep: number of elements to pick for each of main & interactions
  n <- length(y)
  p <- ncol(x)
  q <- (p^2 + 3 * p) / 2
  if(is.null(num_keep)){
    num_keep = min(ceiling(n / log(n)), p)
  }
  else{
    num_keep = min(ceiling(num_keep), p)
  }

  # main effects
  x <- sprintr::myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")
  mean_y <- mean(y)

  # main effects screening using SIS
  ws <- rep(NA, p)
  for(j in seq(p))
    ws[j] <- abs(cor(x[, j], y))
  B <- sort(head(sort(ws, decreasing = TRUE, index.return = TRUE)$ix, num_keep))
  # important interaction variables
  w <- rep(NA, p)
  for(j in seq(p))
    w[j] <- abs(cor(x[, j]^2, y^2))
  A <- sort(head(sort(w, decreasing = TRUE, index.return = TRUE)$ix, num_keep))
  # candidate main effects
  M <- sort(union(A, B))
  # candidate interactions
  # unlike the original paper, we also include squared effects
  I <- t(combn(A, 2))
  x_int <- sprintr::myscale(cbind(x[, A]^2, x[, I[, 1]] * x[, I[, 2]]))
  col_mean <- c(col_mean[M],
                 attr(x = x_int, which = "scaled:center"))
  col_sd <- c(col_sd[M],
                 attr(x = x_int, which = "scaled:scale"))
  # used in the final stage after cross-validation
  design <- cbind(x[, M], x_int)
  # construct index pair
  idx <- rbind(cbind(rep(0, length(M)), M), cbind(A, A), I)

  # fit the lasso
  lambda <- sprintr::get_lambda(x = design, y = y - mean_y)
  nlam <- length(lambda)

  # Do CV for both screening and variable selection (using lasso)
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

    # training/testing data set
    ws_tr <- rep(NA, p)
    for(j in seq(p))
      ws_tr[j] <- abs(cor(x[id_tr, j], y[id_tr]))
    B_tr <- head(sort(ws_tr, decreasing = TRUE,
                      index.return = TRUE)$ix, num_keep)
    # important interaction variables
    w_tr <- rep(NA, p)
    for(j in seq(p))
      w_tr[j] <- abs(cor(x[id_tr, j]^2, y[id_tr]^2))
    A_tr <- head(sort(w_tr, decreasing = TRUE,
                      index.return = TRUE)$ix, num_keep)
    # candidate main effects
    M_tr <- union(A_tr, B_tr)
    # candidate interactions
    I_tr <- combn(A_tr, 2)

    x_int_tr <- sprintr::myscale(cbind(x[id_tr, A_tr]^2,
                            x[id_tr, I_tr[1, ]] * x[id_tr, I_tr[2, ]]))
    x_int_te <- sprintr::myscale(cbind(x[id_te, A_tr]^2,
                            x[id_te, I_tr[1, ]] * x[id_te, I_tr[2, ]]),
                      center = attr(x = x_int_tr, which = "scaled:center"),
                      scale = attr(x = x_int_tr, which = "scaled:scale"))

    x_tr <- cbind(sprintr::myscale(x[id_tr, M_tr]), x_int_tr)
    x_te <- cbind(sprintr::myscale(x[id_te, M_tr]), x_int_te)
    y_tr <- y[id_tr]
    y_te <- y[id_te]
    # get the fit using lasso on training data
    # and fit/obj on the test data
    mat_mse[, i] <- run_path(x_tr = x_tr,
                             y_tr = y_tr,
                             x_te = x_te,
                             y_te = y_te,
                             lambda = lambda,
                             intercept = TRUE)
  }
  # extract information from CV
  # the mean cross-validated error, a vector of length nlam
  cvm <- rowMeans(mat_mse)
  cvsd <- apply(mat_mse, 1, sd)
  # the index of best lambda
  ibest <- which.min(cvm)

  fit <- glmnet::glmnet(x = design,
                        y = y - mean_y,
                        lambda = lambda,
                        intercept = FALSE,
                        standardize = FALSE)

  beta <- as.numeric(fit$beta[, ibest])
  fitted <- as.numeric(mean_y + design %*% beta)
  beta <- beta / col_sd
  a0 <- as.numeric(mean_y - crossprod(col_mean, beta))

  compact <- cbind(idx[beta != 0, , drop = FALSE], beta[beta != 0])
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  out <- list(a0 = a0,
              type = 1,
              compact = compact,
              fitted = fitted,
              lambda = lambda,
              cvm = cvm,
              cvsd = cvsd,
              foldid = foldid,
              ibest = ibest,
              call = match.call())
  class(out) <- "other"
  return(out)
}

run_path <- function(x_tr, y_tr, x_te, y_te, lambda, intercept){
  fit <- glmnet(x = x_tr, y = y_tr, lambda = lambda, intercept = intercept)
  fitted <- predict(object = fit, newx = x_te)
  return(sqrt(as.numeric(colMeans((y_te - fitted)^2))))
}
