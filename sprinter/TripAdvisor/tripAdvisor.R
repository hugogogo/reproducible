rm(list = ls())
set.seed(123)
library(sprintr)
library(glmnet)
source("../other_methods/apl.R")
source("../other_methods/RAMP.R")
source("../other_methods/inter_pursuit.R")

#### data.RData exceeds the maximum file size of github
#### to get the data, please contact the author: Guo Yu (guoyu@ucsb.edu)
load("./data.RData")

idx_train <- sample(nrow(X_bin), size = ceiling(0.1 * nrow(X_bin)), replace = FALSE)
# we use presense-absense instead of count for now
X <- X_bin
X_train <- X[idx_train, ]
X_test <- X[-idx_train, ]
X_train <- X[idx_train, which(colSums(X_train) != 0)]
X_test <- X[-idx_train, which(colSums(X_train) != 0)]
y_train <- rating_all_final[idx_train]
y_test <- rating_all_final[-idx_train]

n <- nrow(X_train)
p <- ncol(X_train)

method_name <- c("MEL", "Two_stage Hier lasso", "IP", "SIS_lasso",
                 "sprinter", "sprinter1cv")
measure_name <- c("prediction rmse",
                  "# selected main effects",
                  "# selected interactions")
result <- matrix(NA, nrow = length(measure_name),
                 ncol = length(method_name))
rownames(result) <- measure_name
colnames(result) <- method_name

out <- list()

## Training starts
idx_method <- 1
# MEL
out[[idx_method]] <- mel(x = X_train, y = y_train)

# hierarchical lasso
idx_method <- idx_method + 1
out[[idx_method]] <- sprintr::hier_lasso(x = X_train, y = y_train, lam_choice = "min")

# by turning on refit, we are actually putting RAMP in advantage
# RAMP (implemented in the package "RAMP") does not work in this data set
# mod_RAMP <- run_RAMP(x = X_train, y = y_train)

num_keep <- min(p, ceiling(n / log(n)))

# ip
idx_method <- idx_method + 1
out[[idx_method]] <- run_ip(x = X_train, y = y_train, num_keep = 800)

# SIS lasso
idx_method <- idx_method + 1
out[[idx_method]] <- sprintr::sis_lasso(x = X_train, y = y_train, num_keep = num_keep)

# sprinter
idx_method <- idx_method + 1
out[[idx_method]] <- cv.sprinter(x = X_train, y = y_train,
                        square = FALSE, num_keep = num_keep)

# sprinter1cv
idx_method <- idx_method + 1
out[[idx_method]] <- cv.sprinter(x = X_train, y = y_train,
                        square = FALSE, num_keep = num_keep, cv_step1 = TRUE)

stopifnot(length(out) == length(method_name))

save(out, file = "out.RData")

## Evaluation starts
for(j in seq(length(out))){
  # prediction
  result[1, j] <- sqrt(mean((as.numeric(predict(object = out[[j]], newdata = X_test)) - y_test)^2))

  # number of selected main effects
  cmp <- out[[j]]$compact
  result[2, j] <- nrow(cmp[cmp[, 1] == 0, ])

  # number of selected interactions
  result[3, j] <- nrow(cmp[cmp[, 1] != 0, ])
}

var <- colnames(X_train)
save(result, var, file = "result.RData")