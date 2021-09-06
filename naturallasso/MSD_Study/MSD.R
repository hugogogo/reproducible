library(npnve)
library(lars)
library(reshape2)
library(dplyr)
library(ggplot2)

rm(list = ls())
set.seed(123)
# the raw data YearPredictionMSD.txt
# is available at https://archive.ics.uci.edu/ml/datasets/yearpredictionmsd
rawraw <- read.table(file = "./YearPredictionMSD.txt", header = FALSE, sep = ",")
raw <- rawraw[1 : 463715, ]

index_tr <- sample(nrow(raw), size = ceiling(nrow(raw) / 2))
train <- raw[index_tr, ]
test <- raw[-index_tr, ]
#train <- raw

# center the columns so that we don't need to model the intercept
n_tr <- nrow(train)
x_tr <- scale(train[, -1], center = TRUE, scale = TRUE) * sqrt(n_tr) / sqrt(n_tr - 1)
#y_tr <- train[, 1] - mean(train[, 1])
y_tr <- train[, 1]

mod_ls <- lm(formula = y_tr ~ ., data = data.frame(cbind(y_tr, x_tr)))
sigma <- sqrt(deviance(mod_ls) / df.residual(mod_ls))

#lambda_transform <- boxcox(mod_ls, lambda = seq(0, 300, by = 1))

#y_transform <- (y_tr^lambda_transform - 1) / lambda_transform

n_seq <- seq(20, 120, by = 20)
M <- 1000

result_mse <- matrix(0, nrow = M * length(n_seq), ncol = 10)
colnames(result_mse) <- c("n", "naive", "Reid", "natural", "organic", "scaled(1)", "scaled(2)", "Bayati", "organic(1)", "organic(2)")

result_ratio <- result_mse

n_te <- nrow(test)
ibest_result <- matrix(0, nrow = M * length(n_seq), ncol = 3)
colnames(ibest_result) <- c("n", "natural", "organic")

for (j in seq(length(n_seq))){
  n = n_seq[j]
  cat('n = ', n, fill = TRUE)
  index_set <- sample(n_te)
  for (i in seq(M)){
    # make non-overlap samples
    # i.e., draw without replacement
    index <- index_set[((i - 1) * n + 1) : (i * n)]
    #cat((j - 1) * M + i, fill = TRUE)
    x <- scale(test[index, -1], center = TRUE, scale = TRUE) * sqrt(n) / sqrt(n - 1)
    colnames(x) <- NULL
    rownames(x) <- NULL
    y <- as.numeric(test[index, 1])
    y <- y - mean(y)
    
    result_mse[(j - 1) * M + i, 1] <- n
    result_ratio[(j - 1) * M + i, 1] <- n
    ibest_result[(j - 1) * M + i, 1] <- n
    
    nl_out <- nlasso_cv(x = x, y = y, flmin = 0.05)
    #cat(nl_out$ibest, fill = TRUE)
    result_mse[(j - 1) * M + i, 2] <- (nl_out$sig_naive / sigma - 1)^2
    result_mse[(j - 1) * M + i, 3] <- (nl_out$sig_df / sigma - 1)^2
    result_mse[(j - 1) * M + i, 4] <- (nl_out$sig_obj / sigma - 1)^2
    
    result_ratio[(j - 1) * M + i, 2] <- nl_out$sig_naive / sigma
    result_ratio[(j - 1) * M + i, 3] <- nl_out$sig_df / sigma
    result_ratio[(j - 1) * M + i, 4] <- nl_out$sig_obj / sigma
    ibest_result[(j - 1) * M + i, 2] <- nl_out$ibest
    
    ol_out <- olasso_cv(x = x, y = y, flmin = 0.05)
    result_mse[(j - 1) * M + i, 5] <- (ol_out$sig_obj / sigma - 1)^2
    result_ratio[(j - 1) * M + i, 5] <- ol_out$sig_obj / sigma 
    ibest_result[(j - 1) * M + i, 3] <- ol_out$ibest
    
    ol_fix <- olasso_guess(x = x, y = y)
    result_mse[(j - 1) * M + i, 9] <- (ol_fix$sig_obj_1 / sigma - 1)^2
    result_mse[(j - 1) * M + i, 10] <- (ol_fix$sig_obj_2 / sigma - 1)^2
    result_ratio[(j - 1) * M + i, 9] <- ol_fix$sig_obj_1 / sigma
    result_ratio[(j - 1) * M + i, 10] <- ol_fix$sig_obj_2 / sigma
    
    sl_out <- slasso(x = x, y = y)
    result_mse[(j - 1) * M + i, 6] <- (sl_out$sig_naive_1 / sigma - 1)^2
    result_mse[(j - 1) * M + i, 7] <- (sl_out$sig_naive_2 / sigma - 1)^2
    result_ratio[(j - 1) * M + i, 6] <- sl_out$sig_naive_1 / sigma
    result_ratio[(j - 1) * M + i, 7] <- sl_out$sig_naive_2 / sigma
    
    bayati <- runBayati(x = x, y = y, flmin = 0.05)
    result_mse[(j - 1) * M + i, 8] <- (bayati$sig / sigma - 1)^2
    result_ratio[(j - 1) * M + i, 8] <- bayati$sig / sigma
  }
}

result_mse <- data.frame(result_mse)
result_ratio <- data.frame(result_ratio)

full_mse <- melt(result_mse, id.vars = 'n')
cc_mse <- full_mse %>% group_by(n, variable) %>% summarise(v_mean = mean(value, na.rm = TRUE), v_sd = sd(value, na.rm = TRUE))

full_ratio <- melt(result_ratio, id.vars = 'n')
cc_ratio <- full_ratio %>% group_by(n, variable) %>% summarise(v_mean = mean(value, na.rm = TRUE), v_sd = sd(value, na.rm = TRUE))

save(sigma, result_mse, result_ratio, cc_mse, cc_ratio, ibest_result, file = "MSD_result_new.RData")