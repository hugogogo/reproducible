rm(list = ls())
library(simulator)
library(reshape2)
library(xtable)
load("./MSD_result_new.RData")
#ylab <- expression(E~group("[", hat(sigma) / sigma, "]"))
#ylabel <- "Relative error"
ylabel <- "Mean squared error"

ndraws <- 1000
cc <- cc_mse
cc <- cc[cc$variable != "Bayati", ]
method_name <- levels(droplevels(unique(cc$variable)))

cc$v_sd <- cc$v_sd / sqrt(ndraws)

multiplier <- 100
mean_table <- round(multiplier * as.matrix(dcast(cc, n ~ variable, value.var = "v_mean")), 2)[, -1]
sd_table <- round(multiplier * as.matrix(dcast(cc, n ~ variable, value.var = "v_sd")), 2)[, -1]
nn <- nrow(mean_table)

outlier <- result_mse[result_mse$n == 80, ]$scaled.1.
# the largest two elements are outliers
range(outlier)
outlier <- outlier[-which.max(outlier)]
outlier <- outlier[-which.max(outlier)]
range(outlier)
# remove them and re-calculate the mean and se
mean_table[4, 5] <- round(multiplier * mean(outlier), 2)
sd_table[4, 5] <- round(multiplier * sd(outlier) / sqrt(ndraws), 2)

M_mse <- matrix(as.vector(paste(as.character(mean_table), " (", as.vector(sd_table), ")", sep = "")), nrow = nn)

M_mse <- cbind(as.character(unique(cc$n)), M_mse)
colnames(M_mse) <- c("n", "naive", "Reid et, al", "natural", "organic", "scaled(1)", "scaled(2)", "organic(2)", "organic(3)")

tab_mse <- print(xtable(t(M_mse)))

cc <- cc_ratio
cc$v_sd <- cc$v_sd / sqrt(ndraws)
cc <- cc[cc$variable != "Bayati", ]
method_name <- levels(droplevels(unique(cc$variable)))

multiplier <- 100
mean_table <- round(multiplier * as.matrix(dcast(cc, n ~ variable, value.var = "v_mean")), 1)[, -1]
sd_table <- round(multiplier * as.matrix(dcast(cc, n ~ variable, value.var = "v_sd")), 1)[, -1]
nn <- nrow(mean_table)

outlier <- result_ratio[result_ratio$n == 80, ]$scaled.1.
# the largest two elements are outliers
range(outlier)
outlier <- outlier[-which.max(outlier)]
outlier <- outlier[-which.max(outlier)]
range(outlier)
# remove them and re-calculate the mean and se
mean_table[4, 5] <- round(multiplier * mean(outlier), 1)
sd_table[4, 5] <- round(multiplier * sd(outlier) / sqrt(ndraws), 1)

M_ratio <- matrix(as.vector(paste(as.character(mean_table), " (", as.vector(sd_table), ")", sep = "")), nrow = nn)

M_ratio <- cbind(as.character(unique(cc$n)), M_ratio)
colnames(M_ratio) <- c("n", "naive", "Reid et, al", "natural", "organic", "scaled(1)", "scaled(2)", "organic(2)", "organic(3)")

tab_ratio <- print(xtable(t(M_ratio)))