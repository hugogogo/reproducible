rm(list = ls())
library(simulator)
library(RColorBrewer)
source("../plot_functions.R")

sim_name <- "speed"
sim <- load_simulation(name = sim_name) %>% 
  subset_simulation(subset = c(1, 2, 4, 5)) %>%
  subset_simulation(methods = c("APL", "sprinter", "sprinter1cv"))

# general graphical paramters
n_method <- length(evals(sim)[[1]]@method_name)

col_seq <- brewer.pal(10, "Paired")[c(2, 6, 5)]
lty_seq <- rep(2, n_method)
lwd_seq <- rep(2, n_method)
pch_seq <- seq(n_method)

pdf(file = "./plots/pred_time.pdf", width = 11, height = 5)
mat <- matrix(c(1, 2), ncol = 2)
layout(mat, c(9, 9, 9), c(1, 1, 1))
par(cex.main = 1.2, cex.lab = 1.6, cex.axis = 1.2)

xlab <- "p"
xaxis <- c(100, 200, 1000, 2000)

par(mar = c(4, 5, 1, 0.2))
ylab <- "Time (s)"
plot_aggr_eval_by_model(sim = sim, 
                        metric_name = "time", 
                        main = NULL,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = "topleft")

par(mar = c(4, 5, 1, 0.2))
ylab <- "Mean squared error"
plot_aggr_eval_by_model(sim = sim, 
                        metric_name = "mse_pred",
                        main = NULL,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)
dev.off()

pdf(file = "./plots/nnzm_nnzi_pred.pdf", width = 11, height = 5)
sim <- subset_simulation(sim, subset = c(4))

mat <- matrix(c(1, 2), ncol = 2)
layout(mat, c(9, 9, 9), c(1, 1, 1))
par(cex.main = 1, cex.lab = 1.6, cex.axis = 1.2)

plot_main <- "Mixed (p = 1000, snr = 3)"
par(mar = c(4, 5, 1, 0.2))
# plot the number of selected main effects vs prediction mse
ylab <- "Mean squared error"
xlab <- "Number of non-zero main effects"
metric_name_1 <- "nnzm"
metric_name_2 <- "mse_pred"
plot_two_raw_evals(sim = sim,
                   metric_name_1 = metric_name_1,
                   metric_name_2 = metric_name_2,
                   main = plot_main,
                   xlab = xlab,
                   ylab = ylab,
                   method_col = col_seq,
                   method_pch = pch_seq,
                   legend_location = "bottomright")

par(mar = c(4, 5, 1, 0.2))
# plot the number of selected interactions vs prediction mse
ylab <- "Mean squared error"
xlab <- "Number of non-zero interactions"
metric_name_1 <- "nnzi"
metric_name_2 <- "mse_pred"
plot_two_raw_evals(sim = sim,
                   metric_name_1 = metric_name_1,
                   metric_name_2 = metric_name_2,
                   main = plot_main,
                   xlab = xlab,
                   ylab = ylab,
                   method_col = col_seq,
                   method_pch = pch_seq,
                   legend_location = NULL)

dev.off()