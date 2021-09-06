library(simulator)
library(dplyr)
rm(list = ls())

##### Changing rho ######
sim_name <- "even_more_cv"
metric_name <- "mse"
sim <- load_simulation(name = sim_name)
sim_s <- load_simulation(name = "relerr_all") %>% subset_simulation(methods = c("sl_cv_naive"))

alpha_list <- c(0.1, 0.3, 0.5, 0.7, 0.9)

# construct a median aggregator
pdf(file = "a.pdf", width = 10, height = 7)
mat <- matrix(c(1, 4, 2, 5, 3, 6), 2)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

ylab <- "Mean squared error"
xlab <- expression(alpha)
#xlab <- "Sparsity level"
col_seq <- c(6, 2, 1, 4, 3, 5)
#col_seq <- gray.colors(6)
lty_seq <- c(6, 2, 3, 1, 5, 4)
lwd_seq <- rep(2, 6)
pch_seq <- c(6, 17, 3, 15, 19, 4)
hor_y <- 1

par(mar = c(2, 5, 2, 0))
ylim <- c(0, 0.1)
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.3)

plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             type = "aggregated",
             varying = "alpha",
             use_ggplot2 = FALSE,
             main = expression(list(tau == 1, rho == 0.3)),
             xlab = xlab,
             xaxt = "n",
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)

vals <- subset_simulation(sim_s, snr == 1 & rho == 0.3) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(mse))

vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

#abline(h = hor_y, col = "gray")
#legend("topleft", legend = c("scaled", "Reid et, al", "naive", "natural", "organic", "oracle"), col = col_seq, lty = lty_seq, lwd = lwd_seq, pch = pch_seq, cex = 1.2)

par(mar = c(2, 2.5, 2, 0))
ylim <- c(0, 0.05)
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             main = expression(list(tau == 1, rho == 0.6)),
             xlab = xlab,
             xaxt = "n",
             #yaxt = "n",
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 1.6)
#abline(h = hor_y, col = "gray")
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(mse))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

par(mar = c(2, 2.5, 2, 0.2))
ylim <- c(0, 0.03)
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.9)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             main = expression(list(tau == 1, rho == 0.9)),
             #yaxt = "n",
             xlab = xlab,
             xaxt = "n",
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 1.6)
#abline(h = hor_y, col = "gray")
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.9) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(mse))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

metric_name <- "ratio"
ylab <- expression(E~group("[", sigma^{-1}*hat(sigma), "]"))
#ylab <- "Relative bias"
par(mar = c(4, 5, 0, 0))
ylim <- c(0.65, 1.15)
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.3)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             #main = expression(list(tau == 1, rho == 0.3)),
             main = NULL,
             xlab = xlab,
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 1.6)
abline(h = hor_y, col = "gray")
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.3) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(ratio))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

ylim <- c(0.75, 1.1)
par(mar = c(4, 2.5, 0, 0))
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             #main = expression(list(tau == 1, rho == 0.6)),
             main = NULL,
             xlab = xlab,
             ylab = ylab,
             #yaxt = "n",
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(ratio))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)
abline(h = hor_y, col = "gray")

ylim <- c(0.85, 1.02)
par(mar = c(4, 2.5, 0, 0.2))
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.9)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             #main = expression(list(tau == 1, rho == 0.9)),
             main = NULL,
             xlab = xlab,
             ylab = ylab,
             #yaxt = "n",
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.9) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(ratio))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)
abline(h = hor_y, col = "gray")
dev.off()

# for appendix
pdf(file = "b.pdf", width = 10, height = 7)
mat <- matrix(c(1, 4, 2, 5, 3, 6), 2)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

ylab <- "Mean squared error"
xlab <- expression(alpha)
col_seq <- c(6, 2, 1, 4, 3, 5)
lty_seq <- c(6, 2, 3, 1, 5, 4)
lwd_seq <- rep(2, 6)
pch_seq <- c(6, 17, 3, 15, 19, 4)
hor_y <- 1

par(mar = c(2, 5, 2, 0))
ylim <- c(0, 0.03)
sim_sub <- subset_simulation(sim, snr == 0.3 & rho == 0.6)

metric_name <- "mse"
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             type = "aggregated",
             varying = "alpha",
             use_ggplot2 = FALSE,
             main = expression(list(tau == 0.3, rho == 0.6)),
             xlab = xlab,
             xaxt = "n",
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)

vals <- subset_simulation(sim_s, snr == 0.3 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(mse))

vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

#abline(h = hor_y, col = "gray")
#legend("topleft", legend = c("scaled", "Reid et, al", "naive", "natural", "organic", "oracle"), col = col_seq, lty = lty_seq, lwd = lwd_seq, pch = pch_seq, cex = 1.2)

par(mar = c(2, 2.5, 2, 0))
ylim <- c(0, 0.05)
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             main = expression(list(tau == 1, rho == 0.6)),
             xlab = xlab,
             xaxt = "n",
             #yaxt = "n",
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)
#abline(h = hor_y, col = "gray")
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(mse))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

par(mar = c(2, 2.5, 2, 0.2))
ylim <- c(0, 0.1)
sim_sub <- subset_simulation(sim, snr == 3 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             main = expression(list(tau == 3, rho == 0.6)),
             #yaxt = "n",
             xlab = xlab,
             xaxt = "n",
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 1.6)
#abline(h = hor_y, col = "gray")
vals <- subset_simulation(sim_s, snr == 3 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(mse))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

metric_name <- "ratio"
ylab <- expression(E~group("[", sigma^{-1}*hat(sigma), "]"))
#ylab <- "Relative bias"
par(mar = c(4, 5, 0, 0))
ylim <- c(0.85, 1.05)
sim_sub <- subset_simulation(sim, snr == 0.3 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             #main = expression(list(tau == 1, rho == 0.3)),
             main = NULL,
             xlab = xlab,
             ylab = ylab,
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)
abline(h = hor_y, col = "gray")
vals <- subset_simulation(sim_s, snr == 0.3 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(ratio))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)

ylim <- c(0.8, 1.1)
par(mar = c(4, 2.5, 0, 0))
sim_sub <- subset_simulation(sim, snr == 1 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             #main = expression(list(tau == 1, rho == 0.6)),
             main = NULL,
             xlab = xlab,
             ylab = ylab,
             #yaxt = "n",
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)
vals <- subset_simulation(sim_s, snr == 1 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(ratio))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)
abline(h = hor_y, col = "gray")

ylim <- c(0.7, 1.2)
par(mar = c(4, 2.5, 0, 0.2))
sim_sub <- subset_simulation(sim, snr == 3 & rho == 0.6)
plot_eval_by(sim = sim_sub, 
             metric_name = metric_name,
             spread_aggregator = FALSE,
             varying = "alpha",
             use_ggplot2 = FALSE,
             #main = expression(list(tau == 1, rho == 0.9)),
             main = NULL,
             xlab = xlab,
             ylab = ylab,
             #yaxt = "n",
             method_col = col_seq[-1],
             method_lty = lty_seq[-1],
             method_lwd = lwd_seq[-1],
             method_pch = pch_seq[-1],
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 1.6)
vals <- subset_simulation(sim_s, snr == 3 & rho == 0.6) %>% evals %>% as.data.frame %>% group_by(Model) %>% summarize(me = mean(ratio))
vals <- cbind(alpha_list, as.numeric(unlist(vals[, 2])))
points(vals, col = col_seq[1], 
       pch = pch_seq[1], lty = lty_seq[1], lwd = lwd_seq[1], 
       type = "o", cex = 1.6)
abline(h = hor_y, col = "gray")
dev.off()