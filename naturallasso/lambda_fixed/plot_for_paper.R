rm(list = ls())
library(simulator)

### Changing rho ######
sim_name <- "relerr_fix_guess"
metric_name <- "mse"
sim <- load_simulation(name = sim_name) %>% subset_simulation(alpha %in% c(0.1, 0.3, 0.5, 0.7, 0.9))

# construct a median aggregator
pdf(file = "a.pdf", width = 10, height = 7)
mat <- matrix(c(1, 4, 2, 5, 3, 6), 2)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

ylab <- "Mean squared error"
xlab <- expression(alpha)
col_seq <- c(3, "cyan", "blue", 5, "darkgoldenrod", 6)
lty_seq <- c(5, 1, 3, 4, 2, 6)
lwd_seq <- rep(2, 6)
pch_seq <- c(19, 15, 3, 4, 17, 6)
hor_y <- 1

par(mar = c(2, 5, 2, 0))
ylim <- c(0, 0.07)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)
#legend("topleft", legend = c(expression(plain(organic)(lambda[2])), expression(plain(organic)(lambda[3])), expression(plain(organic)(lambda[0])), "oracle", "scaled(1)", "scaled(2)"), col = col_seq, lty = lty_seq, lwd = lwd_seq, pch = pch_seq, cex = 1.2)

par(mar = c(2, 2.5, 2, 0))
ylim <- c(0, 0.04)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex = 5) 

par(mar = c(2, 2.5, 2, 0.2))
ylim <- c(0, 0.035)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5) 

metric_name <- "ratio"
ylab <- expression(E~group("[", sigma^{-1}*hat(sigma), "]"))
par(mar = c(4, 5, 0, 0))
ylim <- c(0.75, 1.25)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)
abline(h = hor_y, col = "gray")

ylim <- c(0.8, 1.2)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)
abline(h = hor_y, col = "gray")

ylim <- c(0.92, 1.2)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 5)
abline(h = hor_y, col = "gray")
dev.off()

# for appendix
pdf(file = "b.pdf", width = 10, height = 7)
mat <- matrix(c(1, 4, 2, 5, 3, 6), 2)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

metric_name <- "mse"
ylab <- "Mean squared error"
xlab <- expression(alpha)

par(mar = c(2, 5, 2, 0))
ylim <- c(0, 0.013)
sim_sub <- subset_simulation(sim, snr == 0.3 & rho == 0.6)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)

par(mar = c(2, 2.5, 2, 0))
ylim <- c(0, 0.04)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 5)

par(mar = c(2, 2.5, 2, 0.2))
ylim <- c(0, 0.06)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim, 
             cex_pch = 5)
#legend("topleft", legend = c(expression(plain(organic)(lambda[2])), expression(plain(organic)(lambda[3])), expression(plain(organic)(lambda[0])), "oracle", "scaled(1)", "scaled(2)"), col = col_seq, lty = lty_seq, lwd = lwd_seq, pch = pch_seq, cex = 1.2)

metric_name <- "ratio"
ylab <- expression(E~group("[", sigma^{-1}*hat(sigma) , "]"))
par(mar = c(4, 5, 0, 0))
ylim <- c(0.9, 1.05)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)
abline(h = hor_y, col = "gray")

ylim <- c(0.8, 1.2)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)
abline(h = hor_y, col = "gray")

ylim <- c(0.7, 1.15)
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
             method_col = col_seq,
             method_lty = lty_seq,
             method_lwd = lwd_seq,
             method_pch = pch_seq,
             legend_location = NULL,
             ylim = ylim,
             cex_pch = 5)
abline(h = hor_y, col = "gray")
dev.off()