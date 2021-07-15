rm(list = ls())
library(simulator)
library(RColorBrewer)
source("../plot_functions.R")

sim_name <- "Gaussian"
metric_name <- "mse_pred"
sim <- load_simulation(name = sim_name) %>% 
   subset_simulation(methods = c("sprinter",
                                 "sprinter1cv",
                                 "SIS", "RAMP", "IP", "APL", "MEL"))

n_method <- length(evals(sim)[[1]]@method_name)

pdf(file = "./plots/Gaussian.pdf", width = 10, height = 7)

mat <- matrix(c(1, 4, 2, 5, 3, 6), 2)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

ylab <- "Mean squared error"
xlab <- "snr"
xaxis <- c(0.3, 0.5, 1, 1.5, 2, 2.5, 3)

# when plotting competing methods
# sprinter sprinter1cv Red
# APL MEL Blue
# IP SISlasso Green
# RAMP orange
col_seq <- brewer.pal(8, "Paired")[c(2, 4, 1, 7, 3, 6, 5)]

lty_seq <- rep(2, n_method)
lwd_seq <- rep(2, n_method)
pch_seq <- seq(n_method)

## Plot begins
par(mar = c(1, 5, 4, 0))
mod_type <- "mixed"
sim_sub <- subset_simulation(sim, type == mod_type)
plot_aggr_eval_by_model(sim = sim_sub,# baseline = "Oracle", 
                        metric_name = metric_name,
                        main = mod_type,
                        xlab = xlab,
                        xaxis = NULL,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = "topright")


par(mar = c(1, 2.5, 4, 0))
mod_type <- "hier"
sim_sub <- subset_simulation(sim, type == mod_type)
plot_aggr_eval_by_model(sim = sim_sub,# baseline = "Oracle", 
                        metric_name = metric_name,
                        main = mod_type,
                        xlab = xlab,
                        xaxis = NULL,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)

par(mar = c(1, 2.5, 4, 0.2))
mod_type <- "antihier"
sim_sub <- subset_simulation(sim, type == mod_type)
plot_aggr_eval_by_model(sim = sim_sub,# baseline = "Oracle", 
                        metric_name = metric_name,
                        main = mod_type,
                        xlab = xlab,
                        xaxis = NULL,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)

par(mar = c(4, 5, 2, 0))
mod_type <- "interonly"
sim_sub <- subset_simulation(sim, type == mod_type)
plot_aggr_eval_by_model(sim = sim_sub,# baseline = "Oracle", 
                        metric_name = metric_name,
                        main = mod_type,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)

par(mar = c(4, 2.5, 2, 0))
mod_type <- "mainonly"
sim_sub <- subset_simulation(sim, type == mod_type)
plot_aggr_eval_by_model(sim = sim_sub,# baseline = "Oracle", 
                        metric_name = metric_name,
                        main = mod_type,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)


par(mar = c(4, 2.5, 2, 0.2))
mod_type <- "squareonly"
sim_sub <- subset_simulation(sim, type == mod_type)
plot_aggr_eval_by_model(sim = sim_sub,# baseline = "Oracle", 
                        metric_name = metric_name,
                        main = mod_type,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)
dev.off()