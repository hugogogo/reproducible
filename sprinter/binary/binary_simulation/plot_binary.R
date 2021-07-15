rm(list = ls())
library(simulator)
library(RColorBrewer)
source("../../plot_functions.R")

sim_name <- "binary"
metric_name <- "mse_pred"
sim <- load_simulation(name = sim_name) %>% 
  subset_simulation(methods = c("sprinter",
                                "SIS", "IP", "APL", "MEL"))
# for comparing all variants of sprinter/rinter
#    subset_simulation(methods = c("sprinter", "sprinter_m",
#                                "sprinter2d", "sprinter2d_m",
#                                "rinter", "rinter_m",
#                                "rinter2d", "rinter2d_m"))

n_method <- length(evals(sim)[[1]]@method_name)

# when plotting competing methods
# rinter Blue
# sprinter green
# APL IP red
# MEL RAMP orange
# SIS lasso purple

col_seq <- brewer.pal(8, "Paired")[c(2, 4, 1, 3, 6)]

lty_seq <- rep(2, n_method)
lwd_seq <- rep(2, n_method)
pch_seq <- seq(n_method)

## plots begin
pdf(file = "./plots/binary.pdf", width = 10, height = 4)
#pdf(file = "./plots/binary_variants.pdf", width = 10, height = 4)
mat <- matrix(c(1, 2, 3), 1)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

ylab <- "Mean squared error"
xlab <- "snr"
xaxis <- c(0.3, 0.5, 1, 1.5, 2, 2.5, 3)
metric_name <- "mse_pred"

par(mar = c(4, 5, 2, 0))
mod_type <- "main"
sim_sub <- subset_simulation(sim, type == mod_type)
mir <- round(model(sim_sub)[[1]]@params$mir, 3)
plot_main <- paste(mod_type, " (MIR = ", mir, ")", sep = "")
plot_aggr_eval_by_model(sim = sim_sub,
                        metric_name = metric_name,
                        main = plot_main,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)
                        #legend_location = NULL)

par(mar = c(4, 2.5, 2, 0))
mod_type <- "middle"
sim_sub <- subset_simulation(sim, type == mod_type)
mir <- round(model(sim_sub)[[1]]@params$mir, 3)
plot_main <- paste(mod_type, " (MIR = ", mir, ")", sep = "")
plot_aggr_eval_by_model(sim = sim_sub,
                        metric_name = metric_name,
                        main = plot_main,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = "topright")

par(mar = c(4, 2.5, 2, 0.2))
mod_type <- "inter"
sim_sub <- subset_simulation(sim, type == mod_type)
mir <- round(model(sim_sub)[[1]]@params$mir, 3)
plot_main <- paste(mod_type, " (MIR = ", mir, ")", sep = "")
plot_aggr_eval_by_model(sim = sim_sub,
                        metric_name = metric_name,
                        main = plot_main,
                        xlab = xlab,
                        xaxis = xaxis,
                        ylab = ylab,
                        method_col = col_seq,
                        method_lty = lty_seq,
                        method_lwd = lwd_seq,
                        method_pch = pch_seq,
                        legend_location = NULL)
                        #legend_location = "bottomleft")

dev.off()