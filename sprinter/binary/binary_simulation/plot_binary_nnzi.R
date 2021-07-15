rm(list = ls())
library(simulator)
library(RColorBrewer)
source("../../plot_functions.R")

sim_name <- "binary"
metric_name <- "mse_pred"
sim <- load_simulation(name = sim_name) %>% 
  subset_simulation(snr == 2) %>%
  subset_simulation(methods = c("sprinter1cv",
                                "IP", "APL"))

n_method <- length(evals(sim)[[1]]@method_name)

col_seq <- brewer.pal(8, "Paired")[c(2, 4, 6)]

pch_seq <- seq(n_method)

## plots begin
pdf(file = "./plots/binary_nnzi.pdf", width = 10, height = 4)
mat <- matrix(c(1, 2, 3), 1)
layout(mat, c(9.7, 9, 9.01), c(1, 1, 1))
par(cex.main = 1.6, cex.lab = 1.6, cex.axis = 1.2)

ylab <- "Mean squared error"
xlab <- "Number of nonzero interactions"
metric_name_1 <- "nnzi"
metric_name_2 <- "mse_pred"

par(mar = c(4, 5, 2, 0))
mod_type <- "main"
sim_sub <- subset_simulation(sim, type == mod_type)
mir <- round(model(sim_sub)@params$mir, 3)
plot_main <- paste(mod_type, " (MIR = ", mir, ", snr = 2)", sep = "")
plot_two_raw_evals(sim = sim_sub,
                   metric_name_1 = metric_name_1,
                   metric_name_2 = metric_name_2,
                   main = plot_main,
                   xlab = xlab,
                   ylab = ylab,
                   method_col = col_seq,
                   method_pch = pch_seq,
                   #legend_location = "topright")
                   legend_location = NULL)

par(mar = c(4, 2.5, 2, 0))
mod_type <- "middle"
sim_sub <- subset_simulation(sim, type == mod_type)
mir <- round(model(sim_sub)@params$mir, 3)
plot_main <- paste(mod_type, " (MIR = ", mir, ", snr = 2)", sep = "")
plot_two_raw_evals(sim = sim_sub,
                   metric_name_1 = metric_name_1,
                   metric_name_2 = metric_name_2,
                   main = plot_main,
                   xlab = xlab,
                   ylab = ylab,
                   method_col = col_seq,
                   method_pch = pch_seq,
                   #legend_location = "topright")
                   legend_location = NULL)

par(mar = c(4, 2.5, 2, 0.2))
mod_type <- "inter"
sim_sub <- subset_simulation(sim, type == mod_type)
mir <- round(model(sim_sub)@params$mir, 3)
plot_main <- paste(mod_type, " (MIR = ", mir, ", snr = 2)", sep = "")
plot_two_raw_evals(sim = sim_sub,
                   metric_name_1 = metric_name_1,
                   metric_name_2 = metric_name_2,
                   main = plot_main,
                   xlab = xlab,
                   ylab = ylab,
                   method_col = col_seq,
                   method_pch = pch_seq,
                   legend_location = "topright")
                   #legend_location = NULL)

dev.off()