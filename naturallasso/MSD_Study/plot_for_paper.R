rm(list = ls())
library(simulator)
load("./MSD_result.RData")
#ylab <- expression(E~group("[", hat(sigma) / sigma, "]"))
#ylabel <- "Relative error"
ylabel <- "Mean squared error"

cc_mse <- cc_mse[cc_mse$variable != "Bayati", ]
cc_ratio <- cc_ratio[cc_ratio$variable != "Bayati", ]
method_name <- levels(droplevels(unique(cc_mse$variable)))

pdf(file = "a.pdf", width = 14, height = 7)
mat <- matrix(c(1, 2), nrow = 1)
layout(mat)
par(cex.lab = 1.4, cex.axis = 1.1)
par(mar = c(2, 4.6, 0.5, 0.5))
x <- unique(cc_mse$n)

palette(options("simulator.color_palette")[[1]])
## MSE panel
main <- NULL
xlim <- range(cc_mse$n)
ylim <- c(0, 0.15)
ylab <- "Mean squared error"
xlab <- "Sample size"
#xlab <- "Sparsity level"
#col_seq <- c("black", "red", "olivedrab2", "blue", "yellow", "orange", "purple", "pink")
col_seq <- c(1, 2, 4, 3, "gold", 6, 5, "deeppink")
#col_seq <- gray.colors(6)
lty_seq <- c(1, 2, 4, 3, 7, 6, 5, 8)
lwd_seq <- rep(2, 7)
pch_seq <- c(17, 3, 15, 19, 4, 6, 7, 8)
hor_y <- 1

plot(0, 0, xlab = xlab, ylab = ylab,
     xlim = xlim, ylim = ylim, 
     main = main, type = "n")
for (m in seq(length(col_seq))){
  val_varied = cbind(x, cc_mse[cc_mse$variable == method_name[m], 3])
  points(val_varied, col = col_seq[m], 
         pch = pch_seq[m], lty = lty_seq[m], lwd = lwd_seq[m], 
         type = "o")
}

## relative bias panel
main <- NULL
xlim <- range(cc_ratio$n)
ylim <- c(0.8, 1.2)
ylab <- expression(E~group("[", hat(sigma) / sigma, "]"))
xlab <- "Sample size"
hor_y <- 1

plot(0, 0, xlab = xlab, ylab = ylab,
     xlim = xlim, ylim = ylim, 
     main = main, type = "n")
for (m in seq(length(col_seq))){
  val_varied = cbind(x, cc_ratio[cc_ratio$variable == method_name[m], 3])
  points(val_varied, col = col_seq[m], 
         pch = pch_seq[m], lty = lty_seq[m], lwd = lwd_seq[m], 
         type = "o")
}
abline(h = hor_y, col = "gray")

legend("bottomright", legend = c("naive", "Reid et,al", "natural(cv)", "organic(cv)", "scaled(1)", "scaled(2)", expression(plain(organic)(lambda[2])), expression(plain(organic)(lambda[3]))), col = col_seq, lty = lty_seq, lwd = lwd_seq, pch = pch_seq, cex = 1)

dev.off()