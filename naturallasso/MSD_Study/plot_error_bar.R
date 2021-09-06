library(ggplot2)
library(reshape2)
library(xtable)
rm(list = ls())
load("./MSD_result_big_n.RData")
#ylab <- expression(E~group("[", hat(sigma) / sigma, "]"))
#ylabel <- "Relative error"
ylabel <- "Mean squared error"

#cc <- cc[cc$variable != "Bayati", ]
#cc <- cc[cc$variable != "scaled.1.",]
ggplot(cc, aes(n, v_mean, group = variable, color = variable)) + ylim(0, 0.15) + geom_point() + geom_line() + xlab('n') + ylab(ylabel) + theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title=element_blank()) + geom_hline(yintercept = 1.0, linetype = 2) + scale_color_discrete(name = "variable", labels=c("naive", "Reid et,al", "natural(cv)", "organic(cv)", "scaled(1)", "scaled(2)", "Bayati et,al", expression(plain(organic)(lambda[2])), expression(plain(organic)(lambda[3]))))# + geom_errorbar(aes(ymax = v_mean + v_sd, ymin = v_mean - v_sd), width = 0.25) 

# mean_table <- round(as.matrix(dcast(cc, n ~ variable, value.var = "v_mean")), 4)[, -1]
# sd_table <- round(as.matrix(dcast(cc, n ~ variable, value.var = "v_sd")), 4)[, -1]
# nn <- nrow(mean_table)
# 
# M <- matrix(as.vector(paste(as.character(mean_table), " (", as.vector(sd_table), ")", sep = "")), nrow = nn)
# 
# M <- cbind(as.character(unique(cc$n)), M)
# colnames(M) <- c("n", "naive", "Reid et, al", "natural", "organic", "scaled(1)", "scaled(2)", "Bayati et, al", "organic(2)", "organic(3)")
# 
# tab <- print(xtable(t(M)), include.rownames = FALSE)
