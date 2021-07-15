source("./binary_tree.R")
#result <- compute_parameter(d = 5, prob = 0.1, num_rep = 1000)
#load("parameter_d5.RData")
load("parameter_d6.RData")
btree <- parameter$btree_avg
btree$Value <- round(btree$Value, 1)
pdf("btree_d6_example.pdf", height = 8, width = 10)
plot_btree(btree, nodeid = TRUE)
dev.off()
