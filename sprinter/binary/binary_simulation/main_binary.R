# This is the main simulator file
rm(list=ls())
library(simulator)
library(sprintr)
library(glmnet)

# load in pre-computed population quantities in the binary model
# see ../generate_tree/compute_parameter.R
load("../generate_tree/parameter_d5.Rdata")
#load("../generate_tree/parameter_d6.Rdata")
source("../generate_tree/binary_tree.R")

# load in binary model generating function
source("model_binary.R")
source("../../method_functions.R")
source("../../eval_functions.R")

# load in other methods implementation
source("../../other_methods/apl.R")
source("../../other_methods/RAMP.R")
source("../../other_methods/inter_pursuit.R")

name_of_simulation <- "binary"

n_tr <- 100
n_te <- 100
d <- 5
prob <- 0.1

snr_seq <- c(0.3, 0.5, 1, 1.5, 2, 2.5, 3)
sim <- new_simulation(name = name_of_simulation,
                       label = "binary") %>%
 generate_model(make_binary_model, seed = 123,
                n_tr = n_tr, n_te = n_te, d = d, prob = prob,
                snr = as.list(snr_seq),
                type = "main",
                vary_along = "snr") %>%
 generate_model(make_binary_model, seed = 123,
                n_tr = n_tr, n_te = n_te, d = d, prob = prob,
                snr = as.list(snr_seq),
                type = "middle",
                vary_along = "snr") %>%
 generate_model(make_binary_model, seed = 123,
                n_tr = n_tr, n_te = n_te, d = d, prob = prob,
                snr = as.list(snr_seq),
                type = "inter",
                vary_along = "snr") %>%
 simulate_from_model(nsim = 50) %>%
 run_method(c(sprinter, sprinter_m, sprinter1cv, sprinter1cv_m,
              APL, MEL, IP, SIS, hier)) %>% 
 evaluate(metrics = c(mse_pred, nnzm, nnzi, recall))

save_simulation(sim = sim)