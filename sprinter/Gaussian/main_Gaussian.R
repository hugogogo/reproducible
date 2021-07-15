# This is the main simulator file
rm(list=ls())
library(simulator)
library(glmnet)

source("model_gaussian.R")
source("../method_functions.R")
source("../eval_functions.R")

# load in other methods implementation
source("../other_methods/apl.R")
source("../other_methods/RAMP.R")
source("../other_methods/inter_pursuit.R")

# @knitr init
name_of_simulation <- "Gaussian"
# ## @knitr main
n_tr <- 100
n_te <- 1000
p <- 400
signal_main <- 2
signal_sq <- 3
signal_int <- 3
snr_seq <- c(0.3, 0.5, 1, 1.5, 2, 2.5, 3)
sim <- new_simulation(name = name_of_simulation,
                      label = "Interaction Simulation") %>%
  generate_model(make_gaussian_model, seed = 123,
                 n_tr = n_tr, n_te = n_te, p = p, signal_main = signal_main,
                 signal_sq = signal_sq,
                 signal_int = signal_int,
                 snr = as.list(snr_seq),
                 type = "mixed",
                 vary_along = "snr") %>%
   generate_model(make_gaussian_model, seed = 123,
                  n_tr = n_tr, n_te = n_te, p = p, signal_main = signal_main,
                  signal_sq = signal_sq,
                  signal_int = signal_int,
                  snr = as.list(snr_seq),
                  type = "hier",
                  vary_along = "snr") %>%
   generate_model(make_gaussian_model, seed = 123,
                  n_tr = n_tr, n_te = n_te, p = p, signal_main = signal_main,
                  signal_sq = signal_sq,
                  signal_int = signal_int,
                  snr = as.list(snr_seq),
                  type = "antihier",
                  vary_along = "snr") %>%
   generate_model(make_gaussian_model, seed = 123,
                  n_tr = n_tr, n_te = n_te, p = p, signal_main = signal_main,
                  signal_sq = signal_sq,
                  signal_int = signal_int,
                  snr = as.list(snr_seq),
                  type = "interonly",
                  vary_along = "snr") %>%
   generate_model(make_gaussian_model, seed = 123,
                  n_tr = n_tr, n_te = n_te, p = p, signal_main = signal_main,
                  signal_sq = signal_sq,
                  signal_int = signal_int,
                  snr = as.list(snr_seq),
                  type = "mainonly",
                  vary_along = "snr") %>%
    generate_model(make_gaussian_model, seed = 123,
                  n_tr = n_tr, n_te = n_te, p = p, signal_main = signal_main,
                  signal_sq = signal_sq,
                  signal_int = signal_int,
                  snr = as.list(snr_seq),
                  type = "squareonly",
                  vary_along = "snr") %>%
  simulate_from_model(nsim = 100) %>%
  run_method(c(sprinter, sprinter_m, sprinter1cv, sprinter1cv_m,
               APL, RAMP, MEL, IP, SIS, hier, Oracle)) %>%
  evaluate(metrics = c(mse_pred, nnzm, nnzi, recall))

save_simulation(sim = sim)