# This is the main simulator file
rm(list=ls())
set.seed(123)
library(simulator)

source("model_Gaussian.R")
source("../method_functions.R")
source("../eval_functions.R")

# load in other methods implementation
source("../other_methods/apl.R")

## @knitr init
name_of_simulation <- "speed"
## @knitr main
n_tr <- 100
n_te <- 1000
signal_main <- 2
signal_sq <- 3
signal_int <- 3
snr <- 3
p_seq <- c(100, 200, 500, 1000, 2000)
sim <- new_simulation(name = name_of_simulation,
                      label = "Speed Comparison") %>%
  generate_model(make_gaussian_model, seed = 123,
                 n_tr = n_tr, n_te = n_te, p = as.list(p_seq),
                 signal_main = signal_main,
                 signal_sq = signal_sq,
                 signal_int = signal_int,
                 snr = snr, 
                 type = "mixed",
                 vary_along = "p") %>%
  simulate_from_model(nsim = 100) %>%
  run_method(c(sprinter, sprinter1cv, APL, SIS)) %>%
  evaluate(list(mse_pred, nnzm, nnzi))
save_simulation(sim = sim)