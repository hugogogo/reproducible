# This is the main simulator file
rm(list = ls())
library(simulator) # this file was created under simulator version 0.2.0
library(ggplot2)
library(scalreg)
library(natural)

source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

## @knitr init

## @knitr main

p <- 500
alpha_list <- as.list(seq(9) / 10)
rho_list <- as.list(seq(9) / 10)
snr_list <- as.list(c(0.3, 1, 3))
nsim <- 100 

# an intermediate step for carrying out the simulation for comparison
sim_setup <- new_simulation(name = "setup",
                               label = "Error Variance Estimation Comparison Setup") %>%
  generate_model(make_sparse_model, seed = 123,
                 p = p,
                 alpha = alpha_list,
                 rho = rho_list,
                 snr = snr_list,
                 vary_along = c("alpha", "rho", "snr")) %>%
  simulate_from_model(nsim = nsim) %>%
  run_method(list(ol_cv, nl_cv, ol_fix, sl_fix))
save_simulation(sim_setup)


## simulation on comparison of estimation methods using predefined value of lambda
sim_fix <- load_simulation(name = "setup", dir = "./") %>%
  subset_simulation(methods = "") %>%
  rename("relerr_fix") %>% relabel("Relative Errors without CV") %>%
  run_method(list(sl_fix + naive_1,
                  sl_fix + naive_2,
                  ol_fix + obj_1,
                  oracle))
save_simulation(sim_fix)
sim_fix <- load_simulation(name = "relerr_fix") %>% evaluate(list(mse, ratio))
save_simulation(sim_fix)

sim <- load_simulation(name = "setup") %>%
  rename("more_fix_setup") %>% relabel("more_fix_setup") %>%
  subset_simulation(methods = c("ol_fix", "sl_fix")) %>% 
  subset_simulation(alpha %in% c(0.1, 0.3, 0.5, 0.7, 0.9) & rho %in% c(0.3, 0.6, 0.9)) %>%
  simulate_from_model(nsim = 100, index = 1:10) %>%
  run_method(list(ol_fix, ol_fix_guess, sl_fix))
save_simulation(sim)

sim <- load_simulation("more_fix_setup") %>% 
  subset_simulation(methods = "") %>%
  rename("more_fix") %>% relabel("more_fix") %>%
  run_method(list(sl_fix + naive_1,
                  sl_fix + naive_2,
                  ol_fix + obj_1,
                  ol_fix_guess + obj_1,
                  ol_fix_guess + obj_2,
                  oracle))  

sim <- evaluate(sim, metrics = list(mse, ratio))

sim_fix <- load_simulation(name = "relerr_fix", dir = "./") %>%
  rename("relerr_fix_guess") %>% relabel("Relative Errors without CV guess") %>%
  run_method(list(ol_fix_guess))
save_simulation(sim_fix)

sim_fix <- run_method(sim_fix, methods = list(ol_fix_guess + obj_1, ol_fix_guess + obj_2))
save_simulation(sim_fix)

sim_fix_sub <- subset_simulation(sim = sim_fix, methods = c("oracle", "sl_fix_naive_1", "sl_fix_naive_2", "ol_fix_obj_1", "ol_fix_guess_obj_1", "ol_fix_guess_obj_2")) %>% evaluate(metrics = list(ratio, mse))
save_simulation(sim_fix_sub)