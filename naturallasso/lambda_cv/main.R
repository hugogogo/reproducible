# This is the main simulator file
rm(list = ls())
library(simulator) # this file was created under simulator version 0.2.0
library(ggplot2)
library(scalreg)
library(npnve)

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


sim_setup_new <- load_simulation(name = "setup", dir = "./") %>%
  subset_simulation(rho %in% c(0.3, 0.6, 0.9) & alpha %in% c(0.1, 0.3, 0.5, 0.7, 0.9)) %>%
  rename("relerr_cv_with_slasso") %>% relabel("Relative Errors with CV (including sqrt lasso)") %>%
  run_method(list(sl_cv))
save_simulation(sim_setup_new)

# simulation on comparison of estimation methods using cross-validation
 sim_cv <- load_simulation(name = "relerr_cv_with_slasso", dir = "./") %>%
   subset_simulation(methods = "") %>%
   rename("relerr_all") %>% relabel("Relative Errors with CV") %>%
   run_method(list(nl_cv + obj, nl_cv + naive, nl_cv + df,
                   ol_cv + obj, sl_cv + naive, oracle))
 save_simulation(sim_cv)
 
 sim_cv <- evaluate(sim_cv, metrics = list(ratio, mse))

# add more draws to the original cv simulation
sim <- load_simulation(name = "relerr_cv_with_slasso") %>% rename("more_cv_setup") %>% relabel("more_cv_setup") %>% simulate_from_model(nsim = 100, index = 2:9) %>% run_method(list(ol_cv, nl_cv))
#save_simulation(sim)
sim <- load_simulation(name = "more_cv_setup") %>% rename("even_more_cv_setup") %>% simulate_from_model(nsim = 100, index = 10) %>% subset_simulation(index = 10) %>% run_method(list(ol_cv, nl_cv))

sim <- load_simulation("more_cv_setup") %>%
  subset_simulation(methods = "") %>%
  rename("more_cv") %>% relabel("more_cv") %>%
  run_method(list(nl_cv + obj, nl_cv + naive, nl_cv + df, ol_cv + obj, oracle)) %>%
  evaluate(metrics = list(ratio, mse))
save_simulation(sim)

make_sim_combined <- function(name_1, name_2){
  sim1 <- load_simulation(name_1)
  sim2 <- load_simulation(name_2)
  
  # create a new simulation that is, initially, just a copy of A 
  sim <- sim1 %>% rename("combined") %>% relabel("combined")
  
  # add the draws from B
  sim <- add(sim, list(draws(sim2, reference = TRUE)))
  # add the method outputs from B
  sim <- add(sim, list(output(sim2, reference = TRUE)))
  # add the evaluated metrics from B
  sim <- add(sim, list(evals(sim2, reference = TRUE)))
  return(sim)
}

sim <- make_sim_combined("even_more_cv_setup", "more_cv_setup")
save_simulation(sim)

sim <- load_simulation("combined") %>% rename("even_more_cv") %>% relabel("even_more_cv") %>% run_method(list(nl_cv + obj, nl_cv + naive, nl_cv + df, ol_cv + obj, oracle)) %>%
  evaluate(metrics = list(ratio, mse))

sim <- subset_simulation(sim, methods = c("oracle", "nl_cv_obj", "nl_cv_naive", "nl_cv_df", "ol_cv_obj"))
save_simulation(sim)
