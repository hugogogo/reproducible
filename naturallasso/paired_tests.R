rm(list = ls())
library(simulator)

sim <- load_simulation("even_more_cv", dir = "~/Desktop/sim_cv")
sim <- subset_simulation(sim, snr == 1 & rho %in% c(0.3, 0.6, 0.9), methods = c("nl_cv_df", "nl_cv_obj", "ol_cv_obj"))
ev <- sim %>% evals %>% as.data.frame
library(dplyr)
library(tidyr)
dr <- levels(ev$Draw)
tab <- ev %>% select(Model, Method, Draw, mse) %>% group_by(Model, Method, Draw) %>% spread(Draw, mse)

mods <- levels(tab$Model)
mets <- levels(tab$Method)


# let's look at some p-values
pvals_w <- matrix(NA, length(mods), length(mets))
rownames(pvals_w) <- mods
colnames(pvals_w) <- unlist(sapply(mets, 
                                 function(mm) paste(setdiff(mets, mm), 
                                                    collapse = " vs. ")))
pvals_t <- pvals_w

for (m in seq_along(mods)) {
  for (me in seq_along(mets)) {
    vals <- tab[tab$Model == mods[m] & tab$Method != mets[me], -(1:2)]
    pvals_w[m, me] <- wilcox.test(as.numeric(vals[1, ] - vals[2, ]))$p.value
    pvals_t[m, me] <- t.test(as.numeric(vals[1, ] - vals[2, ]))$p.value
  }  
}
round(pvals_t, 6) %>% xtable %>% print
round(pvals_w, 6) %>% xtable %>% print

# perhaps easier to just report fraction of the time one method gets lower than other
prop_wins <- matrix(NA, length(mods), length(mets))
rownames(prop_wins) <- mods
colnames(prop_wins) <- unlist(sapply(mets, 
                                     function(mm) paste(setdiff(mets, mm), collapse = " vs. ")))
for (m in seq_along(mods)) {
  for (me in seq_along(mets)) {
    vals <- tab[tab$Model == mods[m] & tab$Method != mets[me], -(1:2)]
    prop_wins[m, me] <- sum(vals[1, ] < vals[2, ])
  }  
}
# round(prop_wins / 900, 4) %>% knitr::kable()