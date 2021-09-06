## @knitr metrics

ratio <- new_metric(name = "ratio", label = "Ratio",
                        metric = function(model, out) {
                          return(out$sig / model$sigma)
})

mse <- new_metric(name = "mse", label = "MSE",
                        metric = function(model, out) {
                          return((out$sig / model$sigma - 1)^2)
})

index <- new_metric(name = "index", label = "Index",
                        metric = function(model, out) {
                          return(out$ibest)
})
