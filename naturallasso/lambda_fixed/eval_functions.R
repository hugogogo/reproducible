## @knitr metrics

ratio <- new_metric(name = "ratio", label = "Ratio",
                        metric = function(model, out) {
                          return(out$sig / model$sigma)
})

ratio_path <- new_metric(name = "ratio_path", label = "Ratio_path",
                         metric = function(model, out) {
                           return(as.numeric(out$sig_obj / model$sigma))
                         })

mse <- new_metric(name = "mse", label = "MSE",
                        metric = function(model, out) {
                          return((out$sig / model$sigma - 1)^2)
})

index <- new_metric(name = "index", label = "Index",
                        metric = function(model, out) {
                          return(out$ibest)
})

lam <- new_metric(name = "lambda", label = "lambda",
                  metric = function(model, out) {
                    return(out$lambda)
                  })

const <- new_metric(name = "const", label = "const",
                    metric = function(model, out) {
                      return(sqrt(log(model$p) / 100))
                    })