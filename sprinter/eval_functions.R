## @knitr metrics

# prediction error, defined as
# ||X\hat{\beta} - X\beta|| / ||X\beta||
mse_pred <- new_metric("mse_pred", "prediction mse",
                       metric = function(model, out){
                         fit_te <- predict(object = out, model$x_te)
                         sqrt(mean((model$mu_te - fit_te)^2))
                       }
)

nnzm <- new_metric("nnzm", "number of selected main effects",
                   metric = function(model, out){
                     select <- out$compact
                     return(nrow(select[select[, 1] == 0, , drop = FALSE]))
                   }
)

nnzi <- new_metric("nnzi", "number of selected interactions",
                   metric = function(model, out){
                     select <- out$compact
                     return(nrow(select[select[, 1] != 0, , drop = FALSE]))
                   }
)

recall <- new_metric("recall", "recall",
                   metric = function(model, out){
                     select <- out$compact[out$compact[, 1] != 0, 1:2, drop = FALSE]
                     if(nrow(select) > 0)
                      iidx <- sprintr::extract_inter_indices(select, model$p) - model$p
                     else
                       iidx <- integer(0)
                     return(sum(model$gamma[iidx] != 0) / sum(model$gamma != 0))
                   }
)