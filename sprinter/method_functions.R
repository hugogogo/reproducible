## @knitr methods
library(sprintr)
SIS <- new_method(name = "SIS", label = "SIS lasso",
                  method = function(model, draw) {
                    sprintr::sis_lasso(x = model$x_tr,
                                       y = draw)
                  })

hier <- new_method(name = "hier", label = "two-stage lasso",
                  method = function(model, draw) {
                    sprintr::hier_lasso(x = model$x_tr,
                                        y = draw,
                                        lam_choice = "min")
                  })

sprinter <- new_method(name = "sprinter", label = "sprinter",
                       method = function(model, draw) {
                         sprintr::cv.sprinter(x = model$x_tr,
                                              y = draw,
                                              square = TRUE)
                       })

sprinter_m <- new_method(name = "sprinter_m", label = "sprinter(m)",
                         method = function(model, draw) {
                           sprintr::cv.sprinter(x = model$x_tr,
                                                y = draw,
                                                square = FALSE)
                         })

sprinter1cv <- new_method(name = "sprinter1cv", label = "sprinter(1cv)",
                       method = function(model, draw) {
                         sprintr::cv.sprinter(x = model$x_tr,
                                              y = draw,
                                              square = TRUE,
                                              cv_step1 = TRUE)
                       })

sprinter1cv_m <- new_method(name = "sprinter1cv_m", label = "sprinter(1cv,m)",
                         method = function(model, draw) {
                           sprintr::cv.sprinter(x = model$x_tr,
                                                y = draw,
                                                square = FALSE,
                                                cv_step1 = TRUE)
                         })

IP <- new_method(name = "IP", label = "IP",
                 method = function(model, draw) {
                   run_ip(x = model$x_tr, y = draw)
                 })

RAMP <- new_method(name = "RAMP", label = "RAMP",
                   method = function(model, draw) {
                     run_RAMP(x = model$x_tr, y = draw)
                   })

APL <- new_method(name = "APL", label = "APL",
                  method = function(model, draw) {
                    apl(x = model$x_tr, y = draw)
                  })

MEL <- new_method(name = "MEL", label = "MEL",
                  method = function(model, draw) {
                    mel(x = model$x_tr, y = draw)
                  })

Oracle <- new_method(name = "Oracle", label = "Oracle",
                     method = function(model, draw){
                       p <- model$p

                       #x <- sprintr::myscale(model$x_tr)
                       x <- sprintr::myscale(model$x_tr)

                       idx_m <- which(model$compact[, 1] == 0)
                       if(length(idx_m) == 0){
                         x <- sprintr::myscale(x[, model$compact[, 1]] * x[, model$compact[, 2]])
                       }
                       else{
                         x <- sprintr::myscale(cbind(x[, model$compact[idx_m, 2]],
                                          x[, model$compact[-idx_m, 1]] * x[, model$compact[-idx_m, 2]]))
                       }

                       ####################
                       xx <- cbind(rep(1, nrow(x)), x)
                       # add a little bit ridge penalty in case OLS does not exist
                       beta <- as.numeric(solve(crossprod(xx) + diag(1e-8, ncol(xx)), crossprod(xx, draw)))
                       # construct compact representation for coefficient, used for prediction
                       comp <- matrix(NA, nrow = ncol(x), ncol = 3)
                       comp[, 1:2] <- model$compact[, 1:2]
                       comp[, 3] <- beta[-1]
                       colnames(comp) <- c("index_1", "index_2", "coefficient")

                       result <- list(a0 = beta[1],
                                   compact = comp,
                                   type = 1,
                                   fitted = as.numeric(xx %*% beta))
                       class(result) <- "other"
                       return(result)
                     })
