## @knitr methods

# oracle estimate: ||\varepsilon||^2 / n
oracle <- new_method(name = "oracle", label = "Oracle",
                     method = function(model, draw){
                       sig <- sqrt(mean((draw - model$mu)^2))
                       return(list(sig = sig))
                     })

ol_path <- new_method(name = "ol_path", label = "Ol_path",
                    method = function(model, draw){
                      result <- olasso_path(x = model$x, 
                                            y = draw)
                      return(result)
                    })

# wrapper functionto call slasso_cv
sl_cv <- new_method(name = "sl_cv", label = "SQRT_cv",
                    method = function(model, draw){
                      result <- slasso_cv(x = model$x, 
                                          y = draw)
                      return(result)
                    })

# wrapper function to call nlasso_cv
nl_cv <- new_method(name = "nl_cv", label = "Nl_cv",
                    method = function(model, draw){
                      result <- nlasso_cv(x = model$x, 
                                          y = draw)
                      return(result)
                    }
)

# wrapper function to call olasso_cv
ol_cv <- new_method(name = "ol_cv", label = "Sq_cv",
                    method = function(model, draw){
                      result <- olasso_cv(x = model$x, 
                                          y = draw)
                      return(result)
                    }
)
 
# wrapper function to call olasso
ol_fix <- new_method(name = "ol_fix", label = "Sq_fix",
                 method = function(model, draw){
                   result <- olasso(x = model$x, 
                                    y = draw)
                   return(result)
                 }
)

# wrapper function to call olasso
ol_fix_guess <- new_method(name = "ol_fix_guess", label = "Sq_fix_guess",
                           method = function(model, draw){
                             result <- olasso_guess(x = model$x, 
                                              y = draw)
                             return(result)
                           }
)

# wrapper function to call slasso
sl_fix <- new_method(name = "sl_fix", label = "SQRT_fix",
                 method = function(model, draw){
                   result <- slasso(x = model$x, 
                                    y = draw)
                   return(result)
                 }
)

# wrapper function to call Bayati
Bayati <- new_method(name = "Bayati", label = "Bayati et,al",
                     method = function(model, draw){
                       result <- runBayati(x = model$x, 
                                           y = draw)
                       return(result)
                     }
)

# method extensions which extract results from wrapper functions
ibest <- new_method_extension(name = "ibest", 
                              label = "ibest",
                              method_extension = function(model, draw, out, base_method){
                                return (list(ibest = out$ibest / length(out$lambda)))
                              })

lambest <- new_method_extension(name = "lambest", 
                                 label = "lambest",
                                 method_extension = function(model, draw, out, base_method){
                                   return (list(lam = out$lambda[out$ibest]))
                                 })

obj <- new_method_extension(name = "obj", 
                            label = "obj",
                            method_extension = function(model, draw, out, base_method){
                              return (list(sig = out$sig_obj))
                            })

naive <- new_method_extension(name = "naive", 
                              label = "naive",
                              method_extension = function(model, draw, out, base_method){
                                return (list(sig = out$sig_naive))
                              })

df <- new_method_extension(name = "df", 
                           label = "df",
                           method_extension = function(model, draw, out, base_method){
                             return (list(sig = out$sig_df))
                           })


obj_1 <- new_method_extension(name = "obj_1", 
                            label = "obj_1",
                            method_extension = function(model, draw, out, base_method){
                              return (list(sig = out$sig_obj_1))
                            })

naive_1 <- new_method_extension(name = "naive_1", 
                              label = "naive_1",
                              method_extension = function(model, draw, out, base_method){
                                return (list(sig = out$sig_naive_1))
                              })

df_1 <- new_method_extension(name = "df_1", 
                           label = "df_1",
                           method_extension = function(model, draw, out, base_method){
                             return (list(sig = out$sig_df_1))
                           })

obj_2 <- new_method_extension(name = "obj_2", 
                            label = "obj_2",
                            method_extension = function(model, draw, out, base_method){
                              return (list(sig = out$sig_obj_2))
                            })

naive_2 <- new_method_extension(name = "naive_2", 
                              label = "naive_2",
                              method_extension = function(model, draw, out, base_method){
                                return (list(sig = out$sig_naive_2))
                              })

df_2 <- new_method_extension(name = "df_2", 
                           label = "df_2",
                           method_extension = function(model, draw, out, base_method){
                             return (list(sig = out$sig_df_2))
                           })