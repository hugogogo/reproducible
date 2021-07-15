extract_evals <- function(sim, baseline = NULL, time = TRUE){
  # this function extract the evals as a list
  # with each element corresponding to evaluations of all methods in one model
  e <- evals(sim)
  mod <- model(sim)
  dd <- draws(sim)
  if(length(mod) == 1){
    e <- list(e)
    mod <- list(mod)
    dd <- list(dd)
  }
  nsim <- length(dd[[1]]@draws)
  n_mod <- length(e)
  method_label <- e[[1]]@method_label
  n_method <- length(method_label)
  
  mod_label <- rep(NA, n_mod)
  
  if(!is.null(baseline)){
    idx_baseline <- which(method_label == baseline)
    if(idx_baseline == 0)
      error("Baseline metric not found!")
  }
  
  result <- list()
  for(i in seq(n_mod)){
    idx_list <- 1
    result[[i]] <- list()
    edf <- as.data.frame(e[[i]])
    mod_label[i] <- mod[[i]]@label
    for(j in seq(n_method)){
      if(is.null(baseline)){
        # the first column of edf is the model name, which is long and redundant
        result[[i]][[j]] <- edf[((j - 1) * nsim + 1) : (j * nsim), -1]
      }
      else{
        # baseline method is provided
        # all other methods performance is calculated by dividing that performance of the baseline method
        # we will discard the baseline method performance
        if(j != idx_baseline){
          result[[i]][[idx_list]] <- edf[((j - 1) * nsim + 1) : (j * nsim), -1]
          result[[i]][[idx_list]][, -(1:2)] <- result[[i]][[idx_list]][, -(1:2)] / edf[((idx_baseline - 1) * nsim + 1) : (idx_baseline * nsim), -(1:3)]
          idx_list <- idx_list + 1
        }
      }
    }
    if(is.null(baseline))
      names(result[[i]]) <- method_label
    else
      names(result[[i]]) <- method_label[-idx_baseline]
  }
  
  names(result) <- mod_label
  return(result)
}

aggregate_evals <- function(ev){
  # this function calculates the mean aggregation over multiple simulation draws of each method in each model
  # the output is a list, with each element a matrix associated with one method
  # representing the performance of different metrics (columns)
  # and different models (rows)
  
  # ev is the extracted evals, usually an object returned by `extract_evals`
  n_mod <- length(ev)
  n_method <- length(ev[[1]])
  mod_label <- names(ev)
  method_label <- names(ev[[1]])
  # the first two columns of ev[[1]][[1]] represents Method and Draw
  n_metric <- ncol(ev[[1]][[1]]) - 2
  metric_label <- colnames(ev[[1]][[1]])[-(1:2)]
  
  result <- list()
  for(i in seq(n_method)){
    result[[i]] <- data.frame(matrix(NA, n_mod, n_metric))
    for(j in seq(n_mod)){
      if(any(is.na(ev[[j]][[i]][, -(1:2)]))){
        mess <- paste0("NA value found in ", mod_label[j], " of ", method_label[i], sep = "")
        warning(mess)
      }
        
      result[[i]][j, ] <- colMeans(ev[[j]][[i]][, -(1:2)], na.rm = TRUE)
    }
    rownames(result[[i]]) <- mod_label
    colnames(result[[i]]) <- metric_label
  }
  
  names(result) <- method_label
  return(result)
}

plot_aggr_eval_by_model <- function(sim, baseline = NULL, metric_name, main = NULL,
                               xlab = NULL, xaxis = NULL, ylab = NULL, ylim = NULL,
                               method_col = NULL, method_lty = NULL,
                               method_lwd = NULL, method_pch = NULL,
                               legend_location = NULL, type = "o"){
  # this function calculates the aggregated evaluations across different models
  # custom version of plot_eval_by in simulator
  
  # extract evaluations
  ev <- extract_evals(sim = sim, baseline = baseline)
  # aggregate evaluations
  ag <- aggregate_evals(ev = ev)
  # global variables
  n_mod <- length(ev)
  n_method <- length(ev[[1]])
  mod_label <- names(ev)
  method_label <- names(ev[[1]])
  # the first two columns of ev[[1]][[1]] represents Method and Draw
  n_metric <- ncol(ev[[1]][[1]]) - 2
  metric_label <- colnames(ev[[1]][[1]])[-(1:2)]
  
  idx_metric <- which(metric_label == metric_name)
  if(idx_metric == 0)
    stop("The specified metric is not found!")
  
  # graphic variables
  if(is.null(method_col))
    method_col <- seq(n_method)
  if(is.null(method_lty))
    method_lty <- seq(n_method)
  if(is.null(method_lwd))
    method_lwd <- rep(2, n_method)
  if(is.null(method_pch))
    method_pch <- seq(n_method)
  
  xlim <- c(1, n_mod)
  if(is.null(ylim)){
    ymin <- min(ag[[1]][, idx_metric])
    ymax <- 0
    for(i in seq(n_method)){
      ymin <- min(ymin, min(ag[[i]][, idx_metric]))
      ymax <- max(ymax, max(ag[[i]][, idx_metric]))
    }
    ylim <- c(ymin, ymax)
  }
  
  # base plot
  plot(0, 0, xlim = xlim, ylim = ylim, xaxt = "n", main = main,
       type = "n", xlab = xlab, ylab = ylab)
  if(!is.null(xaxis))
    axis(side = 1, at = seq(n_mod), labels = xaxis)
  
  for (i in seq(n_method)){
    points(x = seq(n_mod), y = ag[[i]][, idx_metric], type = type, 
           col = method_col[i], lty = method_lty[i], lwd = method_lwd[i], 
           pch = method_pch[i])
  }
  
  if(!is.null(legend_location))
    legend(legend_location, legend = method_label,
           col = method_col, lty = method_lty, lwd = method_lwd, pch = method_pch)
}


plot_two_aggr_evals_by_model <- function(sim, baseline = NULL, 
                                         metric_name_1, metric_name_2,
                                         main = NULL, xlab = NULL,
                                         ylab = NULL, xlim = NULL, ylim = NULL,
                                         method_col = NULL, method_lty = NULL,
                                         method_lwd = NULL, method_pch = NULL,
                                         legend_location = NULL, type = "o"){
  # this function plots two aggregated metrics across different models
  # metric_name_1 is the x-axis
  # metric_name_2 is the y-axis
  
  # extract evaluations
  ev <- extract_evals(sim = sim, baseline = baseline)
  # aggregate evaluations
  ag <- aggregate_evals(ev = ev)
  # global variables
  n_mod <- length(ev)
  n_method <- length(ev[[1]])
  mod_label <- names(ev)
  method_label <- names(ev[[1]])
  # the first two columns of ev[[1]][[1]] represents Method and Draw
  n_metric <- ncol(ev[[1]][[1]]) - 2
  metric_label <- colnames(ev[[1]][[1]])[-(1:2)]
  
  idx_metric_1 <- which(metric_label == metric_name_1)
  idx_metric_2 <- which(metric_label == metric_name_2)
  
  if(idx_metric_1 == 0)
    stop("The 1st specified metric is not found!")
  if(idx_metric_2 == 0)
    stop("The 2nd specified metric is not found!")
  
  # graphic variables
  if(is.null(method_col))
    method_col <- seq(n_method)
  if(is.null(method_lty))
    method_lty <- seq(n_method)
  if(is.null(method_lwd))
    method_lwd <- rep(2, n_method)
  if(is.null(method_pch))
    method_pch <- seq(n_method)
  
  if(is.null(xlab))
    xlab <- metric_label[idx_metric_1]
  if(is.null(ylab))
    ylab <- metric_label[idx_metric_2]
  
  if(is.null(xlim)){
    xmin <- min(ag[[1]][, idx_metric_1])
    xmax <- 0
    for(i in seq(n_method)){
      xmin <- min(xmin, min(ag[[i]][, idx_metric_1]))
      xmax <- max(xmax, max(ag[[i]][, idx_metric_1]))
    }
    xlim <- c(xmin, xmax)
  }
  
  if(is.null(ylim)){
    ymin <- min(ag[[1]][, idx_metric_2])
    ymax <- 0
    for(i in seq(n_method)){
      ymin <- min(ymin, min(ag[[i]][, idx_metric_2]))
      ymax <- max(ymax, max(ag[[i]][, idx_metric_2]))
    }
    ylim <- c(ymin, ymax)
  }
  
  # base plot
  plot(0, 0, xlim = xlim, ylim = ylim, main = main,
       type = "n", xlab = xlab, ylab = ylab)
  
  for (i in seq(n_method)){
    points(x = ag[[i]][, idx_metric_1], y = ag[[i]][, idx_metric_2], type = type, 
           col = method_col[i], lty = method_lty[i], lwd = method_lwd[i], 
           pch = method_pch[i])
  }
  
  if(!is.null(legend_location))
    legend(legend_location, legend = method_label,
           col = method_col, lty = method_lty, lwd = method_lwd, pch = method_pch)
}

plot_two_raw_evals <- function(sim, baseline = NULL, 
                               metric_name_1, metric_name_2,
                               main = NULL, xlab = NULL,
                               ylab = NULL, xlim = NULL, ylim = NULL,
                               method_col = NULL, method_pch = NULL,
                               legend_location = NULL){
  # this function plots two aggregated metrics across different models
  # metric_name_1 is the x-axis
  # metric_name_2 is the y-axis
  
  # extract evaluations
  ev <- extract_evals(sim = sim, baseline = baseline)
  # global variables
  n_mod <- length(ev)
  if(n_mod > 1){
    error("only one model is allowed!")
  }
  n_method <- length(ev[[1]])
  mod_label <- names(ev)
  method_label <- names(ev[[1]])
  # the first two columns of ev[[1]][[1]] represents Method and Draw
  n_metric <- ncol(ev[[1]][[1]]) - 2
  metric_label <- colnames(ev[[1]][[1]])[-(1:2)]
  
  idx_metric_1 <- which(metric_label == metric_name_1)
  idx_metric_2 <- which(metric_label == metric_name_2)
  
  if(idx_metric_1 == 0)
    stop("The 1st specified metric is not found!")
  if(idx_metric_2 == 0)
    stop("The 2nd specified metric is not found!")
  
  # graphic variables
  if(is.null(method_col))
    method_col <- seq(n_method)
  if(is.null(method_pch))
    method_pch <- seq(n_method)
  
  if(is.null(xlab))
    xlab <- metric_label[idx_metric_1]
  if(is.null(ylab))
    ylab <- metric_label[idx_metric_2]
  
  if(is.null(xlim)){
    xmin <- min(ev[[1]][1][[1]][, idx_metric_1 + 2])
    xmax <- 0
    for(i in seq(n_method)){
      xmin <- min(xmin, min(ev[[1]][i][[1]][, idx_metric_1 + 2]))
      xmax <- max(xmax, max(ev[[1]][i][[1]][, idx_metric_1 + 2]))
    }
    xlim <- c(xmin, xmax)
  }
  
  if(is.null(ylim)){
    ymin <- min(ev[[1]][1][[1]][, idx_metric_2 + 2])
    ymax <- 0
    for(i in seq(n_method)){
      ymin <- min(ymin, min(ev[[1]][i][[1]][, idx_metric_2 + 2]))
      ymax <- max(ymax, max(ev[[1]][i][[1]][, idx_metric_2 + 2]))
    }
    ylim <- c(ymin, ymax)
  }
  
  # base plot
  plot(0, 0, xlim = xlim, ylim = ylim, main = main,
       type = "n", xlab = xlab, ylab = ylab)
  
  for (i in seq(n_method)){
    points(x = ev[[1]][i][[1]][, idx_metric_1 + 2],
           y = ev[[1]][i][[1]][, idx_metric_2 + 2],
           type = "p", 
           col = method_col[i], 
           pch = method_pch[i])
  }
  
  if(!is.null(legend_location))
    legend(legend_location, legend = method_label,
           col = method_col, pch = method_pch)
}