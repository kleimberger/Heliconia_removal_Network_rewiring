#Functions for checking assumptions, compiling results, etc.

################################################################
#Calculate confidence intervals, method "wald" versus "profile"
################################################################
calculate_ci <- function(model, method_name){
  
  params <- vcov(model)$cond %>% rownames()
  ci <- confint(object = model, parm = params, method = method_name)
  
  return(ci)
  
}


#########################################################
#Extract information from model object about convergence
########################################################
check_convergence <- function(model){
  
  convergence <- model$fit$convergence
  
  return(convergence) #If 0, it converged
  
}

#######################
#Check random effects
#######################
check_re <- function(model, plot_title, plot_subtitle){
  
  title = bquote(atop(bold(.(plot_title)), textstyle(.(plot_subtitle))))
  plot <- cowplot::plot_grid(plotlist = lme4:::dotplot.ranef.mer(ranef(model)$cond)) %>% ggpubr::annotate_figure(., top=text_grob(title)) 
  
  return(plot)
  
}

###########################################
#Make and store plots produced from DHARMa
###########################################
make_dharma_plot <- function(dharma_object, plot_type, plot_title = NA, plot_subtitle = NA){
  
  if(is.na(plot_title)){plot_title = ""}
  if(is.na(plot_subtitle)){plot_subtitle = ""}
  title = bquote(atop(bold(.(plot_title)), textstyle(.(plot_subtitle))))
  
  if(plot_type == "basic"){plot(dharma_object)}
  if(plot_type == "overdispersion"){testDispersion(dharma_object)}
  if(plot_type == "zeroinflation"){testZeroInflation(dharma_object)}
  if(plot_type == "outlier"){testOutliers(dharma_object)}
  
  plot <- grDevices::recordPlot()
  plot <- cowplot::plot_grid(plot) %>% annotate_figure(., top=text_grob(title)) 
  plot.new()
  
  return(plot)
  
}

########################################################################
#Create residual vs. predicted DHARMa plots for each predictor variable
########################################################################
#DHARMa version of plots, created using plotResiduals function
#'predictor_table' is a table of x-variables, with a column called 'xvar'
make_dharma_xvar_plot <- function(predictor_table, dataset, dharma_object, plot_title, plot_subtitle) {
  
  #Internal function to make plots for each predictor
  make_plot = function(xvar){
    
    xvar <- sym(xvar)
    xvar_column <- dataset %>% pull(!!xvar) #Pull is equivalent to dollar sign. Returns vector with length > 1
    plotResiduals(dharma_object$scaledResiduals, as.factor(xvar_column))
    mtext(paste(xvar), side = 1, line = 4, font = 2)
    plot <- recordPlot() #Need to use recordPlot because base R plots cannot be stored (return NULL)
    plot.new()
    
    return(plot)
    
  }
  
  predictors_and_plots = predictor_table %>%
    mutate(plot = map(xvar, ~make_plot(xvar = .)))
  
  title = bquote(atop(bold(.(plot_title)), textstyle(.(plot_subtitle))))
  multiplot = cowplot::plot_grid(plotlist = predictors_and_plots$plot, scale = 0.8) %>% annotate_figure(., top=text_grob(title)) 
  
  return(multiplot)
}

#######################################
#Extract information about sample size 
######################################
#Total number of observations
get_number_obs <- function(model){
  
  num_obs <- model$modelInfo$nobs
  
  return(num_obs)
}

#Number of levels within each random effect/group
get_number_re_levels <- function(model){
  
  #Get number of random effects
  num_re <- model$modelInfo$reStruc$condReStruc %>% length()
  
  #Get number of levels within each re
  results <- list() #empty results list
  for(i in 1:num_re){
    num_levels <- model$modelInfo$reStruc$condReStruc[[i]]$blockReps 
    re_name <- paste("group", i, ":", sep = " ")
    results[[re_name]] <- num_levels #created a named list
  }
  
  return(results)
}

#Number of replicates/etc. that went into model
#Default is number of control & treatment replicates
get_sample_size <- function(data, vars = c("year", "patch", "control_treatment"), grouping_var = "control_treatment"){
  
  vars <- syms(vars)
  grouping_var <- sym(grouping_var)
  
  sum <- data %>%
    distinct(!!!vars) %>%
    group_by(!!grouping_var) %>%
    summarise(n = n())
  
  return(sum)
  
}
