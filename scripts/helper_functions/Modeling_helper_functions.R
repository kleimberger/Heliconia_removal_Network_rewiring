#Functions for checking assumptions, compiling results, etc.

################################################################
#Calculate confidence intervals, method "wald" versus "profile"
################################################################
calculate_ci <- function(model, method_name){
  
  params <- vcov(model)$cond %>% rownames()
  
  ci <- confint(object = model, parm = params, method = method_name)
  
  return(ci)
  
}

