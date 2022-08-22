#Get results/errors from purrr::safely/quietly lists

#'safely' returns results and errors
#'quietly' returns results and warnings

get_result <- function(object){
  
  result <- object$result
  return(result)
  
}

get_error <- function(object){
  
  error <- object$error
  return(error)
  
}

get_warning <- function(object){
  
  warning <- object$warning
  return(warning)
  
}