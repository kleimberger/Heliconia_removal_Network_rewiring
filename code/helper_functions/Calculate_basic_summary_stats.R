#Function to calculate min, max, mean, sd, and median
#Need to pass arguments with {{}}. See https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
calculate_basic_summary_stats <- function(data, variable){
  
  sum <- data %>%
    dplyr::summarise(min = min({{ variable }}, na.rm = TRUE),
                     max = max({{ variable }}, na.rm = TRUE),
                     mean = mean({{ variable }}, na.rm = TRUE),
                     sd = sd({{ variable }},  na.rm = TRUE),
                     median = median({{ variable }}, na.rm = TRUE),
                     n = sum(!is.na({{ variable }})))
  
  return(sum)
  
}



  


