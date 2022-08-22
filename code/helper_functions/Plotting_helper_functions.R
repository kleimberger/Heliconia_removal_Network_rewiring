#############################
##Make interaction plot
############################
#Plot estimated marginals means for:
#control pre, control post, treatment pre, treatment post
make_interaction_plot <- function(ggeffects_df, yvar, ymax){
  
  #Colors and shapes
  colors <- c("#0E0D37", "#BA0022") #blue, red
  shapes <- c(16, 17) #circle, triangle (filled)
  
  #Label for y-axis
  # if(yvar == "H2"){ylabel <- c(expression(atop("Network specialization", paste("("*italic(H[2])*"')"))))}
  # if(yvar == "d"){ylabel <-  c(expression(atop("Species specialization", paste("(mean "*italic(d)*"')"))))}
  # if(yvar == "species.specificity.index"){ylabel <- c(expression(atop("Species specialization", paste("(mean SSI)"))))}
  
  if(yvar == "H2"){ylabel <- c(expression(paste(italic(H[2])*"'")))}
  if(yvar == "d"){ylabel <-  c(expression(paste("mean "*italic(d)*"'")))}
  if(yvar == "species.specificity.index"){ylabel <- c("mean SSI")}
  if(yvar == "num_morphotypes"){ylabel <- c("# morphotypes")}
  
  #Group/legend labels
  group_labels <- c("control" = "Control", "treatment" = "Treatment")
  legend_label <- c(expression(paste(italic("Heliconia "), "removal", sep = "")))
 
  #Plotting by hand for increased customizability
  plot <- ggeffects_df %>%
    mutate(group = factor(group, levels = c("control", "treatment"), labels = c("Control", "Treatment"))) %>%
    mutate(x = factor(x, levels = c("pre", "post"), labels = c("Pre", "Post"))) %>%
    ggplot(data = ., aes(x = x, y = predicted, colour = group, shape = group)) + 
      geom_point(aes(x = x, y = predicted, colour = group), position = position_dodge(width = 0.25), size = 3.5) +
      geom_line(aes(group = group), position = position_dodge(0.25), alpha = 0.6, size = 1) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.25), width = 0.00, size = 1) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom", legend.direction = "horizontal", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.')) +
      scale_color_manual(values = colors, labels = group_labels) +
      scale_shape_manual(values = shapes, labels = group_labels) +
      labs(x = "Experimental period", y = ylabel, colour = legend_label, shape = legend_label) +
      ylim(0, ymax)
    
  return(plot)
  
}

##################################
#Make control vs. treatment plot
##################################
#This plot is similar to interaction plot, but for response variables not calculated at the level of experimental period (control vs. treatment only)
make_control_vs_treatment_plot <- function(ggeffects_df, yvar, ymax){
  
  #Label for y-axis
  if(yvar == "WN"){ylabel <- c("Total")}
  if(yvar == "ST"){ylabel <-  c("Species gain or loss")}
  if(yvar == "OS"){ylabel <- c("Among shared species")}
  
  plot <- ggeffects_df %>%
    mutate(x = factor(x, levels = c("control", "treatment"), labels = c("Control", "Treatment"))) %>%
    ggplot(data = ., aes(x = x, y = predicted)) + 
    geom_point(aes(x = x, y = predicted), position = position_dodge(width = 0.25), size = 4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.25), width = 0.0, size = 1) +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          legend.direction = "horizontal",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "", y = ylabel, shape = "", color = "") +
    ylim(0, ymax)
  
  return(plot)
  
}

###########################################################
#Make plot of interaction contrast + CI ("contrast plot")
###########################################################
#Will need to customize axes, etc. separately, but this will make the basic plot
#estimate_name = name of the column that has the point estimate (e.g., odds.ratio, ratio, etc.)
make_contrast_plot <- function(contrasts_df, xvar, shading = "none"){
  
  legend_text_labels <- c(expression("All species"), expression(paste(italic("Heliconia "), "specialists", sep = "")))
  
  plot_data <- contrasts_df %>%
    mutate(order = 1:length({{ xvar }}))
  
  if(shading == "below"){
    
    plot <- plot_data %>%
      ggplot(data = ., aes(x = forcats::fct_reorder(.data[[xvar]], order), y = ratio, shape = bird_group)) +
        geom_rect(xmax = Inf, xmin = -Inf, ymax = 1, ymin = 0, fill = "grey90", alpha = 1)
  }
  
  if(shading == "above"){
    
    plot <- plot_data %>%
      ggplot(data = ., aes(x = forcats::fct_reorder(.data[[xvar]], order), y = ratio, shape = bird_group)) +
        geom_rect(xmax = Inf, xmin = -Inf, ymax = Inf, ymin = 1, fill = "grey90", alpha = 1)
    
  }
  
  if(shading == "none"){
    
    plot <- plot_data %>%
      ggplot(data = ., aes(x = forcats::fct_reorder(.data[[xvar]], order), y = ratio, shape = bird_group))
    
  }
  
  plot <- plot +
      geom_point(position = position_dodge(.5), size = 3) +
      geom_errorbar(position = position_dodge(.5), aes(ymax = upper.CL, ymin = lower.CL), width = 0.25, size = 1) + # error bars show 95% confidence intervals
      geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.8) + # add a line at 1 (no effect)
      theme_bw(base_size = 18) +
      scale_shape_manual(values = c(16, 17), labels = legend_text_labels, limits = c("all_spp", "greh_visa")) +
      theme(legend.position = "top", legend.justification = "center", legend.text = element_text(size = 18), legend.title = element_text(size = 18),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "", y = "Ratio", shape = "Bird group")
    
  return(plot)
  
}
