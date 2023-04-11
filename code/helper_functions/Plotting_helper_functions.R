#############################
##Make interaction plot
############################
#Plot estimated marginals means for:
#control pre, control post, treatment pre, treatment post
make_interaction_plot <- function(emmeans_df, sampling_method, yvar, ymax, add_icon = TRUE){
  
  camera_icon <-  png::readPNG("../../code/analysis/camera.png")
  pollen_icon <-  png::readPNG("../../code/analysis/pollen.png")
  
  #Colors and shapes
  colors <- c("#0E0D37", "#BA0022") #blue, red
  shapes <- c(16, 17) #circle, triangle (filled)
  
  #Labels for y-axis
  if(yvar == "H2"){ylabel <- c(expression(paste(italic(H[2])*"′")))}
  if(yvar == "d"){ylabel <-  c(expression(paste("mean "*italic(d)*"′")))}
  if(yvar == "species.specificity.index"){ylabel <- c("mean SSI")}
  if(yvar == "num_morphotypes"){ylabel <- c("# morphotypes")}
  
  #Group/legend labels
  group_labels <- c("control" = "Control", "treatment" = "Treatment")
  legend_label <- c(expression(paste(italic("Heliconia "), "removal", sep = "")))
 
  #Plotting by hand for increased customization
  plot <- emmeans_df %>%
    mutate(control_treatment = factor(control_treatment, levels = c("control", "treatment"), labels = c("Control", "Treatment"))) %>%
    mutate(exp_phase = factor(exp_phase, levels = c("pre", "post"), labels = c("Pre", "Post"))) %>%
    ggplot(data = ., aes(x = exp_phase, y = estimate, colour = control_treatment, shape = control_treatment)) + 
      geom_point(aes(x = exp_phase, y = estimate, colour = control_treatment), position = position_dodge(width = 0.25), size = 3.5) +
      geom_line(aes(group = control_treatment), position = position_dodge(0.25), alpha = 0.6, linewidth = 1) +
      geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.25), width = 0.00, linewidth = 1) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.')) +
      scale_color_manual(values = colors, labels = group_labels) +
      scale_shape_manual(values = shapes, labels = group_labels) +
      labs(x = "Experimental period", y = ylabel, colour = legend_label, shape = legend_label) +
      coord_cartesian(ylim = c(0, ymax), xlim = c(1, 2), clip = "off")
  
  #Add icons to plots
  if(add_icon == TRUE & sampling_method == "pollen"){
    
    plot <- plot +
      theme(plot.margin = unit(c(3, 1, 0.5, 0.5), "lines")) + #Default plot margins in theme_bw are 1, 1, 0.5, 0.5 (top, right, bottom, left)
      annotation_custom(rasterGrob(pollen_icon, width = 0.5), xmin = 1, xmax = 2, ymin = ymax * 1.25, ymax = Inf)
    
  }
  
  if(add_icon == TRUE & sampling_method == "camera"){
    
    plot <- plot +
      theme(plot.margin = unit(c(3, 1, 0.5, 0.5), "lines")) + #Default plot margins in theme_bw are 1, 1, 0.5, 0.5 (top, right, bottom, left)
      annotation_custom(rasterGrob(camera_icon, width = 0.5), xmin = 1, xmax = 2, ymin = ymax * 1.25, ymax = Inf)
    
  }
  
  return(plot)
  
}

##################################
#Make control vs. treatment plot
##################################
#This plot is similar to interaction plot, but for response variables not calculated at the level of experimental period (control vs. treatment only)
make_control_vs_treatment_plot <- function(emmeans_df, yvar, ymax){
  
  #Labels for y-axis
  if(yvar == "WN"){ylabel <- c("Total turnover")}
  if(yvar == "ST"){ylabel <-  c("Species turnover")}
  if(yvar == "OS"){ylabel <- c("Among-species turnover")}
  
  #Colors and shapes
  colors <- c("#0E0D37", "#BA0022") #blue, red
  shapes <- c(16, 17) #circle, triangle (filled)
  
  #Group/legend labels
  group_labels <- c("control" = "Control", "treatment" = "Treatment")
  legend_label <- c(expression(paste(italic("Heliconia "), "removal", sep = "")))
  
  plot <- emmeans_df %>%
    mutate(control_treatment = factor(control_treatment, levels = c("control", "treatment"), labels = c("Control", "Treatment"))) %>%
    ggplot(data = ., aes(x = control_treatment, y = estimate, colour = control_treatment, shape = control_treatment)) +
      geom_point(aes(x = control_treatment, y = estimate), position = position_dodge(width = 0.25), size = 4) +
      geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.25), width = 0.0, linewidth = 1) +
      theme_bw(base_size = 18) +
      theme(legend.position = "none",
            legend.direction = "horizontal",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 18)) +
      scale_color_manual(values = colors, labels = group_labels) +
      scale_shape_manual(values = shapes, labels = group_labels) +
      labs(x = "", y = ylabel, colour = "", shape = "") +
      ylim(0, ymax)
  
  return(plot)
  
}

###########################################################
#Make plot of interaction contrast + CI ("contrast plot")
###########################################################
#Will need to customize axes, etc. separately, but this will make the basic plot
#estimate_name = name of the column that has the point estimate (e.g., odds.ratio, ratio, etc.)
make_contrast_plot <- function(contrasts_df, xvar, shading = "none", text_size, title_size, point_size, line_size){
  
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
      geom_point(position = position_dodge(.5), size = point_size) +
      geom_errorbar(position = position_dodge(.5), aes(ymax = upper.CL, ymin = lower.CL), width = 0.25, linewidth = line_size) + # error bars show 95% confidence intervals
      geom_hline(yintercept = 1, color = "black", linetype = "dashed", alpha = 0.8, linewidth = line_size) + # add a line at 1 (no effect)
      theme_bw(base_size = text_size) +
      scale_shape_manual(values = c(16, 17), labels = legend_text_labels, limits = c("all_spp", "greh_visa")) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.justification = "left",
            legend.text = element_text(size = text_size),
            legend.title = element_text(size = text_size),
            axis.title = element_text(size = text_size),
            axis.text = element_text(size = text_size)) +
      labs(x = "", y = "Experimental effect", shape = "Bird group")
    
  return(plot)
  
}
