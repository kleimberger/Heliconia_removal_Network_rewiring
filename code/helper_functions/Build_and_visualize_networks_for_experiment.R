#Functions to build bipartite networks and calculate metrics for each:
#1. Experimental replicate (year+patch combination)
#2. Experimental period (pre and post)
#3. Sampling method (camera and pollen)

########################
##Subset data
#########################
#Need to subset data to relevant replicate, site (patch), experimental period (pre/post)
subset_data <- function(data, method_name, year_name, patch_name, exp_phase_name){
  
  data <- data %>%
    filter(sampling_method == method_name & year == year_name & patch == patch_name & exp_phase == exp_phase_name)
  
  #For camera networks
  if(method_name == "visitation"){
    
    #Bird species that do not visit during certain period. There are currently zeroes if bird species visited during one period, but not the other.
    birds_no_visits <- data %>%
      group_by(bird_species) %>%
      summarise(sightings = sum(sightings)) %>%
      filter(sightings == 0)
    
    #Plant species that never have any visitation during certain period. There are currently zeroes if plant species had some visitation during one period, but not the other.
    plants_no_visits <- data %>%
      group_by(plant_species) %>%
      summarise(sightings = sum(sightings)) %>%
      filter(sightings == 0)
    
    data <- data %>%
      filter(!(bird_species %in% birds_no_visits$bird_species)) %>%
      filter(!(plant_species %in% plants_no_visits$plant_species))
    
  }
  
  return(data)
  
}

####################################
#Build networks by creating matrices
####################################
create_matrix <- function(data, interaction_column){
  
  #Interactions observed between higher trophic level species (columns) and lower trophic level species (rows)
  #Alphabetize to make sure it lines up with flower data
  network_data_wide <- data %>%
    select(plant_species, bird_species, interaction_column) %>%
    pivot_wider(names_from = bird_species, values_from = interaction_column) %>%
    arrange(plant_species) 
  
  #Replace empty cells with zero
  network_data_wide[is.na(network_data_wide)] <- 0 
  
  #Remove first column, add back as rownames below. drop = FALSE necessary to keep class matrix, instead of numeric, when only have one bird or plant species
  matrix <- data.matrix(network_data_wide)[,-1, drop = FALSE] 
  rownames(matrix) <- as.character(network_data_wide$plant_species)
  
  return(matrix)
  
}

###################################################
#Calculate chosen network metrics at network-level
##################################################
calculate_networklevel_metrics <- function(matrix, index_list){
  
  metrics <- bipartite::networklevel(matrix, index = index_list)
  return(metrics)
  
}

###################################################
#Calculate chosen network metrics at species-level
##################################################
calculate_specieslevel_metrics <- function(matrix, index_list){
  
  #level='higher' because only interested in higher trophic level (hummingbirds)
  metrics <- bipartite::specieslevel(matrix, level = "higher", index = index_list, empty.web = TRUE) 
  return(metrics)
  
}

################################
#Calculate sampling completeness
################################
#Note: cannot run estimateR with non-integer network OR with rounded integer network (rounded integer visitation network has numbers that are so high that S.obs = S.chao1)
#This happens because estimateR is meant for **actual counts**. Chao 1 - based on number of abundant (>= 10 ind'ls) vs. rare (<= 10 ind'ls) species
#If don't have counts, doesn't make sense! https://stackoverflow.com/questions/66491005/alpha-diversity-indices-using-estimater-vegan-package-in-r
#When calculating sampling completeness for visitation data, be sure to use network matrix created with actual numbers of sightings (visits), rather than the integer-ified rate
calculate_completeness <-  function(matrix){
  
  output <- vegan::estimateR(t(matrix(matrix, ncol = 1)))
  
  return(output)
  
}

###################
#Visualize network
##################
visualize_network <- function(method_name, matrix, label_heto){
  
  if(label_heto == FALSE){
    
    graphics::par(font = 3) #To make species names italics, change graphical parameters
    network <- bipartite::plotweb(matrix, method = "normal", arrow = "no", text.rot = 90, labsize = 2, low.spacing = 0.02,
                                  y.lim = c(-0.75, 2.75),
                                  x.lim = c(0, 1.75))
    
    plot <- grDevices::recordPlot()
    plot <- cowplot::plot_grid(plot)
    plot.new()
    
  }
  
  if(label_heto == TRUE & method_name == "visitation"){
    
    graphics::par(font = 3) #To make species names italics, change graphical parameters
    network <- bipartite::plotweb(matrix, method = "normal", arrow = "no", text.rot = 90, labsize = 2, low.spacing = 0.02,
                                  y.lim = c(-0.75, 2.75),
                                  x.lim = c(0, 1.75),
                                  col.low = ifelse(rownames(matrix) == "Heliconia tortuosa", "#BA0022", "grey10"),
                                  bor.col.low = NA,
                                  col.interaction = t(ifelse(matrix[,] > 0 & rownames(matrix) == "Heliconia tortuosa", "#BA0022", "grey80")),
                                  bor.col.interaction = t(ifelse(matrix[,] > 0 & rownames(matrix) == "Heliconia tortuosa", "#BA0022", "black")))
    
    plot <- grDevices::recordPlot()
    plot <- cowplot::plot_grid(plot)
    plot.new()
    
  }
  
  if(label_heto == TRUE & method_name == "pollen"){
    
    graphics::par(font = 3) #To make species names italics, change graphical parameters
    network <- bipartite::plotweb(matrix, method = "normal", arrow = "no", text.rot = 90, labsize = 2, low.spacing = 0.02,
                                  y.lim = c(-0.75, 2.75),
                                  x.lim = c(0, 1.75),
                                  col.low = ifelse(rownames(matrix) == "HELICONIA01", "#BA0022", "grey10"),
                                  bor.col.low = NA,
                                  col.interaction = t(ifelse(matrix[,] > 0 & rownames(matrix) == "HELICONIA01", "#BA0022", "grey80")),
                                  bor.col.interaction = t(ifelse(matrix[,] > 0 & rownames(matrix) == "HELICONIA01", "#BA0022", "black")))
    
    plot <- grDevices::recordPlot()
    plot <- cowplot::plot_grid(plot)
    plot.new()
    
  }
  
  return(plot)
  
}