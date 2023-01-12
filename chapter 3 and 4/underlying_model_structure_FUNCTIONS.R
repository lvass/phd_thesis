### Underlying model structure - FUNCTIONS
# relates to "underlying_model_structure.Rmd"


### Function for randomly subsetting the data in a balanced way to preserve ratio of zero:non zero AM-added counts

balanced_random_sample <- function(data, am_name, per_nonzero, nsamples){
  
  set.seed(42)
  
  tetra_data <- data %>%
    filter(plate %in% am_name)
  
  # zeros
  tetra_zeros <- tetra_data %>%
    filter(count == 0)
  
  n <- dim(tetra_zeros)[1]
  
  random_subset <- round(sample(1:n, round(nsamples*(1-per_nonzero)), replace=FALSE))
  random_subset_names <- as.character(unique(tetra_zeros$assay)[random_subset])
  
  tetra_zeros <- tetra_zeros %>%
    filter(assay %in% c(random_subset_names))
  
  # non zeros
  tetra_non <- tetra_data %>%
    filter(count != 0)
  
  n <- dim(tetra_non)[1]
  
  random_subset <- round(sample(1:n, round(nsamples*per_nonzero), replace=FALSE))
  random_subset_names <- as.character(unique(tetra_non$assay)[random_subset])
  
  tetra_non <- tetra_non %>%
    filter(assay %in% c(random_subset_names))
  
  tetra_data <- rbind(tetra_zeros, tetra_non)
  
  # add plains back in
  
  plain_data <- data %>%
    filter(plate == "plain") %>%
    filter(assay %in% as.character(tetra_data$assay))
  
  if(dim(plain_data)[1] == dim(tetra_data)[1]){
    tetra_data <- rbind(plain_data, tetra_data)
    
    return(tetra_data)
  }
  
  else{
    return("AHH")
  }
  
}


### Function for making an aligned multi plot figure using gridextra

make_subplot <-  function(plot1, plot2, plot3, arrangement, filename, h, w){
  
  # only 2 plots
  if(is.null(plot3)){
    # x
    new_minx <- round(min(layer_data(plot1, 1)$xmin, layer_data(plot2, 1)$xmin), 1)
    new_maxx <- round(max(layer_data(plot1, 1)$xmax, layer_data(plot2, 1)$xmax), 1)
    # y
    new_miny <- round(min(layer_data(plot1, 1)$ymin, layer_data(plot2, 1)$ymin), 1)
    new_maxy <- round(max(layer_data(plot1, 1)$ymax, layer_data(plot2, 1)$ymax), 1)
    
    plot1 <- plot1 +
      xlim(new_minx, new_maxx) +
      ylim(new_miny, new_maxy) 
    plot2 <- plot2 +
      xlim(new_minx, new_maxx) +
      ylim(new_miny, new_maxy)
    
  }
  # else do all 3 plots
  else{
    # x
    new_minx <- round(min(layer_data(plot1, 1)$xmin, layer_data(plot2, 1)$xmin, layer_data(plot3, 1)$xmin), 1)
    new_maxx <- round(max(layer_data(plot1, 1)$xmax, layer_data(plot2, 1)$xmax, layer_data(plot3, 1)$xmax), 1)
    # y
    new_miny <- round(min(layer_data(plot1, 1)$ymin, layer_data(plot2, 1)$ymin, layer_data(plot3, 1)$ymin), 1)
    new_maxy <- round(max(layer_data(plot1, 1)$ymax, layer_data(plot2, 1)$ymax, layer_data(plot3, 1)$ymax), 1)
    
    plot1 <- plot1 +
      xlim(new_minx, new_maxx) +
      ylim(new_miny, new_maxy) 
    plot2 <- plot2 +
      xlim(new_minx, new_maxx) +
      ylim(new_miny, new_maxy)
    plot3 <- plot2 +
      xlim(new_minx, new_maxx) +
      ylim(new_miny, new_maxy)
    
  }
  
  # sort arrangement
  if(arrangement == "v"){
    if(is.null(plot3)){
      final_plot <- gridExtra::grid.arrange(plot1 + xlab(""),
                                            plot2,
                                            ncol = 1)
    }
    else{
      final_plot <- gridExtra::grid.arrange(plot1 + xlab(""),
                                            plot2 + xlab(""),
                                            plot3,
                                            ncol = 1)
    }
    
  }
  
  if(arrangement == "h"){
    if(is.null(plot3)){
      final_plot <- gridExtra::grid.arrange(plot1,
                                            plot2 + ylab(""),
                                            ncol = 2)
    }
    else{
      final_plot <- gridExtra::grid.arrange(plot1,
                                            plot2 + ylab(""),
                                            plot3 + ylab(""),
                                            ncol = 3)
    }
    
  }
  
  ggsave(final_plot, filename, height = h, width = w)
  return(final_plot)
  
}
