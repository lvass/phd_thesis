# Model results heatmaps functions

## Functions
### Get results function

get_full_results <- function(model, data, type, threshold){
  
  if(type == "ours"){
    
    n_assay <- length(unique(data$assay))
    
    part1_res <- as_tibble(fitted(model,
                                  newdata = NULL,
                                  re_formula = NULL,
                                  summary = F,
                                  nlpar = "interaction"))
    
    part1_res_trans <- as_tibble(inv_logit_scaled(part1_res))[, 1:n_assay]
    
    ndraws <- dim(part1_res_trans)[1]
    
    part1_res_trans <- part1_res_trans %>%
      mutate(draw = seq(1,ndraws)) %>%
      gather(key = "x_val", value = "value", -draw) %>%
      mutate(x_val = as.numeric(as.character(gsub("V", "", x_val)))) %>%
      arrange(draw, x_val) 
    
    part1_res_withassay <- part1_res_trans %>%
      mutate(assay = rep(unique(data$assay), ndraws)) 
    
    results <- part1_res_withassay %>%
      group_by(assay) %>%
      summarise(var_res = sd(value),
                mean_res = mean(value),
                ci_upp = quantile(value, 0.975),
                ci_low = quantile(value, 0.025)) %>%
      left_join(data, by = "assay") %>%
      spread(key = "am", value = "count") %>%
      rename("pc" = "0",
             "am" = "1")
  }
  
  if(type == "ours_prob"){
    
    if(is.na(threshold)){
      print("No theshold value provided, using 0.01")
      threshold <- 0.01
    }
    
    n_assay <- length(unique(data$assay))
    
    part1_res <- as_tibble(fitted(model,
                                  newdata = NULL,
                                  re_formula = NULL,
                                  summary = F,
                                  nlpar = "interaction"))
    
    part1_res_trans <- as_tibble(inv_logit_scaled(part1_res))[, 1:n_assay]
    
    ndraws <- dim(part1_res_trans)[1]
    
    part1_res_trans <- part1_res_trans %>%
      mutate(draw = seq(1,ndraws)) %>%
      gather(key = "x_val", value = "value", -draw) %>%
      mutate(x_val = as.numeric(as.character(gsub("V", "", x_val)))) %>%
      arrange(draw, x_val) 
    
    part1_res_withassay <- part1_res_trans %>%
      mutate(assay = rep(unique(data$assay), ndraws)) 
    
    results <- part1_res_withassay %>%
      left_join(data, by = "assay") %>%
      spread(key = "am", value = "count") %>%
      rename("pc" = "0",
             "am" = "1")
    
    results <- results %>%
      mutate(outcome = ifelse(value > threshold, 1, 0)) %>%
      group_by(assay) %>%
      summarise(probability = mean(outcome),
                pc = mean(pc, na.rm=T),
                am = mean(am, na.rm=T)) %>%
      select(assay, probability, pc, am) %>%
      distinct()
    
  }
  
  
  if(type == "schubert"){
    n_assay <- dim(data)[1]
    
    part1_res <- as_tibble(fitted(model,
                                  newdata = NULL,
                                  re_formula = NULL,
                                  summary = F))
    
    part1_res_trans <- part1_res[1:n_assay,]
    
    ndraws <- dim(part1_res_trans)[1]
    
    part1_res_trans <- part1_res_trans %>%
      mutate(draw = seq(1,ndraws)) %>%
      gather(key = "x_val", value = "value", -draw) %>%
      mutate(x_val = as.numeric(as.character(gsub("V", "", x_val)))) %>%
      arrange(x_val)
    
    results <- part1_res_trans %>%
      group_by(x_val) %>%
      summarise(var_res = sd(value),
                mean_res = mean(value),
                ci_upp = quantile(value, 0.975),
                ci_low = quantile(value, 0.025))
    
    results <- cbind(results, data)
  }
  
  if(type == "schubert_thres"){
    
    n_assay <- dim(data)[1]
    
    part1_res <- as_tibble(fitted(model,
                                  newdata = NULL,
                                  re_formula = NULL,
                                  summary = F))
    
    part1_res_trans <- part1_res[1:n_assay,]
    
    ndraws <- dim(part1_res_trans)[1]
    
    results <-  part1_res_trans %>%
      mutate(draw = seq(1,ndraws)) %>%
      gather(key = "x_val", value = "value", -draw) %>%
      mutate(x_val = as.numeric(as.character(gsub("V", "", x_val)))) %>%
      arrange(x_val) %>%
      mutate(outcome = ifelse(value > threshold, 1, 0)) %>%
      group_by(x_val) %>%
      summarise(probability = mean(outcome)) %>%
      select(x_val, probability) %>%
      distinct()
    
    results <- cbind(results, data)
    
  }
  
  if(type == "logisitc"){
    n_assay <- dim(data)[1]
    
    part1_res <- as_tibble(fitted(model,
                                  newdata = NULL,
                                  re_formula = NULL,
                                  summary = F))
    
    part1_res_trans <- part1_res[1:n_assay,]
    
    ndraws <- dim(part1_res_trans)[1]
    
    part1_res_trans <- part1_res_trans %>%
      mutate(draw = seq(1,ndraws)) %>%
      gather(key = "x_val", value = "value", -draw) %>%
      mutate(x_val = as.numeric(as.character(gsub("V", "", x_val)))) %>%
      arrange(x_val)
    
    results <- part1_res_trans %>%
      group_by(x_val) %>%
      summarise(var_res = sd(value),
                mean_res = mean(value),
                ci_upp = quantile(value, 0.975),
                ci_low = quantile(value, 0.025))
    
    results <- cbind(results, data)
  }
  
  if(type == "beta"){
    n_assay <- dim(data)[1]
    
    part1_res <- as_tibble(fitted(model,
                                  newdata = NULL,
                                  re_formula = NULL,
                                  summary = F))
    
    part1_res_trans <- part1_res[1:n_assay,]
    
    ndraws <- dim(part1_res_trans)[1]
    
    part1_res_trans <- part1_res_trans %>%
      mutate(draw = seq(1,ndraws)) %>%
      gather(key = "x_val", value = "value", -draw) %>%
      mutate(x_val = as.numeric(as.character(gsub("V", "", x_val)))) %>%
      arrange(x_val)
    
    results <- part1_res_trans %>%
      group_by(x_val) %>%
      summarise(var_res = sd(value),
                mean_res = mean(value),
                ci_upp = quantile(value, 0.975),
                ci_low = quantile(value, 0.025))
    
    results <- cbind(results, data)
  }
  
  return(results)
}



### Make heatmap function

#square function
sq<-function(x){
  x^2
}
#inverse square function (square root)
isq<-function(x){
  x<-ifelse(x<0, 0, x)
  sqrt(x)
}


plot_heatmap <- function(model_results, style, am_limit, pc_limit, point_size, zeros){
  #point_size for plot with full data = 3.6
  #model_results 
  
  if(zeros == "only"){
    model_results <- model_results %>%
      filter(am == 0)
  }
  
  if(zeros == "without"){
    model_results <- model_results %>%
      filter(am != 0)
  }
  
  # if zeros == mixed, do nothing
  
  if(style != "prob_res"){
    heatmap_data <- model_results %>%
      group_by(pc, am) %>%
      summarise(mean_res_binned = mean(mean_res),
                mean_var_binned = mean(var_res),
                mean_ci_upp_binned = mean(ci_upp),
                mean_ci_low_binned = mean(ci_low))
    
    
    plot1_no_colour <- heatmap_data %>%
      ggplot () +
      scale_y_continuous(
                         #breaks = c(0,10, 100),
                         limits = c(0,am_limit)) +
      scale_x_continuous(
                         #breaks = c(0,10, 100),
                         limits = c(0,pc_limit))
  }
  
  
  
  # what to plot - res, var or sens?
  if(style == "res"){
    
    plot1 <- plot1_no_colour + 
      geom_point(aes(x = pc, y = am, colour = mean_res_binned), shape = 15, size = point_size) +
      scale_colour_viridis_c(limits = c(0,1))+
      labs(colour = "Resistance") 
    
  }
  
  
  
  if(style == "var_limits"){
    
    plot1 <- plot1_no_colour +
      geom_point(aes(x = pc, y = am, colour = mean_var_binned), shape = 15, size = point_size) +
      scale_colour_viridis_c(trans = "log10",
                             limits = c(2.580287e-07, 0.0002199597)) +
      labs(colour = "Variance") 
    
  }
  
  if(style == "var"){
    
    plot1 <- plot1_no_colour +
      geom_point(aes(x = pc, y = am, colour = mean_var_binned), shape = 15, size = point_size) +
      scale_colour_viridis_c(trans = "log10",
                             option = "plasma") +
      labs(colour = "Variance") 
    
  }
  
  if(style == "ci_diff"){
    
    plot1 <- plot1_no_colour +
      geom_point(aes(x = pc, y = am, colour = mean_ci_upp_binned - mean_ci_low_binned), shape = 15, size = point_size) +
      scale_colour_viridis_c(
                             limits = c(0,1)) +
      labs(colour = "CI diff") 
    
  }

  
  if(style == "sens"){
    
    heatmap_data <- model_results %>%
      group_by(pc, am) %>%
      summarise(mean_res_binned = mean(mean_res),
                mean_var_binned = mean(var_res),
                sensitivity_binned = mean(sens))
    
    plot1_no_colour <- heatmap_data %>%
      ggplot () +
      scale_y_continuous(trans=scales::trans_new(name = "sq", transform = isq, inverse = sq),
                         breaks = c(0,10, 100, 1000),
                         limits = c(0,am_limit)) +
      scale_x_continuous(trans=scales::trans_new(name = "sq", transform = isq, inverse = sq),
                         breaks = c(0,10, 100, 1000, 10000),
                         limits = c(0,pc_limit))
    
    plot1 <- plot1_no_colour +
      geom_point(aes(x = pc, y = am, colour = sensitivity_binned), shape = 15, size = point_size) +
      scale_colour_viridis_c(trans = "log1p") +
      labs(colour = "Sensitivity") 
    
  }
  
  if(style == "binary"){
    
    heatmap_data <- model_results %>%
      group_by(pc, am) %>%
      summarise(binary_binned = mean(amoxR))
    
    plot1_no_colour <- heatmap_data %>%
      ggplot () +
      scale_y_continuous(
                         breaks = c(0,10, 100),
                         limits = c(0,am_limit)) +
      scale_x_continuous(
                         breaks = c(0,10, 100),
                         limits = c(0,pc_limit))
    
    plot1 <- plot1_no_colour +
      geom_point(aes(x = pc, y = am, colour = as.factor(binary_binned)), shape = 15, size = point_size) +
      labs(colour = "Binary resistance") 
    
  }
  
  if(style == "prob_res"){
    # model results should be probability style, our model only

    
    plot1_no_colour <- model_results %>%
      ggplot () +
      scale_y_continuous(#trans=scales::trans_new(name = "sq", transform = isq, inverse = sq),
                         #breaks = c(0,10, 100, 1000),
                         limits = c(0,am_limit)) +
      scale_x_continuous(#trans=scales::trans_new(name = "sq", transform = isq, inverse = sq),
                         #breaks = c(0,10, 100, 1000, 10000),
                         limits = c(0,pc_limit))
    
    plot1 <- plot1_no_colour +
      geom_point(aes(x = pc, y = am, colour = probability), shape = 15, size = point_size) +
      scale_colour_viridis_c(limits = c(0,1)) +
      labs(colour = "Prob. any resistance") 
    
  }
  
  # changing scale for zero include/exclude plots
  if(zeros == "only"){
    plot1 <- plot1 + 
      scale_y_continuous(
        breaks = c(0),
        limits = c(0,0.1))
    
    
    if(style == "var"){
      plot1 <- plot1
    }
    
  }
  
  if(zeros == "without"){
    model_results <- model_results %>%
      filter(am != 0)
    plot1 <- plot1 + 
      scale_y_continuous(#trans=scales::trans_new(name = "sq", transform = isq, inverse = sq),
                         #breaks = c(1,10, 100, 1000),
                         limits = c(1,am_limit)) 
    
    if(style == "var"){
      plot1 <- plot1 
    }
    
  } 
  
  finished_plot <- plot1 +
    xlab("Plain count (sqrt scale)") +
    ylab("Resistant count (sqrt scale)") +
    theme_minimal()
  
  return(finished_plot)
  
}

plot_full_heatmap <- function(model, type, data){
  
  model_test_results <- get_full_results(model, data, type = type)
  
  res_plot_no_zeros <- plot_heatmap(model_test_results, "res", am_limit = 100, pc_limit = 100, point_size = 7.2, zeros = "without") +
    ggtitle("") +
    labs(colour = "Prop. resistant",
         x = "",
         y = "") +
    theme_minimal() + 
    theme(legend.position = "none") +
    scale_colour_viridis_c(trans ="log10",
                           limits = c(0.001,1),
                           na.value = "#440154",
                           option = 'plasma')
  
  
  res_plot_zeros <- plot_heatmap(model_test_results, "res", am_limit = 100, pc_limit = 100, point_size = 7.5, zeros = "only") +
    ggtitle("") +
    labs(colour = "Prop. resistant",
         y = "",
         x = "") +
    theme_minimal() +
    theme(legend.position = "none")+
    scale_colour_viridis_c(trans ="log10",
                           limits = c(0.001,1),
                           na.value = "#440154",
                           option = 'plasma')
  
  
  
  # cis 
  
  ci_diff_plot_no_zeros <- plot_heatmap(model_test_results, "ci_diff", am_limit = 100, pc_limit = 100, point_size =  7.2, zeros = "") +
    ggtitle("") +
    labs(colour = "95% CI",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.position = "none")+
    scale_colour_viridis_c(trans ="log10",
                           limits = c(0.01,1),
                           na.value = "#440154",
                           option = 'plasma')
  
  ci_diff_plot_zeros <- plot_heatmap(model_test_results, "ci_diff", am_limit = 100, pc_limit = 100, point_size =  7.5, zeros = "only") +
    ggtitle("") +
    labs(colour = "95% CI",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_colour_viridis_c(trans ="log10",
                           limits = c(0.01,1),
                           na.value = "#440154",
                           option = 'plasma')
  
  gridExtra::grid.arrange(res_plot_no_zeros, res_plot_zeros, nrow = 2, heights = c(10,4))
  gridExtra::grid.arrange(ci_diff_plot_no_zeros, ci_diff_plot_zeros, nrow = 2, heights = c(10,4))
  
  ci_diff_plot_zeros + theme(legend.position = "left") 
  
}



