figure2_fun = function(data, category, keep_x, header, ti, left){
  if (category == "Any" | category == "Any Low-Birthweight (Full-Term)"){
    col = "dodgerblue"
  }else if (category == "Slightly" | category == "Slightly"){
    col = "coral"
  }else if (category == "Moderately" | category == "Moderately"){
    col = "darkseagreen"
  }else if (category == "Very" | category == "Very"){
    col = "orchid4"
  }else if (category == "Stillbirth"){
    col = "firebrick"
  }
  
  if (keep_x){
    pany1 = data %>% 
      ggplot(aes(y = Check)) + 
      geom_point(aes(x=Estimate), color = col, shape=16, size=18) +
      geom_linerange(aes(xmin=d_lower, xmax=d_upper), size = 3) + 
      geom_vline(xintercept = 0, linetype="dashed") +
      theme_classic() + ylab(category) + xlab("") + 
      theme(axis.line.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank(),
            axis.title.y= element_blank(), 
            axis.text.x = element_text(size = 46), 
            axis.title.x = element_text(size = 50)) + 
      guides(color = "none") + xlim(c(-300, 400)) + xlab("Change, as % of Population Base Rate")
  }else{
    pany1 = data %>% 
      ggplot(aes(y = Check)) + 
      geom_point(aes(x=Estimate), color = col, shape=16, size=18) +
      geom_linerange(aes(xmin=d_lower, xmax=d_upper), size = 3) + 
      geom_vline(xintercept = 0, linetype="dashed") +
      theme_classic() + ylab(category) + xlab("") + 
      theme(axis.line = element_blank(),
            axis.ticks= element_blank(),
            axis.text= element_blank(),
            axis.title= element_blank()) + 
      guides(color = "none") + xlim(c(-300, 400))
  }
  
  # if (ti != FALSE){
  #   pany1 = pany1 + labs(title = ti) + 
  #     theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))
  # }
  
  # wrangle results into pre-plotting table form
  res_plot <-data %>%
    # Round estimates and 95% CIs to 2 decimal places for journal specifications
    # Ensure that a decimal point is included, even if there are only zeros after it
    dplyr::mutate(across(
      .cols = c(Estimate, d_lower, d_upper),
      .fns = ~ sprintf("%0.2f", round(.x, 2))
    ),
    # add an "-" between HR estimate confidence intervals
    estimate_lab = paste0(Estimate, " (", d_lower, "-", d_upper, ")")) %>%
    # round p-values to two decimal places, except in cases where p < .001
    mutate(pval = case_when(
      pval < .001 ~ "<0.001",
      round(pval, 2) == .05 ~ as.character(round(pval,3)),
      pval < .01 ~ str_pad( # if less than .01, go one more decimal place
        as.character(round(pval, 3)),
        width = 4,
        pad = "0",
        side = "right"
      ),
      TRUE ~ str_pad( # otherwise just round to 2 decimal places and pad string so that .2 reads as 0.20
        as.character(round(pval, 2)),
        width = 4,
        pad = "0",
        side = "right"
      )
    )) 
    
  if (header){
    res_plot = res_plot %>%
    bind_rows(
      data.frame(
        Check = "Model",
        estimate_lab = "Estimate (95% CI)",
        d_lower = "",
        d_upper = "",
        pval = "p-value"
      )
    ) 
    res_plot$Check = factor(res_plot$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                             "Drop After 2015",
                                            "Drop within 1km", "Baseline" , "Model"
    ))
    res_plot$model = res_plot$Check
  }else{
    res_plot$Check = factor(res_plot$Check, c("Site Fixed Effects", "No Medical Controls", 
                                              "No Demographics", "Relax Upgradient Def'n", 
                                              "Drop Border Sites", 
                                               "Drop After 2015",
                                              "Drop within 1km", "Baseline"
    ))
    res_plot$model = res_plot$Check
  }
    
  
  
  if (!header){
    p_left =
      res_plot  %>%
      ggplot(aes(y = model)) 
    p_left = 
      p_left +
      geom_text(aes(x = 0, label = Check), hjust = 0, 
                fontface = "plain", size = 20)
  }else{
    p_left =
      res_plot  %>%
      ggplot(aes(y = model)) 
    p_left = 
      p_left +
      geom_text(aes(x = 0, label = Check), hjust = 0, 
                fontface = ifelse(res_plot$model == "Model", "bold", "plain"), size = 20)
  }
  
  
  
  p_left =
    p_left +
    coord_cartesian(xlim = c(0, 10)) + labs(y = ti, x = "") + 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 60))
  
  if (!header){
    p_right = res_plot %>%
      ggplot() +
      geom_text(
        aes(x = 0, y = model, label = estimate_lab),
        hjust = 0,
        fontface = "plain", size = 20)
    p_right =  p_right + 
      geom_text(
        aes(x = 1.5, y = model, label = pval),
        hjust = 0,
        fontface = "plain", size = 20) + 
      coord_cartesian(xlim = c(0, 2)) + 
      theme_void()+ 
      theme(axis.line.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank(),
            axis.title.y= element_blank())
  }else{
    p_right = res_plot %>%
      ggplot() +
      geom_text(
        aes(x = 0, y = model, label = estimate_lab),
        hjust = 0,
        fontface = ifelse(res_plot$estimate_lab == "Estimate (95% CI)", "bold", "plain"), size = 20
      )
    p_right =  p_right + 
      geom_text(
        aes(x = 1.5, y = model, label = pval),
        hjust = 0,
        fontface = ifelse(res_plot$pval == "p-value", "bold", "plain"), 
        size = 20
      ) + 
      coord_cartesian(xlim = c(0, 2)) + 
      theme_void()+ 
      theme(axis.line.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank(),
            axis.title.y= element_blank())
  }
  
  if (left){
    if (header){
      layout = c(
        area(t = 0, l = 0, b = 30, r = 9), 
        area(t = 5, l = 10, b = 30, r = 19), 
        area(t = 0, l = 20, b = 30, r = 31) 
      ) 
    }else{
      layout = c(
        area(t = 0, l = 0, b = 30, r = 9), 
        area(t = 0, l = 10, b = 30, r = 19), 
        area(t = 0, l = 20, b = 30, r = 31) 
      ) 
    }
    f = p_left + pany1 + p_right + plot_layout(design = layout) 
  }else{
    if (header){
      layout = c(
        area(t = 5, l = 0, b = 30, r = 14), 
        area(t = 0, l = 15, b = 30, r = 35) 
      ) 
    }else{
      layout = c(
        area(t = 0, l = 0, b = 30, r = 14), 
        area(t = 0, l = 15, b = 30, r = 35) 
      ) 
    }
    f = pany1 + p_right + plot_layout(design = layout) 
    
  }
  
  return(f)
  
} 

