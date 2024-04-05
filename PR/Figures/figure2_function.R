figure2_fun = function(data, category, keep_x, header, ti){
  if (category == "Any (<37 Weeks)" | category == "Any (<2500g)"){
    col = "dodgerblue"
  }else if (category == "Slightly (32-36 Weeks)" | category == "Slightly (1500-2499g)"){
    col = "coral"
  }else if (category == "Moderately (28-31 Weeks)" | category == "Moderately (1000-1499g)"){
    col = "darkseagreen"
  }else if (category == "Very (<28 Weeks)" | category == "Very (<1000g)"){
    col = "orchid4"
  }
  
  if (keep_x){
    pany1 = data %>% 
      ggplot(aes(y = Check)) + 
      geom_point(aes(x=Estimate), color = col, shape=16, size=8) +
      geom_linerange(aes(xmin=d_lower, xmax=d_upper), size = 1) + 
      geom_vline(xintercept = 0, linetype="dashed") +
      theme_classic() + ylab(category) + xlab("") + 
      theme(axis.line.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank(),
            axis.title.y= element_blank(), 
            axis.text.x = element_text(size = 30), 
            axis.title.x = element_text(size = 40)) + 
      guides(color = "none") + xlim(c(-400, 400)) + xlab(ti)
  }else{
    pany1 = data %>% 
      ggplot(aes(y = Check)) + 
      geom_point(aes(x=Estimate), color = col, shape=16, size=8) +
      geom_linerange(aes(xmin=d_lower, xmax=d_upper), size = 1) + 
      geom_vline(xintercept = 0, linetype="dashed") +
      theme_classic() + ylab(category) + xlab("") + 
      theme(axis.line = element_blank(),
            axis.ticks= element_blank(),
            axis.text= element_blank(),
            axis.title= element_blank()) + 
      guides(color = "none") + xlim(c(-400, 400))
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
    res_plot$Check = factor(res_plot$Check, c("Contamination Source Fixed Effect", "No Medical Controls", 
                                            "No Demographics", "Relaxed Upgradient Definition", 
                                            "Drop Cont. Sources w/in 5km of State Border", 
                                             "Drop After 2015",
                                            "Drop within 1km", "Baseline" , "Model"
    ))
    res_plot$model = res_plot$Check
  }else{
    res_plot$Check = factor(res_plot$Check, c("Contamination Source Fixed Effect", "No Medical Controls", 
                                              "No Demographics", "Relaxed Upgradient Definition", 
                                              "Drop Cont. Sources w/in 5km of State Border", 
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
                fontface = "plain", size = 10)
  }else{
    p_left =
      res_plot  %>%
      ggplot(aes(y = model)) 
    p_left = 
      p_left +
      geom_text(aes(x = 0, label = Check), hjust = 0, 
                fontface = ifelse(res_plot$model == "Model", "bold", "plain"), size = 10)
  }
  
  
  
  p_left =
    p_left +
    theme_void() +
    coord_cartesian(xlim = c(0, 10))
  
  if (!header){
    p_right = res_plot %>%
      ggplot() +
      geom_text(
        aes(x = 0, y = model, label = estimate_lab),
        hjust = 0,
        fontface = "plain", size = 10)
    p_right =  p_right + 
      geom_text(
        aes(x = 1.5, y = model, label = pval),
        hjust = 0,
        fontface = "plain", size = 10) + 
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
        fontface = ifelse(res_plot$estimate_lab == "Estimate (95% CI)", "bold", "plain"), size = 10
      )
    p_right =  p_right + 
      geom_text(
        aes(x = 1.5, y = model, label = pval),
        hjust = 0,
        fontface = ifelse(res_plot$pval == "p-value", "bold", "plain"), 
        size = 10
      ) + 
      coord_cartesian(xlim = c(0, 2)) + 
      theme_void()+ 
      theme(axis.line.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank(),
            axis.title.y= element_blank())
  }
  
  
  if (header){
    layout = c(
      area(t = 0, l = 0, b = 30, r = 25), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
      area(t = 3.7, l = 26, b = 30, r = 45), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
      area(t = 0, l = 46, b = 30, r = 60) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
    ) 
  }else{
    layout = c(
      area(t = 0, l = 0, b = 30, r = 25), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
      area(t = 0, l = 26, b = 30, r = 45), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
      area(t = 0, l = 46, b = 30, r = 60) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
    )
  }
  # final plot arrangement
  f = p_left + pany1 + p_right + plot_layout(design = layout)
  
  return(f)
  
} 

