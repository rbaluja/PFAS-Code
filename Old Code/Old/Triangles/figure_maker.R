t_fun = function(radius, n){
  theta = pi/4
  x = 1
  for (j in 1:n){
    x = x + 2
    a = list('x' = (radius * cos(theta + (pi * x)/n)), 'y' = radius * sin(theta + (pi * x)/n)) %>%
      as.data.frame() %>% 
      st_as_sf(coords = c('x', 'y'), crs = 3488) %>% 
      st_transform(4326)
    
    assign(paste0('a_', j), a)
  }
  
  o = list('x' = 0, 'y' = 0) %>%
    as.data.frame() %>% 
    st_as_sf(coords = c('x', 'y'), crs = 3488) %>% 
    st_transform(4326)
  
  
  l = vector(mode = 'list', length = n)
  for (j in 1:n){
    if (j == 1){
      tri = rbind(o, eval(parse(text = paste0('a_', n))), a_1, o)
      tri = tri %>%
        st_set_crs(4326) %>%
        st_transform(32632) %>% #helps with casting
        dplyr::summarise() %>%
        st_cast('POLYGON') %>% 
        st_transform(4326)
      l[[j]] = tri
    }else{
      tri = rbind(o, eval(parse(text = paste0('a_', j))), eval(parse(text = paste0('a_', (j-1)))), o)
      tri = tri %>%
        st_set_crs(4326) %>%
        st_transform(32632) %>% #helps with casting
        dplyr::summarise() %>%
        st_cast('POLYGON') %>% 
        st_transform(4326)
      l[[j]] = tri
    }
  }
  
  l = bind_rows(setNames(l, seq_along(l)), .id = "triangle")
  return(l)
}

e_st_diff = function(x, y){
  z =st_difference(x, st_union(y))
  return(z)
}


#make triangles
sf_use_s2(TRUE)
sections = c(2, 4, 6, 8)

for (i in seq_along(sections)){
  a = t_fun(sections[i], n_triangles)
  assign(paste0("triangle_s", i), a)
}

fdf = tibble('id' = 1:n_triangles)

for (k in 1:length(sections)){
  tr = get(paste0("triangle_s", k))
  c_name = paste0("tr_s", sections[k])
  fdf[[c_name]] <- tr$geometry
}

fdf2 = fdf
for (k in 2:length(sections)){
  c_name = paste0("tr_s", sections[k])
  c1_name =paste0("tr_s", sections[k-1])
  fdf2[[c_name]] <- e_st_diff(fdf[[c_name]], st_union(fdf[[c1_name]]))
  
}

fdf = fdf2 %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("tr"), 
                      names_to = "c_level", 
                      values_to = "geometry") %>% 
  st_as_sf()

fdf$lev = parse_number(sapply(strsplit(fdf$c_level, "_"), "[", 2))
fdf = fdf %>% dplyr::rename(tr = id)
#replace 2 with 1
fdf$lev = ifelse(fdf$lev == 2, 1, fdf$lev)



#first plot n_sites
var = 'nsites'
names_coef = names(fs_reg$coefficients)[endsWith(names(fs_reg$coefficients), var)]
coef = fs_reg$coeftable[names_coef, 1]
t_val = fs_reg$coeftable[names_coef, 3]

coef_df = data.frame("coef" = coef, "name" = names_coef, "t_val" = t_val)
coef_df$tr = sapply(strsplit(coef_df$name, "_"), "[", 1)
coef_df$tr = parse_number(coef_df$tr)

coef_df$lev = parse_number(sapply(strsplit(coef_df$name, "_"), "[", 2))

fdf = fdf %>% 
  left_join(coef_df %>% dplyr::select(coef, t_val, tr, lev))
fdf$coef = round(fdf$coef, 2)

fdf$stars = ifelse(abs(fdf$t_val) >= 2.56, 3, 
                   ifelse(abs(fdf$t_val) >= 1.96, 2, 
                          ifelse(abs(fdf$t_val) >= 1.645, 1, 0)))
fdf$al = ifelse(fdf$stars == 3, 0.9, 
                ifelse(fdf$stars == 2, 0.7, ifelse(
                  fdf$stars == 1, 0.5, 0.3
                )))

nsites_plot = ggplot() + geom_sf(data = fdf, aes(fill = coef, alpha = al)) +
  labs(fill = "Coef.") +
  # geom_sf_text(
  #   data = fdf, 
  #   aes(label = coef), 
  #   size = 2.5) +
  scale_fill_distiller(palette = "Oranges", trans = "reverse") +theme_minimal() + 
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) + 
  ggtitle("First Stage Num Sites")+ guides(alpha = "none")

#now plot pfas
var = 'pfas'
names_coef = names(fs_reg$coefficients)[endsWith(names(fs_reg$coefficients), var)]
coef = fs_reg$coeftable[names_coef, 1]
t_val = fs_reg$coeftable[names_coef, 3]

coef_df = data.frame("coef" = coef, "name" = names_coef, "t_val" = t_val)
coef_df$tr = sapply(strsplit(coef_df$name, "_"), "[", 1)
coef_df$tr = parse_number(coef_df$tr)

coef_df$lev = parse_number(sapply(strsplit(coef_df$name, "_"), "[", 2))

fdf = fdf %>% 
  dplyr::select(!c(coef, t_val)) %>%
  left_join(coef_df %>% dplyr::select(coef, t_val, tr, lev))

fdf$stars = ifelse(abs(fdf$t_val) >= 2.56, 3, 
                   ifelse(abs(fdf$t_val) >= 1.96, 2, 
                          ifelse(abs(fdf$t_val) >= 1.645, 1, 0)))
fdf$al = ifelse(fdf$stars == 3, 0.9, 
                ifelse(fdf$stars == 2, 0.7, ifelse(
                  fdf$stars == 1, 0.5, 0.3
                )))



pfas_plot = ggplot() + geom_sf(data = fdf, aes(fill = coef, alpha = al)) +
  labs(fill = "Coef.") +
  # geom_sf_text(
  #   data = fdf, 
  #   aes(label = format(coef, scientific = T, digits = 1)), 
  #   size = 2) +
  scale_fill_distiller(palette = "Greens", trans = "reverse") +theme_minimal() + 
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) + 
  ggtitle("First Stage PFAS")+ guides(alpha = "none")

#now plot pfas
var = '^2)'
names_coef = names(fs_reg$coefficients)[endsWith(names(fs_reg$coefficients), var)]
coef = fs_reg$coeftable[names_coef, 1]
t_val = fs_reg$coeftable[names_coef, 3]

coef_df = data.frame("coef" = coef, "name" = names_coef, "t_val" = t_val)
coef_df$tr = sapply(strsplit(coef_df$name, "_"), "[", 1)
coef_df$tr = parse_number(coef_df$tr)

coef_df$lev = parse_number(sapply(strsplit(coef_df$name, "_"), "[", 2))

fdf = fdf %>% 
  dplyr::select(!c(coef, t_val)) %>%
  left_join(coef_df %>% dplyr::select(coef, t_val, tr, lev))


fdf$stars = ifelse(abs(fdf$t_val) >= 2.56, 3, 
                   ifelse(abs(fdf$t_val) >= 1.96, 2, 
                          ifelse(abs(fdf$t_val) >= 1.645, 1, 0)))
fdf$al = ifelse(fdf$stars == 3, 0.9, 
                   ifelse(fdf$stars == 2, 0.7, ifelse(
                     fdf$stars == 1, 0.5, 0.3
                   )))
pfas2_plot = ggplot() + geom_sf(data = fdf, aes(fill = coef, alpha = al)) +
  labs(fill = "Coef.") +
  # geom_sf_text(
  #   data = fdf, 
  #   aes(label = format(coef, scientific = T, digits = 2)), 
  #   size = 2) +
  scale_fill_distiller(palette = "Blues", trans = "reverse") +theme_minimal() + 
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) + 
  ggtitle(bquote("First Stage " (PFAS^2))) + guides(alpha = "none")

ggarrange(nsites_plot, ggarrange(pfas_plot, pfas2_plot), nrow = 2)





figure_fun = function(reg, var, radius, n_triangles, sections, color = "Reds", title = "none"){
  
  #first build triangle dataframe
  for (i in seq_along(sections)){
    a = t_fun(sections[i], n_triangles)
    assign(paste0("triangle_s", i), a)
  }
  
  fdf = tibble('id' = 1:n_triangles)
  
  for (k in 1:length(sections)){
    tr = get(paste0("triangle_s", k))
    c_name = paste0("tr_s", sections[k])
    fdf[[c_name]] <- tr$geometry
  }
  
  fdf2 = fdf
  for (k in 2:length(sections)){
    c_name = paste0("tr_s", sections[k])
    c1_name =paste0("tr_s", sections[k-1])
    fdf2[[c_name]] <- e_st_diff(fdf[[c_name]], st_union(fdf[[c1_name]]))
    
  }
  
  fdf = fdf2 %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("tr"), 
                        names_to = "c_level", 
                        values_to = "geometry") %>% 
    st_as_sf()
  
  fdf$lev = parse_number(sapply(strsplit(fdf$c_level, "_"), "[", 2))
  fdf = fdf %>% dplyr::rename(tr = id)
  #replace 2 with 1 (if present, helps with figure)
  fdf$lev = ifelse(fdf$lev == 2, 1, fdf$lev)
  
  
  #get coeficients & t_value
  names_coef = names(reg$coefficients)[endsWith(names(reg$coefficients), var)]
  coef = reg$coeftable[names_coef, 1]
  t_val = reg$coeftable[names_coef, 3]
  
  coef_df = data.frame("coef" = coef, "name" = names_coef, "t_val" = t_val)
  coef_df$tr = sapply(strsplit(coef_df$name, "_"), "[", 1)
  coef_df$tr = parse_number(coef_df$tr)
  
  coef_df$lev = parse_number(sapply(strsplit(coef_df$name, "_"), "[", 2))
  
  fdf = fdf %>% 
    left_join(coef_df %>% dplyr::select(coef, t_val, tr, lev))
  
  fdf$stars = ifelse(abs(fdf$t_val) >= 2.56, 3, 
                     ifelse(abs(fdf$t_val) >= 1.96, 2, 
                            ifelse(abs(fdf$t_val) >= 1.645, 1, 0)))
  fdf$al = ifelse(fdf$stars == 3, 0.9, 
                  ifelse(fdf$stars == 2, 0.7, ifelse(
                    fdf$stars == 1, 0.5, 0.3
                  )))
  
  p = ggplot() + geom_sf(data = fdf, aes(fill = coef, alpha = al)) +
    labs(fill = "Coef.") +
    # geom_sf_text(
    #   data = fdf, 
    #   aes(label = format(coef, scientific = T, digits = 1)), 
    #   size = 2) +
    scale_fill_distiller(palette = color) +theme_minimal() + 
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),  #remove y axis ticks
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
    ) + guides(alpha = "none")
  
  if (title != "none"){
    p = p + ggtitle(title)
  }
  
  return(p)
}


nsites_plot = figure_fun(ss_reg, "nsites", meters, 
                n_triangles, c(2, 4, 6, 8), color = "YlOrRd", 
                title = "Number of Sites on Gestation")

pfas_plot = figure_fun(ss_reg, "pfas", meters, 
                                n_triangles, c(2, 4, 6, 8), color = "Blues", 
                                title = "PFAS Level on Gestation") 



ggarrange(nsites_plot, pfas_plot)



