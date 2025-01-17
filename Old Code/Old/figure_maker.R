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
sections = c(1, 4, 6, 8)

for (i in seq_along(sections)){
  a = t_fun(sections[i], n_triangles)
  assign(paste0("triangle_s", i), a)
}

fdf = tibble('id' = 1:n_triangles)

for (k in 1:length(sections)){
  tr = get(paste0("triangle_s", k))
  c_name = paste0("tr_s", k)
  fdf[[c_name]] <- tr$geometry
}

fdf2 = fdf
for (k in 2:length(sections)){
  c_name = paste0("tr_s", k)
  c1_name =paste0("tr_s", k-1)
  fdf2[[c1_name]] <- e_st_diff(fdf[[c_name]], st_union(fdf[[c1_name]]))
  
}

fdf = fdf2 %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("tr"), 
                      names_to = "c_level", 
                      values_to = "geometry") %>% 
  st_as_sf()

ggplot() + geom_sf(data = fdf)
  







# Create a data frame for the regression outputs
regression_df <- data.frame(section = factor(1:n, labels = paste("Section", 1:n)),
                            output = runif(n))

# Create the plot
ggplot() +
  geom_polygon(data = circle_df, aes(x, y), fill = "lightgray", color = "black") +
  geom_polygon(data = triangle_df, aes(x, y, group = rep(1:n, each = 3)),
               fill = "white", color = "black") +
  geom_text(data = regression_df, aes(label = output, x = 0, y = 0), size = 4) +
  coord_equal() +
  theme_void()
