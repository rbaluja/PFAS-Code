library(sf)
inner_radius = 1

#first create inner circle
circle = st_buffer(st_point(c(0, 0)), dist = inner_radius)

#create first triangle (this creats a larger triangle and intersects with the circle to get the nice arc shape)
t1 = list(rbind(c(0, 0), 
                c(inner_radius, inner_radius), 
                c(-inner_radius, inner_radius), 
                c(0, 0))) %>% 
  st_polygon() 

t1 = st_intersection(circle, t1)

t2 = list(rbind(c(0, 0), 
                c(-inner_radius, inner_radius), 
                c(-inner_radius, -inner_radius), 
                c(0, 0))) %>% 
  st_polygon()

t2 = st_intersection(circle, t2)

t3 = list(rbind(c(0, 0), 
                c(-inner_radius, -inner_radius), 
                c(inner_radius, -inner_radius), 
                c(0, 0))) %>% 
  st_polygon()

t3 = st_intersection(circle, t3)

t4 = list(rbind(c(0, 0), 
                c(inner_radius, inner_radius), 
                c(inner_radius, -inner_radius), 
                c(0, 0))) %>% 
  st_polygon()

t4 = st_intersection(circle, t4)

inner_t = data.frame(id = c("t1", "t2", "t3", "t4"))
inner_t$outer = rep(0, 4)
inner_t$middle = rep(0, 4)
inner_t$triangle = 1:4
inner_t$geometry = list(t1, t2, t3, t4)
inner_t = st_as_sf(inner_t)


#make middle shape
middle_radius = 3


circle = st_buffer(st_point(c(0, 0)), dist = middle_radius)

#create first triangle (this creats a larger triangle and intersects with the circle to get the nice arc shape)
mt1 = list(rbind(c(0, 0), 
                c(middle_radius, middle_radius), 
                c(-middle_radius, middle_radius), 
                c(0, 0))) %>% 
  st_polygon() 

mt1 = st_difference(mt1, t1)
mt1 = st_intersection(circle, mt1)

mt2 = list(rbind(c(0, 0), 
                c(-middle_radius, middle_radius), 
                c(-middle_radius, -middle_radius), 
                c(0, 0))) %>% 
  st_polygon()

mt2 = st_difference(mt2, t2)
mt2 = st_intersection(circle, mt2)

mt3 = list(rbind(c(0, 0), 
                c(-middle_radius, -middle_radius), 
                c(middle_radius, -middle_radius), 
                c(0, 0))) %>% 
  st_polygon()

mt3 = st_difference(mt3, t3)
mt3 = st_intersection(circle, mt3)

mt4 = list(rbind(c(0, 0), 
                c(middle_radius, middle_radius), 
                c(middle_radius, -middle_radius), 
                c(0, 0))) %>% 
  st_polygon()

mt4 = st_difference(mt4, t4)
mt4 = st_intersection(circle, mt4)

middle_t = data.frame(id = c("mt1", "mt2", "mt3", "mt4"))
middle_t$outer = rep(0, 4)
middle_t$middle = rep(1, 4)
middle_t$triangle = 1:4
middle_t$geometry = list(mt1, mt2, mt3, mt4)
middle_t = st_as_sf(middle_t)

#make outer shapes
outer_radius = 5

#first create outer circle
circle = st_buffer(st_point(c(0, 0)), dist = outer_radius)


ot1 = list(rbind(c(0, 0), 
                 c(outer_radius, outer_radius), 
                 c(-outer_radius, outer_radius), 
                 c(0, 0))) %>% 
  st_polygon() 

ot1 = st_difference(ot1, t1)
ot1 = st_difference(ot1, mt1)
ot1 = st_intersection(circle, ot1)


ot2 = list(rbind(c(0, 0), 
                 c(-outer_radius, outer_radius), 
                 c(-outer_radius, -outer_radius), 
                 c(0, 0))) %>% 
  st_polygon() 

ot2 = st_difference(ot2, t2)
ot2 = st_difference(ot2, mt2)
ot2 = st_intersection(circle, ot2)

ot3 = list(rbind(c(0, 0), 
                 c(-outer_radius, -outer_radius), 
                 c(outer_radius, -outer_radius), 
                 c(0, 0))) %>% 
  st_polygon() 

ot3 = st_difference(ot3, t3)
ot3 = st_difference(ot3, mt3)
ot3 = st_intersection(circle, ot3)


ot4 = list(rbind(c(0, 0), 
                 c(outer_radius, outer_radius), 
                 c(outer_radius, -outer_radius), 
                 c(0, 0))) %>% 
  st_polygon() 

ot4 = st_difference(ot4, t4)
ot4 = st_difference(ot4, mt4)
ot4 = st_intersection(circle, ot4)

#combine the outer triangles
outer_t = data.frame(id = c("ot1", "ot2", "ot3", "ot4"))
outer_t$outer = rep(1, 4)
outer_t$middle = rep(0, 4)
outer_t$triangle = 1:4
outer_t$geometry = list(ot1, ot2, ot3, ot4)
outer_t = st_as_sf(outer_t)

#combine inner and outer shapes
fig = rbind(outer_t, middle_t, inner_t)

