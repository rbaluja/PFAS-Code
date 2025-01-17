source("Code/theta_triangles.R")

#set up elevation indicators
df$downstream = ifelse(abs(df$theta - df$theta_gw) <= pi/8, 1, 0)
df$higher_elev = ifelse(df$elevation > df$c_elevation + 50, 1, 0)

summary(fixest::feols(gestation ~ 
                        downstream*higher_elev + log(dist)|year^county + month, 
                      data = df, cluster = "year^month^county"))

        