
ssg = fixest::feols(as.formula(paste("gestation", "~",
                                        paste(colnames(df)[(endsWith(colnames(df), "nsites") |
                                                              endsWith(colnames(df), "pfas")) ], collapse = "+"),
                                     "|year + month + county",
                                        sep = "")), data = df_t[df_t$close_pease == 0, ], warn = F)

ssb = fixest::feols(as.formula(paste("bweight", "~",
                                     paste(colnames(df)[(endsWith(colnames(df), "nsites")|
                                                           endsWith(colnames(df), "pfas")) ], collapse = "+"),
                                     "|year + month + county",
                                     sep = "")), data = df_t[df_t$close_pease == 0, ], warn = F)
                                   




nsites_plot = figure_fun(ssg, "nsites", meters, 
                         n_triangles, c(2, 4, 6, 8), color = "YlOrRd", 
                         title = "Gestation on Num Sites")

pfas_plot = figure_fun(ssg, "pfas", meters, 
                       n_triangles, c(2, 4, 6, 8), color = "YlOrRd", 
                       title = "Gestation on PFAS Level") 
ggarrange(nsites_plot, pfas_plot)


                             
                          
modelsummary::modelsummary(list(ssg, ssb), 
                            stars = TRUE, 
                           fmt = modelsummary::fmt_significant(2), 
                           coef_map = str_sort(names(ssg$coefficients)) 
                           )





















# 
# 
# summary(fixest::feols(gestation ~ t4_n_sites  + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ, data = df))
# summary(fixest::feols(gestation ~ ifelse(t1_n_sites > 0, 1, 0)  + m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(preterm ~ ifelse(t4_n_sites > 0, 1, 0) +
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(gestation ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(bweight ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(preterm ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(lbweight ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(lbweight ~ t1_n_sites  + m_age + m_married + white + private_insurance + cig + n_prenatal + gestation|year + month, data = df_nazero))
# 
# 
df_c = df %>%
  dplyr::filter(!is.na(t1_n_sites))
# 
summary(fixest::feols(gestation ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  +
                        ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
                        + m_age + m_married + white + private_insurance + cig + n_prenatal +
                        pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ, data = df_c))
# 
# summary(fixest::feols(bweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ, data = df_c))
# 
# summary(fixest::feols(preterm ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ, data = df_c))
# 
# summary(fixest::feols(lbweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0) +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ, data = df_c))
# 
# 
# #adding county fe
# summary(fixest::feols(gestation ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  + 
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(bweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(preterm ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(lbweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0) +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
# #adding cont amounts
# summary(fixest::feols(gestation ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas+
#                         m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(bweight ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(preterm ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas+
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(lbweight ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas+
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
