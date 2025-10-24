# Import Cambodia_data and Cambodia_df
set.seed(100)

t <- c(rep(0, 476*580), rep(1, 476*580))
c_df <- rbind(df, df)

regress_OLS <- lm(c(df$Y0_v, df$Y1_v) ~ c_df$cover2k_v + c_df$elev_v + 
                    c_df$latitude_v + c_df$longitude_v + c_df$pop_v + 
                    c_df$PREC_v + c_df$TEMP_v + c_df$urban_v + c_df$water_v + 
                    t + c(df$con_v,df$con_v) + t*c(df$con_v,df$con_v))
regress_robust_OLS <- lm_robust(c(df$Y0_v, df$Y1_v) ~ c_df$cover2k_v + c_df$elev_v + 
                                  c_df$latitude_v + c_df$longitude_v + c_df$pop_v + 
                                  c_df$PREC_v + c_df$TEMP_v + c_df$urban_v + c_df$water_v + 
                                  t + c(df$con_v,df$con_v) + t*c(df$con_v,df$con_v))
