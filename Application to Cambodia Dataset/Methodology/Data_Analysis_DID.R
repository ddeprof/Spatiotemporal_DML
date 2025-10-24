# Import Cambodia_data and Cambodia_df
set.seed(100)

# number of pixels
n <- 476*580

# DBar Creation
D_m <- matrix(df[,6], 476, 580)
loc_D <- matrix(NA, 476, 580)
  
for (i in 1:nrow(D_m)) {
  for (j in 1:ncol(D_m)) {
    # get surroundings (rook)
    # consider all cases
    if (i == 1) {
      if (j == 1) {
        neighbors <- c(D_m[i+1, j], D_m[i, j+1])
      }
      else if (j == 580) {
        neighbors <- c(D_m[i+1, j], D_m[i, j-1])
      }
      else {
        neighbors <- c(D_m[i+1, j], D_m[i, j+1], D_m[i, j-1])
      }
    }
    else if (i == 476) {
      if (j == 1) {
        neighbors <- c(D_m[i-1, j], D_m[i, j+1])
      }
      else if (j == 580) {
        neighbors <- c(D_m[i-1, j], D_m[i, j-1])
      }
      else {
        neighbors <- c(D_m[i-1, j], D_m[i, j+1], D_m[i, j-1])
      }      
    }
    else {
      if (j == 1) {
        neighbors <- c(D_m[i+1, j], D_m[i-1, j], D_m[i, j+1])
      }
      else if (j == 580) {
        neighbors <- c(D_m[i+1, j], D_m[i-1, j], D_m[i, j-1])
      }
      else {
        neighbors <- c(D_m[i+1, j], D_m[i-1, j], D_m[i, j+1], D_m[i, j-1])
      }
    }
    loc_D[i,j] <- mean(na.omit(neighbors))  
  }
}
loc_D_v <- as.vector(loc_D)
  
# regression
t <- c(rep(0, n), rep(1, n))
c_df <- rbind(df, df)

regress_DID <- lm(c(df$Y0_v, df$Y1_v) ~ c_df$cover2k_v + c_df$elev_v + 
                    c_df$latitude_v + c_df$longitude_v + c_df$pop_v + 
                    c_df$PREC_v + c_df$TEMP_v + c_df$urban_v + c_df$water_v + 
                    t + c(df$con_v,df$con_v) + t*c(df$con_v,df$con_v) + c(loc_D_v, loc_D_v) + 
                    t*c(loc_D_v, loc_D_v))
regress_robust_DID <- lm_robust(c(df$Y0_v, df$Y1_v) ~ c_df$cover2k_v + c_df$elev_v + 
                           c_df$latitude_v + c_df$longitude_v + c_df$pop_v + 
                           c_df$PREC_v + c_df$TEMP_v + c_df$urban_v + c_df$water_v + 
                           t + c(df$con_v,df$con_v) + t*c(df$con_v,df$con_v) + c(loc_D_v, loc_D_v) + 
                           t*c(loc_D_v, loc_D_v))


