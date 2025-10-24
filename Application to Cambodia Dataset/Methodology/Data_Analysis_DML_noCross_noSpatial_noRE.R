# Import Cambodia_data and Cambodia_df
set.seed(100)

library(estimatr)
library(dbarts)
library(BART)

# number of pixels
n <- 476*580

# BART f0
x <- df[,c(4:5, 8:12)]
y <- df[,1]
bart_fitY0 <- bart(x.train = x, y.train = y)

# BART f1
y <- df[,2]
bart_fitY1 <- bart(x.train = x, y.train = y)

# BART g
y <- df[,3]
bart_fit_g <- bart(x.train = x, y.train = y, keeptrees = TRUE)
predicted_g <- predict(bart_fit_g, x)

restrict_Y0 <- (
  df$cover2k_v < 0.05 | is.na(df$con_v) | is.na(df$cover2k_v) | is.na(df$elev_v)
  | is.na(df$pop_v) | is.na(df$PREC_v) | is.na(df$TEMP_v) | df$urban_v > 0.05
  | is.na(df$water_v) | is.na(df$Y0_v) 
)
restrict_Y1 <- (
  df$cover2k_v < 0.05 | is.na(df$con_v) | is.na(df$cover2k_v) | is.na(df$elev_v)
  | is.na(df$pop_v) | is.na(df$PREC_v) | is.na(df$TEMP_v) | df$urban_v > 0.05
  | is.na(df$water_v) | is.na(df$Y1_v) 
)
restrict_D <- (
  df$cover2k_v < 0.05 | is.na(df$con_v) | is.na(df$cover2k_v) | is.na(df$elev_v)
  | is.na(df$pop_v) | is.na(df$PREC_v) | is.na(df$TEMP_v) | df$urban_v > 0.05
  | is.na(df$water_v) | (is.na(df$Y0_v) & is.na(df$Y1_v))
)

# handle non NA values
R_Y0_valid <- df$Y0_v[!restrict_Y0] - bart_fitY0$yhat.train.mean
R_Y1_valid <- df$Y1_v[!restrict_Y1] - bart_fitY1$yhat.train.mean
R_D_valid <- df$con_v[!restrict_D] - colMeans(predicted_g)

# put back into vectors of length n (number of pixels)
R_Y0 <- rep(NA, length(df$Y0_v))
R_Y0[!restrict_Y0] <- R_Y0_valid
R_Y1 <- rep(NA, length(df$Y1_v))
R_Y1[!restrict_Y1] <- R_Y1_valid
R_D <- rep(NA, length(df$con_v))
R_D[!restrict_D] <- R_D_valid

R_matrix <- cbind(R_Y0, R_Y1)

# vectorize R
R <- as.vector(R_matrix)
  
# calculate local mean
R_D_m <- matrix(R_D, 476, 580)
localR_D_m <- matrix(NA, 476, 580)
for (i in 1:476) {
  for (j in 1:580) {
    # get surroundings (rook)
    # consider all cases
    if (i == 1) {
      if (j == 1) {
        neighbors <- c(R_D_m[i+1, j], R_D_m[i, j+1])
      }
      else if (j == 580) {
        neighbors <- c(R_D_m[i+1, j], R_D_m[i, j-1])
      }
      else {
        neighbors <- c(R_D_m[i+1, j], R_D_m[i, j+1], R_D_m[i, j-1])
      }
    }
    else if (i == 476) {
      if (j == 1) {
        neighbors <- c(R_D_m[i-1, j], R_D_m[i, j+1])
      }
      else if (j == 580) {
        neighbors <- c(R_D_m[i-1, j], R_D_m[i, j-1])
      }
      else {
        neighbors <- c(R_D_m[i-1, j], R_D_m[i, j+1], R_D_m[i, j-1])
      }      
    }
    else {
      if (j == 1) {
        neighbors <- c(R_D_m[i+1, j], R_D_m[i-1, j], R_D_m[i, j+1])
      }
      else if (j == 580) {
        neighbors <- c(R_D_m[i+1, j], R_D_m[i-1, j], R_D_m[i, j-1])
      }
      else {
        neighbors <- c(R_D_m[i+1, j], R_D_m[i-1, j], 
                       R_D_m[i, j+1], R_D_m[i, j-1])
      }
    }
    localR_D_m[i,j] <- mean(na.omit(neighbors))   
  }
}
localR_D <- as.vector(localR_D_m)

# regression
t <- c(rep(0, n), rep(1, n))
regress_DML <- lm(R ~ t + c(R_D, R_D) + c(localR_D, localR_D) + 
                    t*c(R_D, R_D) + t*c(localR_D, localR_D))
regress_robust_DML <- lm_robust(R ~ t + c(R_D, R_D) + c(localR_D, localR_D) + 
                                  t*c(R_D, R_D) + t*c(localR_D, localR_D))

