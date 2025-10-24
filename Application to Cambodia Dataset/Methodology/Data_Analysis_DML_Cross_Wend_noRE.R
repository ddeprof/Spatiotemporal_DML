# Import Cambodia_data and Cambodia_df
set.seed(100)

library(fields)
library(dbarts)
library(caret)
library(estimatr)

# This is a Wendland Kernel 
basis_wend <- function(s,u,theta){
  d1 <- rdist(s,u)/theta
  # Wendland basis function from Nychka  
  mat1 <- apply(d1,c(1,2),function(x){
    if(x <= 1){
      return( (1-x)^6 * (36*x^2 +18*x + 3)/3)
    }else{
      return(0)
    }
  })
  return(mat1)
}

# number of pixels
n <- 476*580
# s is spatial locations
s <- expand.grid((1:476)/(476+1),(1:580)/(580+1))
l <- 10
L <- l^2
# knot points
u <- expand.grid(seq(0,1,length=l),seq(0,1,length=l))
#bandwidth parameter 2.5
z <- basis_wend(s,u,2.5/l)
colnames(z) <- paste0("z", seq_len(ncol(z)))
df_w <- cbind(df, z)

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

# f0
x <- df_w[!restrict_Y0,c(4:12, 14:ncol(df_w))]
y <- df_w[!restrict_Y0,1]
# split data into 5 folds
folds <- createFolds(y, k = 5)
# initialize placeholder for predictions
predict_Y0 <- rep(NA, sum(!restrict_Y0))
# store variable importance
var_importance_Y0 <- rep(NA, k)
# crossfitting Y0
for (fold in seq_along(folds)) {
  # this is the training data set
  train_idx <- setdiff(seq_len(sum(!restrict_Y0)), folds[[fold]])
  # ith fold
  test_idx <- folds[[fold]]
  # trains a bart model to predict Y0
  bart_Y0 <- bart(x.train = x[train_idx, ], y.train = y[train_idx], keeptrees = TRUE)
  # storing the predicted bart data into the placeholder vectors
  bart_preds <- predict(bart_Y0, x[test_idx, ])
  predict_Y0[test_idx] <- colMeans(bart_preds)
  # store variable importance
  var_importance_Y0[test_idx] <- bart_Y0$varcounts
}

# f1
x <- df_w[!restrict_Y1,c(4:12, 14:ncol(df_w))]
y <- df_w[!restrict_Y1,2]
# split data into 5 folds
folds <- createFolds(y, k = 5)
# initialize placeholder for predictions
predict_Y1 <- rep(NA, sum(!restrict_Y1))
# store variable importance
var_importance_Y1 <- rep(NA, k)
# crossfitting Y1
for (fold in seq_along(folds)) {
  # this is the training data set
  train_idx <- setdiff(seq_len(sum(!restrict_Y1)), folds[[fold]])
  # ith fold
  test_idx <- folds[[fold]]
  # trains a bart model to predict Y1
  bart_Y1 <- bart(x.train = x[train_idx, ], y.train = y[train_idx], keeptrees = TRUE)
  # storing the predicted bart data into the placeholder vectors
  bart_preds <- predict(bart_Y1, x[test_idx, ])
  predict_Y1[test_idx] <- colMeans(bart_preds)
  # store variable importance
  var_importance_Y1[test_idx] <- bart_Y1$varcounts
}

# g
x <- df_w[!restrict_D,c(4:12, 14:ncol(df_w))]
y <- df_w[!restrict_D,3]
# split data into 5 folds
folds <- createFolds(y, k = 5)
# initialize placeholder for predictions
predict_D <- rep(NA, sum(!restrict_D))
# store variable importance
var_importance_D <- rep(NA, k)
# crossfitting D
for (fold in seq_along(folds)) {
  # this is the training data set
  train_idx <- setdiff(seq_len(sum(!restrict_D)), folds[[fold]])
  # ith fold
  test_idx <- folds[[fold]]
  # trains a bart model to predict D
  bart_D <- bart(x.train = x[train_idx, ], y.train = y[train_idx], keeptrees = TRUE)
  # storing the predicted bart data into the placeholder vectors
  bart_preds <- predict(bart_D, x[test_idx, ])
  predict_D[test_idx] <- colMeans(bart_preds)
  # store variable importance
  var_importance_D[test_idx] <- bart_D$varcounts
}

# handle non NA values
R_Y0_valid <- df$Y0_v[!restrict_Y0] - predict_Y0
R_Y1_valid <- df$Y1_v[!restrict_Y1] - predict_Y1
R_D_valid <- df$con_v[!restrict_D] - predict_D

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

save(regress_robust_DML, predict_Y0, predict_Y1, predict_D, var_importance_Y0, var_importance_Y1, var_importance_D, df, file = "data_analysis.RData")

