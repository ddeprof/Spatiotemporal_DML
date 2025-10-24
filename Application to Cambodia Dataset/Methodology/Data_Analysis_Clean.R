# Import Cambodia data before use
set.seed(100)

library(fields)
library(viridis)
library(tidyverse)

# latitude direction are the N to S (columns), latitude is W to E, rows
latitude <- matrix(1:nrow(elev), nrow = nrow(elev), ncol = ncol(elev))
longitude <- matrix(1:ncol(elev), nrow = nrow(elev), ncol = ncol(elev), byrow = TRUE)

# treatment variable
con_v <- as.vector(con)
# make it binary
con_v <- as.numeric(con_v >= 0.05)

# confounding variables
cover2k_v <- as.vector(cover2k)
elev_v <- as.vector(elev)
latitude_v <- as.vector(latitude)
longitude_v <- as.vector(longitude)
pop_v <- as.vector(pop)
PREC_v <- as.vector(PREC)
TEMP_v <- as.vector(TEMP)
urban_v <- as.vector(urban)
water_v <- as.vector(water)

# keep track of concession regions
# NA <- restricted, 0 <- non con, 1 <- con early, 2 <- con late,
# 3 <- con valid, 4 <- way too early, 5 <- way too late
conTracker <- matrix(NA, 476, 580)
Y0 <- matrix(NA, 476, 580)
Y1 <- matrix(NA, 476, 580)

for (i in 1:476) {
  for (j in 1:580) {
    year <- con_year[i,j] - 2000
    
    # con valid (2005 <= con year <= 2020)
    if (!is.na(year) & year >= 5 & year <= 20) {
      conTracker[i,j] <- 3
      Y0[i,j] <- sum(Y[i,j,(year-4):(year-1)])
      Y1[i,j] <- Y1[i,j] <- sum(Y[i,j,(year):(year+3)])
    }
    
    # con early (2001 <= year <= 2004)
    if (!is.na(year) & year >= 1 & year <= 4) {
      conTracker[i,j] <- 1
      Y0[i,j] <- NA
      Y1[i,j] <- Y1[i,j] <- sum(Y[i,j,(year):(year+3)])
    }
    
    # con late (2021 <= year <= 2024)
    if (!is.na(year) & year >= 21 & year <= 24) {
      conTracker[i,j] <- 2
      Y0[i,j] <- sum(Y[i,j,(year-4):(year-1)])
      Y1[i,j] <- NA
    }
    
    # con way too early (year <= 2000)
    if (!is.na(year) & year <= 0) {
      conTracker[i,j] <- 4
      Y0[i,j] <- NA
      Y1[i,j] <- NA
    }
    
    # con way too late (2025 <= year)
    if (!is.na(year) & year >= 25) {
      conTracker[i,j] <- 5
      Y0[i,j] <- NA
      Y1[i,j] <- NA
    }
    
    # no con (Y0: 2007-2010, Y1: 2011-2014)
    if (is.na(year)) {
      conTracker[i,j] <- 0
      Y0[i,j] <- sum(Y[i,j,7:10])
      Y1[i,j] <- sum(Y[i,j,11:14])
    }    
  }
}

# create vector forms
Y0_v <- as.vector(Y0)
Y1_v <- as.vector(Y1)
conTracker_v <- as.vector(conTracker)

# domain restriction criteria
restrict <- (
  cover2k_v < 0.05 | is.na(con_v) | is.na(cover2k_v) | is.na(elev_v)
  | is.na(pop_v) | is.na(PREC_v) | is.na(TEMP_v) | urban_v > 0.05
  | is.na(water_v)
)

# create data frame
df <- data.frame(Y0_v, Y1_v, con_v, cover2k_v, elev_v, latitude_v,
                 longitude_v, pop_v, PREC_v, TEMP_v, urban_v, water_v,
                 conTracker_v)


# remove data from regions way too early or way too late
df[conTracker_v == 4 | conTracker_v == 5, 1:12] <- NA

# restrict data frame
df[restrict, ] <- NA

# Store Results
save(df, file = "Cambodia_df.RData")

