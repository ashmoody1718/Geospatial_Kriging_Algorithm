# Abigail Moody
# Nov. 5th 2024

# Homework 6: Ordinary Kriging

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(matlib)
source("variogram_mf.r")

# Setting up the data
N5 = read_excel("N5Samples.xlsx")
N5 = N5[1:5,-1]
colnames(N5) <- c("x", "y", "Na")

# D matrix function
D_matrix = function(n, x_val, y_val, og_df, fit){
  n = nrow(og_df)
  D = c()
  # For loop grabbing fitted values based on distances
  for(i in 1:n){
    new_dist = edist(og_df[i,1], og_df[i,2], x_val, y_val)
    rounded_dist = as.numeric(round(new_dist, 2))
    semi_index = which(near(fit[,1],rounded_dist))
    semi_num = fit[semi_index,2]
    D = append(D, semi_num)
  }
  D = matrix(append(D, 1))
  return(D)
}

# C matrix function
C_matrix = function(n, og_df, fit){
  n = nrow(og_df)
  C = matrix(1, nrow = n+1, ncol = n+1)
  C[n+1, n+1] = 0
  # For loop grabbing fitted values based on distances
  for(i in 1:n){
    for(j in 1:n){
      new_dist = edist(og_df[i,1], og_df[i,2], og_df[j,1], og_df[j,2])
      rounded_dist = as.numeric(round(new_dist, 2))
      semi_index = which(near(fit[,1],rounded_dist))
      semi_num = fit[semi_index,2]
      if(length(semi_num) == 0){
        semi_num = 0
      }
      C[i,j] = semi_num
    }
  }
  return(C)
}

# Function to output the estimates
estimates = function(og_df, primary, sill, points){
  n = nrow(og_df)
  fitted_nums = variogram_mf(og_df,"Equal", primary, sill)
  C = C_matrix(n, og_df, fitted_nums)
  C_inv = inv(C)
  est = matrix(NA, nrow(points), 4)
  for(i in 1:nrow(points)){
    p1 = points[i,1]
    p2 = points[i,2]
    # Making the d matrix
    D = D_matrix(n, p1, p2, og_df, fitted_nums)
    # Creating the w matrix
    w = C_inv%*%D
    col_index = which(colnames(og_df) == primary)
    # Calculating estimates and errors
    V_Xo = sum(w[1:n,]*(og_df[,col_index]))
    sigma_r = sum(w*D)
    est[i,] = c(p1, p2, V_Xo, sigma_r)
  }
  df = data.frame(est)
  colnames(df) <- c("x", "y", "V_Xo", "sigma_r")
  return(df)
}


# Plotting Original Points
ggplot(N5, aes(x = x, y = y)) + 
  geom_point(alpha = 0.8, size = 4, aes(color = Na)) +
  labs(title="Location and Na",
       x ="X Location", y = "Y Location", color = "Na (mg/L)") +
  scale_color_gradient(low = "blue", high = "orange")+
  theme_light()


# Creating a grid of points
x_points = seq(4,17,1)
y_points = seq(8,15,1)

# Finding all of the pairs to create a full grid of points
p_v = expand.grid(x_points, y_points)

# Running the estimates function
est = estimates(N5, "Na", 15000, p_v)

# Plotting the estimates and errors
ggplot(est, aes(x = x, y = y)) + 
  geom_point(alpha = 0.8, size = 4, aes(color = V_Xo)) +
  labs(title="Location and V(Xo) Estimate",
       x ="X Location", y = "Y Location", color = "V(Xo) Estimate") +
  scale_color_gradient(low = "blue", high = "orange")+
  theme_light()

ggplot(est, aes(x = x, y = y)) + 
  geom_point(alpha = 0.8, size = 4, aes(color = sigma_r)) +
  labs(title="Location and Error Variance",
       x ="X Location", y = "Y Location", color = "Error Variance") +
  scale_color_gradient(low = "blue", high = "orange")+
  theme_light()
