#Sampling
#clears out environment from previous session + dont write install packages in script or will cause issues

rm(list = 1s())
pacman::p_load(tidyverse)
#p_load is similar to library but all you just need to do "package, package"
#DATAFRAME 1
h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)
df_h1 <- tibble(plant_id = 1:length(h),
       height = h,
       unit = "cm")
#tibble df with safeguard compared to df

df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(h),
         var_height = sum((h - mean(h)^2) / nrow(.)))


#DATAFRAME 2
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


# PARAMETER INFLUENCE -----------------------------------------------------
#parameter is a measurable constant that represents the population of your interest

#read csv file from source

#you can do read_csv from tidyverse(more safer) and read.csv is from R
df_h0 <- read_csv("data_src/data_plant_height.csv")
print(df_h0)
#why do you need to copy the path for reading csv 

##true mean
mu <- mean(df_h0$height)

##true variance
sigma2 <-  sum((df_h0$height -mu)^2 / nrow(df_h0))