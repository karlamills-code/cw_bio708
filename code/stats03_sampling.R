
#clears out environment from previous session + dont write install packages in script or will cause issues
#rm(list = 1s())

# Sampling ----------------------------------------------------------------


pacman::p_load(tidyverse, patchwork)
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

##random sampling
df_i <- df_h0 %>% 
  sample_n(size = 10)

#for loop automates the function 
#isnt function and for loop the same thing?

##for loop

mu_i <- var_i <- NULL

for (i in 1:1000) {
  ##randomly sample 10 individuals but runs only one time
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  ##mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  ##variance for subset
  var_i[i] <- ((df_i$height - mu_i[i])^2 /nrow(df_i))  
  #if you don't put [i] then it wont say which element this individual element for the mean will be
  #for example mu_i1 it will assign, mu_i2, etc 
}

#mu_i output is showing the mean for 10 individual sample types

#visualization
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

g_mu <- df_sample %>% 
  ggplot(aes(x = mu_i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)


g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

#show how histogram of estimated means are distributed

# PATCHWORK ---------------------------------------------------------------
#combines multiple figures into one

g_hor <- g_mu + g_var
g_ver <- g_mu / g_var

#there will always be deviation or bias from the truth from sampling.we can make an inference to better know the entire population for stats
#there should not be biased but we see that variance has bias
#when collecting variance we need to do a "- 1" to correct bias


# BIAS CORRECT VERSION ----------------------------------------------------
#EXERCISE - show bias correct version from variance 

##for loop

var_ub_i <- NULL

for (i in 1:1000) {
  ##randomly sample 10 individuals but runs only one time
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
    ##variance for subset
    #var()'s denominator is N-1, not N
  var_ub_i[i] <- var(df_i$height)  
}

df_sample <- df_sample %>% 
  mutate(var_ub_hat = var_ub_i)

g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

#patchwork
gvar_ub_hor <- g_var_ub + g_var
gvar_ub_ver <- g_var_ub / g_var

#Dr.Terui's ver of patchwork
g_var <- g_var + scale_x_conntinuous(limits = c(0:80)
