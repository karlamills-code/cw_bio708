rm(list = ls())
pacman::p_load(tidyverse, patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")
print(df_h0)


# subset for 50  ----------------------------------------------------------
mu_50 <- var_50 <- NULL

for (i in 1:1000) {
  df_i_50 <- df_h0 %>% 
    sample_n(size = 50)

mu_50[i] <- mean(df_i_50$height)

var_50[i] <- var(df_i_50$height)
}

#subset for 100 --------------------------------------
mu_100 <- var_100 <- NULL


for (i in 1:1000) {
  df_i_100 <- df_h0 %>% 
    sample_n(size = 100)

mu_100[i] <- mean(df_i_100$height)

var_100[i] <- var(df_i_100$height)
}

df_sample <- tibble( mu_50 = mu_50,
                     var_50 = var_50,
                     mu_100 = mu_100,
                     var_100 = var_100)
#plotting
mean50 <- df_sample %>% 
  ggplot(aes(x = mu_50)) +
  geom_histogram()

var50 <- df_sample %>% 
  ggplot(aes(x = var_50)) +
  geom_histogram() 

mean100 <- df_sample %>% 
  ggplot(aes(x = mu_100)) +
  geom_histogram() 

var100 <- df_sample %>% 
  ggplot(aes(x = var_100)) +
  geom_histogram() 

g_hor <- (mean50 + var50 + mean100 + var100)


# Dr. Teruis quicker way to format everything lapply ver -----------------------------

# list_out <- lapply(x = c(50, 100),
#                    FUN = function(x){
#                      
#                      
#                      mu_i <- var_i <- NULL
#                      for(i in 1:1000) {
#                        df_i<- df_h0 %>% 
#                          sample_n(size = x)
#                        
#                        mu_i[i] <- mean(df_i$height)
#                        var_i[i] <- var(df_i$height)
#                      }
#                      
#                      tibble(mu_hat = mu_i,
#                             var_hat = var_i,
#                             n = x)
#                    })



# less than 10 Exercise 2 ------------------------------------------------------------
#if you fiter out the smaller samples in ecology it doesn't show randomness and it's hard to meet that assumption 
#mean would increase but variance would decrease
df_h10 <- df_h0 %>% 
  filter(height >=10)

mu_50_i <- var_50_i <- NULL

for (i in 1:1000) {
  df_i <- df_h10 %>% 
    sample_n(size = 50)

    mu_50_i[i] <- mean(df_i$height)
    var_50_i[i] <- var(df_i$height) }

#graphing
df_s10 <- tibble(mu_hat = mu_50_i,
                 var_hat = var_50_i)

g_mu_50 <- df_s10 %>% 
  ggplot(aes(x = mu_hat)) + 
  geom_histogram()

g_vu_50 <- df_s10 %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram()

ghor <- g_mu_50 + g_vu_50 

# EXTRA  --------------------------------------------------------------

#facet
df_s10 %>% 
  pivot_longer(
    cols = everything(), #can choose which column 
    names_to = "measure", 
    values_to = "value"
  ) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(facets =~measure)

#each value must come with a label in order to display the specific thing you need 
#changed  the column names to measure and value 


# Correct way to do Lab ---------------------------------------------------

mu_i_50 <- mu_i_100 <- var_i_50 <- var_i_100 <- NULL

for(i in 1:1000) {
  df_50 <- df_h0 %>%
    sample_n(size = 50)
  
  df_100 <- df_h0 %>% 
    sample_n(size = 100)
  
  mu_i_50[i] <- mean(df_50$height)
  mu_i_100[i] <- mean(df_100$height)
  
  var_i_50[i] <- var(df_50$height)
  var_i_100[i] <- var(df_100$height)
  
 }

df_sample1 <- tibble(mu50 = mu_i_50,
                       mu100 = mu_i_100,
                       var50 = var_i_50,
                       var100 = var_i_100)

g50mu <- df_sample1 %>%
  ggplot(aes(x = mu50)) +
  geom_histogram()
  
g100mu <- df_sample1 %>%
  ggplot(aes(x = mu100)) +
  geom_histogram()

g50var <- df_sample1 %>%
  ggplot(aes(x = var50)) +
  geom_histogram()

g100var <- df_sample1 %>%
  ggplot(aes(x = var100)) +
  geom_histogram()

(g50mu / g100mu) | (g50var / g100var)
