#two-group test comparison

pacman::p_load(tidyverse)
rm(list = ls())

df_fl <- read_csv("data_src/data_fish_length.csv")

#base-R
unique(df_fl$lake)

#dplyr
distinct(df_fl, lake)

#get mean and sd body size
df_fl_mu <- df_fl %>% 
  group_by(lake) %>%
  summarize(mu_l = mean(length),
            sd_l = sd(length)
  )

#figure
df_fl %>% 
  ggplot(
    aes(x= lake, 
        y = length)
  ) +
  geom_jitter(
    width = 0.1,
    height = 0,
    alpha = 0.25
  ) +
  geom_segment(data = df_fl_mu,
               aes( x = lake,
                    y = mu_l - sd_l,
                    yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu,
             aes(x = lake,
                 y = mu_l),
               size = 3) +
  labs(x = "lake",
       y = "Fish body length")


#t-test
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t.test(x, y, var.equal = TRUE)

#t value =  difference in avg values + how reliable difference is and is created by dividing difference in mean value by the variation 

#df = degrees in freedom is parameter related to how many samples you collected and how the p_value becomes reliable 
#p value = two groups are identical this is the probability of observing the observation between two groups(OBSERVED DIFFERENCE) if value is super small the difference is unlikely to be different therefore it means there must be from different populations instead


#get t-value-----
v_mu <- df_fl_mu %>% 
  pull(mu_l)

v_mu[1] - v_mu[2] #this is difference between mean values

#create another dataframe to get t-value 
df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            var_l = var(length),
            n = n()
            )

#get the weighted avg btwn two groups

#mean vector
v_mu <- pull(df_t, mu_l)

#variance vector
v_var <- pull (df_t, var_l)

#sample size vector
v_n <- pull(df_t, n)


var_p <- ((v_n[1] - 1) / (sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1) / (sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p* ((1/ v_n[1]) + (1/ v_n[2])))
          #diff in mean value     #the variance 

#t value divides difference in mean value by the variation 
#when the difference of the two groups is bigger the t-value is bigger 
#if the variation increases the t-value is smaller
#tvalue increases either magnitude in difference is larger and/or reliability of difference is larger because of small variation 
#difference in mean value is not able to show how certain the difference is but t-value fills that need 
#t value is called tvalue bc diff btwn 2 groups and the diff value tends to follow t distribution  esp when there is no differnce between two groups


#null hypothesis is the baseline conditions for the two groups
#p value is calculated based on null hypothesis

#get p-value
x <- seq(-5, 5, length = 500)

#probability density of t-stats with df = 98
y <- dt(x, df = sum(v_n) - 2)
y1 <- dt(x, df = 10 - 2)
            
tibble(x,y, y1) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(aes(y  = y1),
            color = "red") +
  geom_vline(xintercept = abs(t_value), color = "salmon")+
  geom_vline(xintercept = t_value, color = "salmon")
  labs(y = "Probability Density",
       x = "t=statistic")

#this distribution drawn under assumption where there is no difference

#if you increase sample size the distribution is closer to the truth 
  
#t-test under unequal variance
x <- df_fl %>% 
  filter(lake == "a") %>%
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = FALSE)
#want the two groups to be equal but they are often not so t-test puts assumption theres a difference in variance so you need to make it false
#var = truth good when two groups are equal
#var = truth has terrible performance if two groups not equal
#var.equal = false is better because it assumes the two groups are not equal providing more correct values
#t-value used in relation and other relationships as well to help interpret results
#also to observe greater value + the ratio of mean diff and uncertainty(variance)