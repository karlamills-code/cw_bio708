# more than two groups for t-test (ANOVA) and compares the avg but uses var in data

pacman::p_load(tidyverse)
rm(list = ls())

df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova, lake)

df_anova %>% 
  ggplot(aes(x = lake,
             y = length)
         ) +
  geom_violin(
    draw_quantiles = 0.5, 
    alpha = 0.2
    ) + 
  geom_jitter(
    height = 0,
    width = 0.1
  )

#violin plot is to show frequencies of this point
#partition var within/between variance 


# ANOVA -------------------------------------------------------------------
#put variable on left is what you want to explain like measurement errors ~ var on right is predictor and this is variable you use to explain the variation and why, distinction is important caause you make the assumption of causality
aov(length ~ lake, 
    data = df_anova)


#overall average and between average needed to do the Between 

#BETWEEN-GROUP VARIANCE------
#overall mean 
mu <- mean(df_anova$length)

#group specific means #this is the body for the eq (μg(i)-μ)^2
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length),
            dev_g = (mu_g - mu)^2,
            n = n())

#this is the summation across individuals for the first half of the equation ∑i
ss_b <- df_g %>% 
  mutate(ss_g = dev_g * n) %>% 
  pull(ss_g) %>% 
  sum() #this is the summation across groups for the first hald of equation ∑g


#WITHIN-GROUP VARIANCE------
ss_w <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% 
  ungroup() %>% #good to use ungroup esp if use mutate or you'll get wrong numbers
  mutate(dev_i = (length - mu_g)^2) %>% 
  pull(dev_i) %>% 
  sum()
#we interested in mean between each individual so this is more ideal

##OVERALL VARIABILITY
ss_o <- sum((df_anova$length - mu)^2)
         #or you can do 
ss_b +ss_w


#if residual variability is reduced after counting for group structure that means it has some meaning


#take simple transformation of sum of squares and take ratio between two to get the mean aq error
#F value is the ratio difference between the mean sq 
summary(aov(length ~ lake, df_anova))

#degree of freedom is information available so when calc group mean/deviation from it you will use overall mean to calculate group specific means


##convert variability to variance 
sig_b <- ss_b / 2
sig_w<- ss_w / (nrow(df_anova) - n_distinct(df_anova$lake)) #denominator is 147 

## test-statistic
f_value <- sig_b / sig_w

# you use f-statistic in anova because the groups you want to compare aganist null hypothesis 
#exploit info from variance component to compare means

#calculate p value you want to understand distribution under null hypothesis
#there is no diff among averages on all groups
#anova cant tell which has significant difference
#so you need to pair with post-hoc

#1 distinct feat f stat is ratio variance (var is positive cant be negative), var on numerator is pos too 


##null distribution
f <- seq(0,10, by = 0.01)
pd <- df(f, df1 = 2, df2 = 147)
#the null distribution is defined by degree of freedom which is related to sample size
#as total no. of sample size increase there is more possibility to get more significant p-value
#having more groups you will need more samples to get p-value 

tibble(x = f, y = pd) %>% 
  ggplot(
    aes(x = x,
        y = y)
  ) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "chocolate")+
  labs(x = "F Statistics",
       y = "Probability Density")

#this is null distribution if there is no significance between groups
#if no diff between groups the ratio converges to 0 

p_value <- 1 - pf(f_value, df1 = 2, df2 = 147) 

#this is cumulative probability up to observed value but we want beyond value of observed
#so you need to do 1 - pf = p-value

summary(aov(length ~ lake, df_anova))

#look at the p-value we got and the summary

#if u have more than 2 groups you have multi combos which means you need to perform multiple tests
#this is problematic in stats cause it'll say you have increase chance of false positives
#so post-hoc tests are designed to account for the multiple groups 