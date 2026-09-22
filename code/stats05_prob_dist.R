#Probability distribution
#if the variable of interest is continuous - density if then discrete - math function \
rm(list = ls())
pacman::p_load(tidyverse, patchwork)

## Continuous variable -----------------------------------------------------
df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0 %>%  
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1, center = 0.5) +
  geom_vline(xintercept = mean(df_h0$height))

#bell shape formed by approximate normal distribution
#need unknown constant and standard variance to get bell shape histogram
#you need mean and variance for ?????????
#x ~ Normal(mean mew, sd^2)

#draw probability distribution
x <- seq(min(df_h0$height),
         max(df_h0$height),   
         length = 100)

mu <- mean(df_h0$height) #mean 
sigma <- sd(df_h0$height) #standard deviation
pd <- dnorm(x, mean = mu, sd = sigma) #density distribution function and what value youd like to evaluate and second is mean of distribution and the third argument is the SD 

tibble(y = pd, x = x) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  labs(y = "Probability density")

#HISTOGRAM NOT GRAPHED RIGHT - REVIEW


#convert probability density to frequency
p10 <- pnorm(q = 10, mean = mu, sd = sigma) #this calculating the left quadrant of the histogram
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
p20 - p10 #asking area under curve under the 10th to 20th percentile

x_min <- floor(min(df_h0$height))
x_max <- floor(max(df_h0$height)) #floor takes the integer of a decimal and ceiling rounds up
bin <- seq(x_min, x_max, by = 1) #if you want fixed interval or no. of elements and in this example uses a fixed interval

#this is probability
p<- NULL
for(i in 1:(length(bin) - 1)) {
  #probability up to bin [i+1]
  p_up <- pnorm(bin[i+1], mean = mu, sd = sigma)
  #probability up to bin[i] 
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)
  
  #difference p_up and p_low represents probability between bin[i] and bin[i+1]
  p[i] <- p_up - p_low
}

##P DONT LOOK RIGHT - REVIEW

#this is frequency and you multiply it by the amt of rows in df
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

#combine data and PDF
df_h0 %>% 
  ggplot(aes(x = height)) +
  geom_histogram(
    binwidth = 1,
    center = 0.5
  ) + 
  geom_point(
    data = df_prob,
    aes(x = bin,
        y = freq)
  ) + 
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin), 
            color = 'salmon')


## Discrete variable -------------------------------------------------------

#introducing probabilistic distributions because you need to choose a distribution depending on your data
#the number of individuals in a given area - the variable isn't continuous so normal distribution wouldnt be the right choice
#then the statistic analysis failed to find the statistical distribution
##POISSON has two variables function ???????????????????????????????
#when representing some variable then you have to pick right distribution analysis

df_count <- read_csv("data_src/data_garden_count.csv")
(df_count)

#histogram 
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, #define binwidth
                 center = 0) #the relative position for each bin

#Poisson fit
x <- seq(0, 10, by = 1)

lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat) #dpois returns probability because its discrete representatinon variable

tibble(x = x,
       y = pm) %>% 
  ggplot(
    aes(x =x,
        y=y)
  ) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(y = "Probability",
       x = "Count")

df_prob <- tibble(x=x,
                  y=pm) %>% 
  mutate(freq = y * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(
    binwidth = 0.5,
    center = 0
  ) +
  geom_line(
    data = df_prob, 
    aes(x = x,
        y = freq),
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_point(
    data = df_prob,
    aes(x = x,
        y = freq),
    color = "steelblue"
  )

#trying to find if there is a significant difference between two groups
#youll see difference between two groups and how variable the group is
#when you compare and statistical p value
#compare distribution of the two groups and normal probable distribution and the approximate shape of bell shape curve
#when you choose differen probable distribution you'll get different results
