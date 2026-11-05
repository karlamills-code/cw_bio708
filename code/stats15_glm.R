#Linear Model:
 #t-test - 2, anova - multi group, regression - continuous, ANCOVA 

#GLM uses poisson, binomial, negative binomial, beta, beta binomial 
#poisson uses whole numbers and uses discrete vs. continuous 


pacman::p_load(tidyverse, patchwork)

df_count <- read_csv("data_src/data_garden_count.csv")

##draw figure relating nitrate to plant count
df_count %>% 
  ggplot(aes(x = nitrate, 
             y = count
             )) +
  geom_point()

##fit lm() to data and draw predicted line w/ geom_abline for prediction/fitted line
m_lm <- lm(count ~ nitrate,
   data = df_count)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count
             )) +
  geom_point() +
  geom_abline(
    intercept = coef(m_lm)[1],
    slope = coef(m_lm)[2]
  )

#bc it normal assumption "y" can take any values even below 0, but its count data so it should be positive and above 0
#poisson regression is diff from ordinary regression bc you can make a different assumption when developing a model

##alternativ to ordinary regression 

# poisson regression for count data ---------------------------------------
#glm needs one additional one line with slightly diff function
m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson") #asking what we assuming abt the model 
summary(m_pois)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count
  )) +
  geom_point() +
  geom_abline(
    intercept = coef(m_pois)[1],
    slope = coef(m_pois)[2]
  )

#link function is key in glm and makes difference between lm vs glm
#pois can only take positive numbers so mean values can be 0 or above so the mean value will produce the discreet value 

#link function, log(lambda) = alpha + beta * x for poisson 
x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length = 100)

a <- coef(m_pois)[1] #intercept in a log scale
b <- coef(m_pois)[2] #slope in a log-scale

log_y_pred <- a + b * x
y_pred <- exp(log_y_pred)

df_pred <- tibble(y_pred = y_pred,
                  nitrate = x)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count
  )) +
  geom_point() +
  geom_line(
    data = df_pred,
    aes(y = y_pred))

summary(m_lm)
summary(m_pois)

#the data shows discrete distribution because its positive numbers meaning we need to use poisson instead

##proportional data
#you look at response variable for proportional data like if there are ten flowers per plant the max fruit it can produce is 10 but if we did normal distribution it would go past that number
#binomial distribution may be a better alternative 


# binomial distribution ---------------------------------------------------
df_mussel <- read_csv("data_src/data_mussel.csv")

df_mussel %>% 
  mutate( p = n_fertilized / n_examined) %>% 
  ggplot(aes(x = density,
             y = p)
         ) +
  geom_point()

#because its proportional p cannot exceed 1
#log would make it exponential and exceed limit
#logit would limit range from 0 to 1

##n_examined is possible max success
##n_fertilized is success numbers
##binomial distribution takes two parameters
##Bin(N, p) where N is number of trials, p is success probability

#cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized) #column bind function
#this will be response variable for binomial model 


m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
    data = df_mussel,
    family = "binomial")

summary(m_binom)

#null dev has higher num than resid deviance meaning there is a strong relationship with them

z <- seq(min(df_mussel$density),
         max(df_mussel$density),
         length = 100) 

theta <- coef(m_binom)
logit_p <- theta[1] + theta[2] * z

p_pred <- exp(logit_p) / (1 + exp(logit_p))

df_p <- tibble(p_pred = p_pred,
               density = z)
  
df_mussel %>% 
  mutate(p = n_fertilized/ n_examined) %>% 
  ggplot(aes( x = density, 
              y = p
  )) +
  geom_point() +
  geom_line(
    data = df_p,
    aes(y = p_pred)
  )
