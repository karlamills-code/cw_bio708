rm(list = ls())
pacman::p_load(tidyverse)


# data structure ----------------------------------------------------------

df_algae <- read_csv("data_src/data_algae.csv")



df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)
         ) +
  geom_point()


m <- lm(biomass ~ conductivity, 
   data = df_algae)

summary(m)

#sigma is called std error, if # is large -> a big part of variation isn't explained

# extract intercept + slope -----------------------------------------------

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)
         ) +
  geom_point() +
  geom_abline(
    intercept = alpha,
    slope = beta
  )


#if std. error is greater than estimate then theres quite a bit of uncertainty in estimate
#p value has greater significance and std. error is strongly related to t/p-value
# t-test is mean difference between two groups divided by some crude(?) std deviation
#null hypothesis is two groups have an equal mean but in regression model it is having no relationship when y is 0
#t test in regression model is beta divided by standard error 


# extract coefficient
theta <- coef(m) #estimate
se <- sqrt(diag(vcov(m)))  #standard error
(t_value <- theta/se)



# unexplained variable ----------------------------------------------------
#residual is deviation from the fitted line

##extract residual
eps0 <- df_algae$biomass - (alpha +beta * df_algae$conductivity)
#residual =     y - prediction model  
eps<- resid(m)
plot(eps0 ~ eps)


#adding new column called eps to make the figure easier
##visualize error
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)
         ) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta
  ) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps),
               linetype = "dashed")

#when you calculate r^2 this is basically how much proportion or reduction happened variation in Y

##original variation in y 
y <- df_algae$biomass
ss0 <- sum((y - mean(y))^2)
ss1 <- sum(eps^2)
#sumsquare 0 = sum((y - muy)^2)
#use residuals to account for conductivity so the relationship wouldnt be horizontal it would be a non-zero line and calc EPS or residual from line and then you can do
#ss1 = sum(eps^2)

#to explain basically theres been a reduction meaning less variation 

rsq <- 1 - ss1 / ss0 
#basically how much variation remains unexplained even after accounting affect of conductivity
#that's why you gotta explain y and then account for x 
#figure out which should be on x and y because it has consequences for statistical analysis 
#vertical deviation is standard to assume but some other methods do other ways
#sometimes people over-interpret r square but basically how much variation reduction after counting variable predictors

summary(m)
