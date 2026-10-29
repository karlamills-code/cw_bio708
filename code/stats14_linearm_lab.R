rm(list = ls())
pacman::p_load(tidyverse, patchwork)


df_tg <- as_tibble(ToothGrowth)
distinct(df_tg, dose)

###group doses

m_tooth <- lm(len ~ supp * dose,
            data = ToothGrowth)

summary(m_tooth)


eps <- resid(m_tooth)
shapiro.test(eps)

#check residual not the y

#minus the means and then itll turn a bimodal distribution to normal distribution 
#if you see 0.5 or less in the shapiro test the residual is following normal distribution 
#that means the residual is following null hypothesis is rejected


# model prediction --------------------------------------------------------

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)


df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)


ToothGrowth %>% 
  ggplot(aes(x = dose,
             y = len,
             color = supp
             )) +
  geom_point(alpha = 0.2) +
  geom_line(data = df_pred,
            aes(y = y_pred))



# 13.4.3 ------------------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))


plot_x1 <- df_y %>% 
  ggplot(aes(x = x1,
            y = y)) +
  geom_point(alpha = 0.5) 
  

plot_x2 <- df_y %>% 
  ggplot(aes(x = x2,
             y = y)) +
  geom_point(alpha = 0.5) 

plot_x1 +plot_x2


lm(y ~ x1 * x2,
   data = df_y) %>% 
  summary()

##checking predictor relationship
df_y %>% 
  ggplot(aes(x = x1,
             y = x2
             )) +
  geom_point()


#multicommonality causes uncertainty of statistics to be larger
#when there are stronger correlation between predictors

##correlation coefficient
cor(df_y$x1, df_y$x2)


#when you see pair of predictor with a coeff greater than 0.7 avg
#if they correlated you need to say they correlated you need to adress before analysis
#so you need to break the correlation by taking field samples from a different area that goes against the correlation 
