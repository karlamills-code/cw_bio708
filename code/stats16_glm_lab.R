# GLM Exercise ------------------------------------------------------------
pacman::p_load(tidyverse, patchwork)


url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

fish_pois <- glm(n_sp ~ distance + cat_area + hull_area,
                data = df_fish,
                family = "poisson")
summary(fish_pois)


#cars
df_mtcars <- as_tibble(mtcars)

cars_binom <- glm(am ~ mpg + hp + wt,
                 data = df_mtcars,
                 family = "binomial")
summary(cars_binom)

cars_gaus <- glm(am ~ mpg + hp + wt,   #gaussian is not good cause its not a normal dist
                 data = df_mtcars,
                 family = "gaussian")   #normal distribution
summary(cars_gaus)

#if the est is negative then it means it is significant
# Effect Size -------------------------------------------------------------

#we standardizing predictors to make it comparable 
#subtracting by mean is called centering and changes estimate of intersect
#dividng by sd will affect the spread
#slope effect by how much spread in predictor value
#can compare effect of each variable at same scale bc we cancelled out the unit measure
#interept has meaning on unscaled version and when x = 0 
#after standardization the meaning changes bc you center it meaning subtracting mean of x and shifting plot in a dirrection
#result means expected value of y when x is given mean value of x
#intercept is expected value of y when x =0 is changed to x = mean

df_fish <- df_fish %>% 
  mutate(
    std_dist = c(scale(distance)),
    std_cat = c(scale(cat_area)),
    std_hull = c(scale(hull_area))
  )  #have negative values bc you subtracted the mean

es_fish <- glm(n_sp ~ std_dist + std_cat + std_hull,
    data = df_fish,
    family = "poisson")
summary(es_fish)


# Offset term -------------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

df_offset <- df_offset %>% 
  mutate(density = count / area)

g_area <- df_offset %>% 
  ggplot(aes(x = area,
             y = count)
         ) + 
  geom_point()

g_nit <- df_offset %>% 
  ggplot(aes(x = nitrate, 
             y = count)
         ) +
  geom_point()

g_den <- df_offset %>% 
  ggplot(aes(x = nitrate, 
             y = density)
         ) +
  geom_point()

g_area | g_nit | g_den

#offset 
m_pois_off <- glm(count ~ nitrate + offset(log(area)),
    data = df_offset,
    family = "poisson")

m_normal <- glm(density ~ nitrate,
    data = df_offset,
    family = "gaussian")

# Overdispersion ----------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)

df_tadpole %>% 
  ggplot(aes(x = aqveg,
             y = tadpole)
         ) +
  geom_point()

df_tadpole %>% 
  ggplot(aes(x = permanence,
             y = tadpole)
         ) +
  geom_point()



glm_tad <- glm(tadpole ~ aqveg + permanence,
               data = df_tadpole,
               family = "poisson")
summary(glm_tad)


##negative binomial much better bc var is greater than mean
mean(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
var(df_tadpole$tadpole)

MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole) %>% 
  summary()

#var usually = to mean but if var is greater than mean then overdisp then conseqquence is var has no relationship with y = very significant so be careful





