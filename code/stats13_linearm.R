rm(list = ls())
pacman::p_load(tidyverse)


# categorical -------------------------------------------------------------
df_fl <- read_csv("data_src/data_fish_length.csv")

###lm approach
m_fl <- lm(length ~ lake,
   data = df_fl)

summary(m_fl)

##calculate mean fish length for a & b to verify the summary above
#take difference

v_mu <- df_fl %>% 
  group_by(lake) %>%
  summarize(mu = mean(length)) %>% 
pull(mu)

v_mu[2] - v_mu[1]

### t-test for comparison of bw a & b
t.test(length ~ lake, data = df_fl, var.equal = TRUE)
#does true or false matter a lot here????????????????????????????

###anova vs lm
df_anova <- read_csv("data_src/data_fish_length_anova.csv")

###lm 
m_anova <- lm(length ~ lake,
               data =df_anova)
summary(m_anova)

###calculate mean for each group
###differences between a & b, a & c
mu_anova <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

#a & b 
mu_anova[2] - mu_anova[1]
#a & c
mu_anova[3] - mu_anova[1]

###anova or aov() function
fit_aov <- aov(length ~ lake,
    data = df_anova)

summary(fit_aov)

###modfiy order of groups to compare different baseline in lm
df_anova <- df_anova %>% 
  mutate(lake_factor = factor(lake, levels = c("c", "a", "b")))

unique(df_anova$lake)
unique(df_anova$lake_factor)

###lm with lake factor
lm(length ~ lake_factor,
   data = df_anova) %>% 
  summary
# mix of continuous and categorical in predictors -------------------------
iris <- as_tibble(iris)
distinct(iris, Species)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,  #add more cateogries into it
   data = iris) 

  summary(m_iris)

# visualization  ----------------------------------------------------------
n_rep <- 100
df_pred <- iris %>% 
  group_by(Species) %>% 
  reframe(   #want to create full range of petal width and create continuous range
    Petal.Width = seq(min(Petal.Width), #make seq of min and max from species and have max of 100 numbers 
                      max(Petal.Width),
                      length = n_rep)
  ) 

y_pred <- predict(m_iris,  #replacing data of obs to data of predicted values based on values of petal width 
                  newdata = df_pred)
  
df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

###figure
iris %>% 
  ggplot(aes(
    x = Petal.Width,
    y = Petal.Length,
    color = Species
  )) +
  geom_point(alpha = 0.5) +
  geom_line(
    data = df_pred,    #need to change dataframe cause the prediced values is in another df
    aes(y = y_pred)
  )
  



# Interactions ------------------------------------------------------------

#         main effect                      Interaction
#lm(Petal.Length ~ Petal.Width + Species + Petal.Width:Species, #the petal.width:species is the interaction part
#   data = iris) 

###shortcut but the exact same formula as above
m_iris_int <- lm(Petal.Length ~ Petal.Width * Species, #* is a shortcut for this model formula
   data = iris) 

summary(m_iris_int)

y_pred_int <- predict(m_iris_int,
                      newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred_int = y_pred_int)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred_int))

# notes from class --------------------------------------------------------
#using normal distribution when you testing if two groups are significant 
#only diff between three methods is what kind of predictor you are using and they can 
#analysis covariance


#####located in section categorical---
#interpret output w lake b being contrast of lake a 
#lake b is on avg 2.056 avg higher in mean fish length 

#when you give categorical in lm you will convert it to binary predictor
#mu a = alpha + beta * 0 but beta and 0 cancels out so you get alpha as the base
#in mu b = alpha + beta * 1

#fstat from lm output is corresponding to output from anova
#anova can tell you any diff has combo but lm takes a default of donut test with one group as a baseline vs another group and handles the issue of every single pair in anova
#lm sets baseline but sometimes you want to set a different baseline because you comparing groups
#you can modify the order of groups

######located in mix of categorical and continuous---
#general linear model refers to set of model that assumes normal dist with error structure
#generalized linear model is a whole different thing
#mu = alpha + beta Petal Width + beta Species versicolor + beta species virginica 
  #alpha is setosa as the baseline
  #alpha refers to the intercept between setosa and petal width 
#basically categorical values can affect how differnet groups change the intercept so you end up 
#drawing a parellel line above the baseline

######located in visualization---
#the affect of factor 1 is evaluated after accounting for another factor. it is a conditional effect 
#factor 1 is considered significant in relationship to a factor but it may not be for another factor
#interaction between petal width and length is species specific based on the results in the visualization


######located in Interactions---
#always included main effects and the interaction. Main effect already included in shortcut
#slopes look different compared to the visualization

#species versicolor is significant but not virginica in this case but virginica slope is different comapred to the other species
#so the numerical interaction term is difference in slope value from baseline 
#baseline for petal.width is 1.3276 but the slope with petal.width is 0.5465
#interaction terms are hard to grasp until you can understand 

#when you see interactions in paper youll see one of the groups change in slope