rm(list = ls())
pacman::p_load(tidyverse)


df_iris <- tibble(iris)

df_iris %>% 
  group_by(Species) %>% 
  summarize(n_sample = n())
  
#12.4.1--------------------
#individual version
v_sp <- unique(iris$Species)

fit1  <- iris %>%                               #youd have to define dataframe here
  filter(Species == v_sp[1]) %>% 
  lm(Sepal.Width ~ Petal.Width, data = .)
#lm takes only formula, you can use '.' as data frame, only effective for tidyverse tho  

#for loop/lappy species
#lapply stands for list apply 
list_lm <- lapply(X = v_sp,
       function(s) {
         iris %>% 
           filter(Species == s) %>% 
           lm(Sepal.Width ~ Petal.Width, data = .)
       })

#basically its gonna show up as 1, 2, 3 when you show each line regression

##12.4.2------------------
#no matter if you add smth useless, r^2 will increase



#for loop/lapply

list_lm1 <- lapply(X = v_sp,
                  function(s) {
                    iris %>% 
                      filter(Species == s) %>% 
                      lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)
                  })

list_lm[[1]] %>%  summary()
list_lm1[[1]] %>%  summary()
