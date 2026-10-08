pacman::p_load(patchwork, tidyverse, pwr)
rm(list = ls())

distinct(PlantGrowth, group)

#df_pg <- as_tibble(PlantGrowth)

#df_pg %>%     why he do this??????????
  #mutate(
   #group_label = case_when(
      #group == "ctrl" ~ "control",
      #group == "trt1" ~ "Treatment 1",
      #group == "trt2" ~ "Treatment 2"
    
PlantGrowth %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(
    draw_quantiles = 0.5,
    alpha = 0.2
  ) +
  geom_jitter(
    height = 0,
    width = 0.1
  ) +
  labs( x = "Treatment Group",
        y = "Weight")

aov(weight ~ group,
    data = PlantGrowth)

summary(aov(weight ~ group, PlantGrowth))


#ill discuss values later


# Power Analysis ----------------------------------------------------------
#very good with designing an experiment
pwr::pwr.anova.test(f = 0.5,          #effect size and magnitude of group difference
                    k =3,             #number of groups/independent variable
                    n = NULL,         #sample size of group
                    power = 0.8,      #detecting true difference of groups and sample size
                    sig.level = 0.05) #there is no mathematical justification for 0.05 for significance 

#EXPERIMENT W/ POWAAAAAaaaaa-----
#compare no. of groups affecting power
pwr::pwr.anova.test(f = 0.5,          
                    k =3,             
                    n = 10,         
                    sig.level = 0.05)

#compare no. samples per group affecting power
pwr::pwr.anova.test(f = 0.5,          
                    k =10,             
                    n = 3,         
                    sig.level = 0.05)

#compare power to sample size
pwr::pwr.anova.test(f = 0.5,          
                    k =3,             
                    power = 0.9,         
                    sig.level = 0.05)

#compare power to no. of groups
pwr::pwr.anova.test(f = 0.5,
                    n = 15,
                    power = 0.9,         
                    sig.level = 0.05)

#compare sig level to power
pwr::pwr.anova.test(f = 0.5,          
                    k =3,             
                    n = 10,
                    power = 0.8,
                    sig.level = NULL)
