#model comparison lab
rm(list = ls())

pacman::p_load(patchwork, tidyverse, palmerpenguins, janitor, MuMIn)

#cleaning names
df_pen <- penguins_raw %>% 
  clean_names()

#convert yes Nor to 1/0 info
df_pen <- df_pen %>% 
  mutate(clutch_completion = ifelse(clutch_completion == "Yes",
                                    1, 0)
         #can do this to make it in one code rather than breaking it up, choice up to you 
         #, species = case_when(species == sp[1] ~ "adelie",
         #species == sp[2] ~ "gentoo"
         #species == sp[3] ~ "chinstrap"
         )

#species column
sp <- unique(df_pen$species)

df_pen <- df_pen %>%             #this is order sensitive approach and needs to be careful when doing this
  mutate(species = case_when(species == sp[1] ~ "adelie",
                             species == sp[2] ~ "gentoo",
                             species == sp[3] ~ "chinstrap")
         ) %>% 
  drop_na(
    culmen_length_mm,
    culmen_depth_mm,
    flipper_length_mm,
    body_mass_g
  )

#if you encounter na omit function bc it can remove rows that contain any NA

#install mumin and run it(look at line 4)

m <- glm(clutch_completion ~ species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + body_mass_g,
        data = df_pen, 
        family = binomial)

#model selection with dredge
options(na.action = "na.fail")

m_set <- dredge(m, rank = "AIC") #AICC is corrected AIC for small sample size and normal distribution
m_sub <- subset(m_set, delta < 2) #this only picks the delta AIC that is less than 2. So it filtered out only 4 models with comparable performance 

#lowest aic at top and is the best model and delta is deviation of aic is observed in 1,2,3,4...n models. 
#delta AIC less than 2 is competitive model and the top 4 is more comparable model.
#this is good when you dont have a clear hypothesis 

list_m <- get.models(object = m_set, subset = delta < 2) #this gives model details from top to bottom
summary(list_m[[1]])

#basically when trying to max predictive capacity of model it tends to be more significant after model selection but AIC and significant have different philosophy so its difficult to present model results.
#show just model output with estimate and standard error to show involved but not show p value which is fit to data set you have based on observed data set. AIC is based on prediction so its different from p-value. 
