#least square method is a specialized likelihood 

#easiest model to make is a model comparison
#perfect model assigns different parameters to each data point
#generalizability is the issue bc you making a specific parameter for each data point
#common pitfall when comparing model
#when making complex model comparison is not creating a generalized prediction of the samples

pacman::p_load(tidyverse, patchwork)


# model fit ---------------------------------------------------------------
set.seed(1)

#sample size
n <- 100

##true intercept and slope
b <- c(0.1, 0.5)

##hypothetical predictors
x1 <- rnorm(n = n, mean = 0, sd = 1)

#design matrix
X <- model.matrix(~x1)

#simulated u
y_hat <- X %*% b    #this will create a generalized linear pattern
y <- rnorm(n=n, mean = y_hat, sd = 0.5)

df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(x = x1, 
                   y = y
                   )) +
  geom_point()


# linear model fit --------------------------------------------------------
m1 <- lm(y ~ x1, df0)
summary(m1)

#adding an unrelated variable
df0 <- df0 %>% 
  mutate(x2 = rnorm(n = n))

m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)

#adjusted rquare decreased when you add useless variables in ur dataset
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared


#you cant use this measure for binomial or poisson, so in glm you will get deviance instead to get model complexity or balance



# non normal case ---------------------------------------------------------
df_count <- read_csv("data_src/data_garden_count.csv") %>% 
  mutate(moisture = runif(nrow(.), 0, 100))


m_pois0 <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

m_pois1 <- glm(count ~ nitrate + moisture,
               data = df_count,
               family = "poisson")

#null deviance says 48.6 with residual deviance says 19.07 which is info we can use
s_pois0 <- summary(m_pois0)
s_pois1 <- summary(m_pois1)



##for model0 with nitrate only
pseudoR0 <- 1 - (s_pois0$deviance / s_pois0$null.deviance)

##adj model0 with nitrate only
adj.pseudoR0 <- 1 - ((s_pois0$deviance - 1) / s_pois0$null.deviance)

##for model1 with nitrate + moisture
pseudoR1 <- 1 - (s_pois1$deviance / s_pois1$null.deviance)

#adj model1 with nitrate + moisture
adj.pseudoR1 <- 1 - ((s_pois1$deviance - 2) / s_pois1$null.deviance)



# likelihood ratio test ---------------------------------------------------
#versatile method that can be used in variety context and highly recommended by Dr. Terui
#the likelihood is the measure of how much model can represent data set as a probability
#statistic that is called likelihood ratio that follows certain probability distribution when sample size is large

##normal case using m1 & m2
anova(m1, m2, test = "Chisq")

#model 1 only contains x1 but model 2 takes x1 + s2 and it calculated log likelihood ratio, and its showing if x2 gives more data 
#whether obs improvement greater than random chance or not
#p value saying improvement by adding x2 is greater than expected compared to random chance

##trying poisson model 
anova(m_pois0, m_pois1, test = "Chisq")
#why is my p_test not 0.286 but 0.7849 ????????????????????????????????????????????????????????
#when trying to do likelihood ratio test, you need to make sure it has a nested structure

#these two models are evaluating the fit to the data 
#accounts for how complex the model is but doesn't translate well into the prediction 

#next approach considers real prediction based on sample prediction 
#chitest info criterion or AIC is out of sample prediction based metric, and want to pick model based on model capacity to use for model fit
#it can predict model you omitted in the model based on sample prediction
#AIC selects model that maximizes that capacity of prediction 
#usually model with adjusted high r square model are good with AIC model but sometimes can diverge
#if you interested in prediction of which model is the best 


# AIC ---------------------------------------------------------------------
## out-of-sample prediction
## AIC = 2k - 2 ln L
###it looks simialr to orderly adjustment for rsquare 

# get AIC value from a model
AIC(m1)

AIC(m2)

#if adding one variable to the AIC and it doesnt improve then it means the data isnt improving model capacity

AIC(m_pois0)
AIC(m_pois1)

#AIC not popular in cell or molecular because its primarly experimental but more common in ecology 
#AIC can choose model based on model capacity 
#there is debate on topic of foundation on statistics
#AIC has high predictive capacity but not causal relationships


#adjusted rsq, likelihood ratio test, and AIC practical prediction, AIC doesn't need to be nested, so basically you can do one variable defined by two different data i.e. y = x vs. y = z

##when you see |AIC1 - AIC2| > 2, their model's predictive capacities are meaningfully different
#AIC model greater than 2 then its meaningfully different
#AIC model used in ecology bc of multiple predictor variables and consider all possible combinations, run AIC, run lowest to highest, and see what remains in top right
#need to be careful about why we use AIC for statistical reasoning
