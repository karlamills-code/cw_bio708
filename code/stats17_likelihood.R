#likelihood

pacman::p_load(tidyverse, patchwork)



# poisson example ---------------------------------------------------------

##probability of observing 3 given lambda = 3.5
(pr3 <- dpois(x = 3, lambda = 3.5)) #3 is probability obs no.3 and lambda is mean of poisson distribution
#x must be discrete and lambda (mean) is realization and it can be any number


##try 0.1 - 10 as ppossible values of lambda (intervals 0.1)

lambda <- seq(0.1, 10, by = 0.1)   

df_pois <- tibble(lambda = lambda,
       pr = dpois(3, lambda = lambda)) %>% 
  arrange(desc(pr))

#if ur model gives highest probability of ur dataset thats a good parameter for your model
#maximum probability of observations
#this is one observation at a time 
#when u want to define probability in ur dataset for example(below) because y = {3, 2, 5} which are the sites
#pr(y = 3) * pr(y =2) * pr(y =5) is how you get the max likelihood of probabilities for all three sites at the same time 

y <- c(3, 2, 5)

pr <- dpois(x = y, lambda = 3.5)

##manual calculation
lik <- pr[1] * pr[2] * pr[3]  #likelihood is probability this is the probability of obs your dataset with lambda at 3.5

##product
lik <- prod(pr)

lambda <- seq(0.01, 10, by = 0.01)

lik_i <- sapply(lambda,
                function(x) {
                  prod(dpois(y, lambda = x))
                })

df_pois3 <- tibble(
  lambda = lambda, lik = lik_i) %>% 
  arrange(desc(lik))

##sample mean of y
mean(y)

#we basically changed the lambda and arranged probability of y and we did the best value of lambda and this is the max likelihood of estimate


##why sample mean matches the MLE
##Pr(y = 3) with lambda = 3.5

dpois(3, lambda = 3.5)
(3.5^3 * exp(-3.5)) / factorial(3)

#when slope gives you 0, lambda gives you the max likelihood

##visualization 
df_pois3 %>% 
  ggplot(aes(x = lambda,
             y = log(lik)
             )) +
  geom_line() +
labs(x = "Lambda Value",
     y = "Likelihood")

#took the derivative and made the slope 0 to find the likelihood 
#you can generalize the method to have more parameters 

#need to max probability 

##get likely fit from glm 
df_count <- read_csv("data_src/data_garden_count.csv")

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = poisson)

logLik(m_pois)

summary(m_pois)

#log likelihood gets lower as the sample gets larger bc likelihood relies on the assumption that all samples are independent
#if samples are not independent then likelihood gets smaller
#spatial and temporal models are non-independence in dataset

