#if you have x,y,z and ou define a new variable to sum up all the new variables then if you add enough variables that follow the same kind of distribution, the sum will become approximated as a normal distribution. the poisson distribution looks like a normal when the mean is large
#poisson has a property called reproducability.. poisson is the sum of means
#df is calculating mean, SD or variance, and you need to have a mean to get SD


pacman ::p_load(tidyverse, patchwork)
rm(list = ls())

#this is for 10.5.3
df_fl <- read_csv("data_src/data_fish_length.csv")
print(df_fl)

# lab 10.5.1 --------------------------------------------------------------
xs <- rnorm(10, mean = 10, sd = 3)
ys <- rnorm(10, mean = 12, sd = 5)
xl <- rnorm(100, mean = 10, sd = 5)
yl <- rnorm(100, mean = 12, sd = 5)

#perform t-test for xs vs ys and xl vs yl

t.test(xs, ys, var.equal = FALSE)
t.test(xl, yl, var.equal = FALSE)
#false for x/ys has p vlaue of .86, df = 17.888
#false for x/yl has p value of 0.000474, df  = 197.85


t.test(xs, ys, var.equal = TRUE)
t.test(xl, yl, var.equal = TRUE)
#true for x/ys has p value of 0.86, df = 18
#true for x/yl has p value of 0.0004739, df =198


# lab 10.5.2 ---------------------------------------------------------------

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

#estimate sample mean/sd for each vector

#WRONG WAY OF DOING IT
#ab12 <- data.frame(a1, a2, b1, b2)
#tibble(ab12)

#ab12_mu <- ab12 %>%
  #groupby()

#simple approach has more probability for error
df_ab <- tibble(
  group = c(rep("a1", length(a1)),
            rep("a2", length(a1)),
            rep("b1", length(a1)),
            rep("b2", length(a1))),
  value = c(a1, a2, b1, b2)
)

#pivot longer - better approach
df_ab <- tibble(a1 = a1,
                a2 = a2,
                b1 = b1,
                b2 = b2) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "value"
  )

df_mu <- df_ab %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sig = sd(value))

df_ab %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(
    aes(x = group, y = value)
  ) +
  geom_jitter(
    height = 0, width = 0.1, alpha = 0.5 
  ) + #alpha is transparency and height is 0 so it doesnt change the graph
  geom_segment(
    data = df_mu %>% 
      filter(group %in% c("a1", "a2")), #filter so you dont have b1, b2 in the visual 
    aes(y = mu - sig,
        yend = mu + sig)
  ) +
  geom_point(
    data = df_mu %>% 
      filter(group %in% c("a1", "a2")),
    aes(y = mu),
    size = 2.5
  )

t.test(a1, a2)
t.test(b1, b2)


# lab 10.5.3 --------------------------------------------------------------
mu <- mean(df_fl$length)
sig <- sd(df_fl$length)

x <- rnorm(50, mean = mu, sd = sig)
y <- rnorm(50, mean = mu, sd = sig)


# right way to do for loop for 10.5.3------------------------------------------------
v <- NULL
R <- 100 #if you do 1000 youll get more normal distribution, 50000
for (i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(50, mean = mu, sd = sig)
  v[i] <- t.test(x, y, var.equal = TRUE)$statistic
}

#4
a <- df_fl %>% 
  filter(lake =="a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t_obs <- t.test(a, b, var.equal = TRUE)$statistic

tibble(v = v) %>% 
  ggplot(aes(x = v)) +
  geom_histogram()+
  geom_vline(xintercept = t_obs) +
  geom_vline(xintercept = -t_obs)

#5
mean(abs(v) > abs(t_obs))



# 50000  ------------------------------------------------------------------
v <- NULL
R <- 50000 #if you do 1000 youll get more normal distribution, 50000
for (i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(50, mean = mu, sd = sig)
  v[i] <- t.test(x, y, var.equal = TRUE)$statistic
}

a <- df_fl %>% 
  filter(lake =="a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t_obs <- t.test(a, b, var.equal = TRUE)$statistic

tibble(v = v) %>% 
  ggplot(aes(x = v)) +
  geom_histogram()+
  geom_vline(xintercept = t_obs) +
  geom_vline(xintercept = -t_obs)

mean(abs(v) > abs(t_obs))

