rm(list = ls())

pacman::p_load(tidyverse, patchwork)


# normal distribution -----------------------------------------------------
x <- rnorm(50, mean = 10, sd = 3)

mu <- mean(x)
sigma <- sd(x)
pd  <- dnorm(x, mean = mu, sd = sigma)

x2 <- tibble(y = pd, x = x)

x2 %>% 
ggplot(aes(x=x, y=y)) +
  geom_line()
labs(y = "Probability density")

x_min <- floor(min(x))
x_max <- ceiling(max(x))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for(i in 1:(length(bin) - 1)) {
 p_up <- pnorm(bin[i+1], mean = mu, sd = sigma)
 p_low <- pnorm(bin[i], mean = mu, sd = sigma)
 
 p[i] <- p_up - p_low 
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(x2))


x2 %>% 
  ggplot(aes(x = x)) +
  geom_histogram(
    binwidth = 1,
    center = 0.5
  ) +
  geom_point(
    data = df_prob,
    aes(x = bin,
        y = freq)
  ) +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = 'salmon')

# poisson distribution ----------------------------------------------------

z <- rpois(n=1000, lambda = 10)

lambda <- mean(z)
bin <- seq(min(z), max(z), by = 1)

pm <- dpois(x = bin, lambda = lambda)

df_z <- tibble(z = z)

df_prob <- tibble(pm = pm, bin = bin) %>% 
  mutate(freq = pm * nrow(df_z))

df_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram(
    binwidth = 0.5,
    center = 0) +
  geom_point(
    data = df_prob,
             aes(x = bin,
                 y = freq
               ),
    color = "salmon") +
  geom_line(data = df_prob, 
            aes(x = bin, 
                y = freq),
                linetype = "dashed")
 

