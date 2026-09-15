#descriptive stats - lab 
library(tidyverse)

# central tendency --------------------------------------------------------

(z <- exp(rnorm(n = 100, mean = 0, sd = 0.1)))

#arith
(zarith <- (sum(z)/length(z)))
#geom
(zgeom <- exp(sum(log(z)) / length(z)))
#median
(zmed <- median(z))

#histogram
ztibbl <- tibble(z) 

ztibbl %>% 
  ggplot(mapping = aes(x = z)) +
  geom_histogram()+
  geom_vline(xintercept = zarith, color = "red") +
  geom_vline(xintercept = zgeom, color = "green") +
  geom_vline(xintercept = zmed, color = "blue")

#for zrev
(z_rev <- -z + max(z) + 0.5)
#arith
(zrevarith <- (sum(z_rev)/length(z_rev)))
#geom
(zrevgeom <- prod(z_rev)^(1/length(z_rev))
)#median
(zrevmed <- median(z_rev))

#histogram
zrevtibbl <- tibble(z_rev) 

ztibbl %>% 
  ggplot(mapping = aes(x = z_rev)) +
  geom_histogram()+
  geom_vline(xintercept = zrevarith, color = "red") +
  geom_vline(xintercept = zrevgeom, color = "green") +
  geom_vline(xintercept = zrevmed, color = "blue")

# variation  --------------------------------------------------------------
w <- rnorm(100, mean = 10, sd = 1) 
head(w)

m <- c(w*1000)

sd_w <- sqrt(sum((w - mean(w))^2) / length(w))
mad_w <- median(abs(w - median(w)))

sd_m <- sqrt(sum((m - mean(m))^2) / length(m))
mad_m <- median(abs(m - median(m)))

#CV and MAD/Median for w and m
#CV is a very common measure in ecology
sd_w / mean(w)
sd_m / mean(m)

mad(w)/median(w)
mad(m)/median(m)