# CTRL + Shift + N creates new project

##call package every single time 
library(tidyverse)

##when you see x subscript 1 that is the first value of the first character
##seeing a subscript is an element describing the vector
##for example x = 15.1, 24.2, 35.4, 23.4, n)
##arithmetic mean used in a spatial analysis + good stats with normal distribution data but sensitive to outliers
##geometric used for temporal
##median mean used for ?????

x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

x
y


#arithmetic mean 
(mu_x <- sum(x) / length(x))
mean(x)
##if you want to print the value while still assigning, put parenthesis around it while assigning it. See line 21

mu_y <- sum(y)/ length(y)
mean(y)


#geometric mean
#option 1
mu_xge <- prod(x)^(1/length(x))
#option 2
exp(sum(log(x)) / length(x))

(mu_y_ge <- exp(sum(log(y))/ length(y)))
##ask why my y is the same exact value as x in geomean

#median
med_x <- median(x)
med_y <- median(y)


# variation ---------------------------------------------------------------
##CTRL + Shift + R is to make section label

#variance
(var_x <- sum((x - mean(x))^2) / length(x))
##square it to not mix positive/negative values
##this is arith mean of the deviation 
(var_y <- sum((y - mean(y))^2) / length(y))

sqrt(var_x)
sqrt(var_y)

#inter-quartile range
x_l <- quantile(x, 0.25)
x_h <- quantile(x, 0.75)
(iqr_x <- x_h - x_l)

y_q <- quantile(y, c(0.25, 0.75))
(iqr_y <- y_q[2] - y_q[1])
##cause we know we only have 2 elements in the vector we can represent it as 1 and 2
##IQR is more robust to outliers 


#MAD
ad_x <- abs(x - median(x))
median(ad_x)
##median of the absolute SD

ad_y <- abs(y-median(y))
median(ad_y)

#Coefficient of variation
##different from others SD/arithmetic mean 
##measuring degree note variation across different vectors/datasets or different sites/populations
##rarely using SD for variation unless specified

sd_x <- sqrt(var_x)
(cv_x <- sd_x /mu_x)


# EXTRA -------------------------------------------------------------------
##there is a function called function when you are defining your own function 
##there is no tool called cv but you can define yours BRUH
##lets say we want to use v as a vector function
##you put in the () the function variable youd like to use

mycv <- function(v) {
  mu<- mean(v)
  s <- sd(v)
  cv <- s/mu
  
  return(cv)
}

mycv(x)
#we used a different calculation is SE
##you can skip writing the script over n over by using function
##cause it did it internally rather than externally showing each process
##people will write it as a function and make it a package bundle and then release it to the public
##function becomes critical skill set to avoid errors instead of copying n pasting  