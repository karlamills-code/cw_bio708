
###you have to call library every single time
library(tidyverse)

##there are two types of pipe but use the tidyverse pipe - go to tools -> global options -> uncheck native pipe
# use CTRL + Shift + M = %>% 
#ggplot is a standalone package but tidyverse is a shortcut bundle package inc. ggplot

#click the document outline on top right corner to see the headline sections
#press Alt + O to see only one section 
#to get the section headline do Crtl + Shft + R

# Point figure ------------------------------------------------------------
###point figure - NEED AES to define what kind of graph you want or get error + mapping is argument name
iris %>% 
  ggplot(
      mapping = aes(x = Sepal.Length,
                    y = Sepal.Width)
  )+
  geom_point()


###color by species
#code is just different way to plot same thing above
#color needs to be within the aes function or R wont recognize what it's for
ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width,
                color = Species)
) + 
  geom_point()


# Line figure -------------------------------------------------------------

df0 <- tibble(
  x = rep(1:50, 3),
  y = x * 2
)
#dont need to define x or y since we already defined it in previous section above
df0 %>% 
  ggplot(
    mapping = aes(x = x,
                  y = y)
  ) + 
  geom_line()


# Histogram ---------------------------------------------------------------

iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) + 
  geom_histogram(color = "salmon")


# Box plot ----------------------------------------------------------------

#color argument changes the border color
#so you need to use fill argument to change the whole box color
iris %>% 
  ggplot(mapping = aes(x = Species,
                       y = Sepal.Length,
                      fill = Species)
         ) +
  geom_boxplot()


# Fun plot ----------------------------------------------------------------

#violin plot
iris %>% 
  ggplot(mapping = aes(x = Species,
                        y = Sepal.Length,
                        fill = Species)
          ) +
  geom_violin()

#you can flip the x and y so you can a more readable violin plot
#ggplot2 is the package name but ggplot is the function 
