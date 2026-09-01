#install.packages("tidyverse")

library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#filter
filter(iris_sub, Species == "virginica")

filter(iris_sub, Species != "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, Sepal.Width > 5,
       Species == "versicolor")

arrange(iris_sub, Sepal.Length)
#default is descending order

filter(iris_sub, Sepal.Length >= 5)

#exercise for filter
iris_3 <- filter(iris_sub, Sepal.Width > 3)
iris_3

iris_setosa <- filter(iris_sub, Species != "setosa")
iris_setosa

iris_3_setosa <- filter(iris_sub,
                        Sepal.Length > 3 | Species == "setosa")
iris_3_setosa



#filter is making the data fatter lol 
#Select
select(iris_sub, Sepal.Length)

select(iris_sub, c(Sepal.Length, Sepal.Width))

select(iris_sub, starts_with("Sepal"))

#select is taking out columns and mutate is to add columns
select(iris_sub, -ends_with("Width"))
select(iris_sub, -starts_with("Sepal"))

#Mutate
(x_max <- nrow(iris_sub))

x <- 1:x_max

iris_sub_row <- mutate(iris_sub, row_id = x)

arrange(iris_sub_row, desc(row_id))

mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

mutate(iris_sub,id_name = paste(Species, Sepal.Length))

#exercises for select/mutate
iris_pw <- select(iris_sub, c(Petal.Width, Species))
iris_pw

iris_petal <- select(iris_sub, starts_with("Petal"))
iris_petal

iris_pw_two <- mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
iris_pw_two



#Pipe %>%
#pipe string together a series of operations
iris_sub %>%
  filter(Species == "virginica")

select(filter(iris_sub, Species == "virginica"), Sepal.Length)


#exercise for pipe
select(filter(iris_sub, Species == "Setosa"), Sepal.Length)

iris_sub %>% filter(Species == "setosa")

iris_pipe <- mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
iris_pipe



#Grouping
iris_sub %>%
  group_by(Species) %>%
  mutate(mu_sl = mean(Sepal.Length)) %>%
  ungroup()

#need to use ungroup to prevent errors in sunsequent operations after grouping operations 
  
#Summarize 
  iris_sub %>%
    group_by(Species) %>%
    summarize(mu_sl = mean(Sepal.Length))