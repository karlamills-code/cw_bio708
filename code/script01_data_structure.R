##data structure
##scalar
a <- 2.0 #numrtic
b <- 4L  #integer
d <- "aquatic"

a
b
d

##vector is the same type of data and you cant mix it
va <- c(1.0, 2.3, 3) #numeric vector
vb <- c("a", "b", "c") #character vector
vc <- c("1", "2", "3.5")
vd <- c(1.0, "b") #it became a character but its super error prone DO NOT RUN 

va
vb
vc
#if you put double "" it will be recognized as a character vector

##matrix 
va <- c(1.0, 2.3, 3) #numeric vector
vb <- c(3, 2, 5.6) #numeric vector and overwrite prev object

ma <- cbind(va, vb) #bind by column
mb <- rbind(va, vb) #bind by row

mc <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2) #asking to use 2 rows/columns

ma
mb
mc