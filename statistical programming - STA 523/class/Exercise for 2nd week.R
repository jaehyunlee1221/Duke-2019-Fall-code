mydate <- "10.30.2019"
attr(mydate, which = "class") <- "Date"
mydatetime <- "10.30.2019-09:39"
attr(mydatetime, which = "class") <- "Date-times"
mydate
mydatetime


##Exercise 2 
set.seed(962019)
x <- sample(1:100, size = 100, replace = TRUE)
x

#select every third values from x beginning at position 2;
num <- 1:100
index <- num %% 3 == 2
x[index]
#remove all values with an odd index;
index <- num %% 2 == 1
x[!index]
#remove every 4th value, but only if it is odd;
index <- num %% 4 == 0 & x[num] %% 2 == 1
x[!index]
#remove all numbers divisible by 3 or 7 and replace them with 0.
index <- num %% 7 == 0 | num %% 3 == 0 
x[index] <- 0
x
