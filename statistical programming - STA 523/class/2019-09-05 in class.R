#Exercise 1
x<- c(5,1,5,5,1,1,5,3,2,
      3,2,6,4,4,1,2,1,3)
attr(x=x, which ="dim") <-c(3,3,2)
x
attributes(x)

#Exercise2
z<- as.integer(c(1,2,3,4,1,4,5,6,6,7,3))
airport <- c("RDU","ABE","DTW","GRR","GNV","JFK","SFO")
attr(x=z, which = "levels") <- airport
attr(x=z, which = "class") <- "factor"
z
attributes(z)
#Exercis3
x <- letters[1:5]
y <- list(i=1:5,j=-3:3,k=rep(0,4))
x[[1]]
x[1]
y[[1]]
y[1]
x[-1]
y[2]
y[-2]
y[[2]][-1]
y[[2]][x<0]
str(y)
y[2]
y[[2]][y$j<0]
typeof(x)
x["a"]

y[[c(1,3)]]
y[[c(2,4)]]


##Exercise 1
x <- c(5, 1, 5, 5, 1, 1, 5, 3, 2, 
       3, 2, 6, 4, 4, 1, 2, 1, 3)
attr(x, which = "dim") <- c(3,3,2)
x


##Exercise 2
airport <- as.integer(c(1,2,3,4,1,4,5,6,6,7,3))
attr(airport, which = "levels") <- c("RDU", "ABE", "DTW", "GRR", "GNV", "JFK", "SFO")
attr(airport, which = "class") <- "factor"
attributes(airport)

##Exercise 3