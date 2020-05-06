#2019-08-27

#exercise1
#what does each of the following return? run the code to chekc your answer
if (1 == "1") "coercion works" else "no coercion"
ifelse(5>c(1,10,2),"hello","olleh")

#exercise2
#consider two vectors x,y each of length one. Write a set of conditional that satisfy the following
#If x is positive and y is negative or y is positive and x is negative, print "knits".
#If x divided by y is positive, print "stink".
#Stop execution if x or y are zero.

x <- 4
y <- -2

if (x==0|y==0) {
  stop( "x or y are zero")
} else if (x*y<0) {
  print("knit")
} else if (x/y>0) {
  print("stink")
}

#exercise3
#Consider the vector x below.
#x <- c(3, 4, 12, 19, 23, 49, 100, 63, 70)
#Write R code that prints the square numbers in x.

#suggested answer sqrt(i) %% 1 ==0

x <- c(3,4,12,19,23,49,100,63,70)

for (i in x){
  if (sqrt(i) %% 1 ==0) {
    next
  }
  print(i)
}


#exercise4
#Consider z <- c(-1, .5, 0, .5, 1). Write R code that prints the smallest non-negative integer 
#k satisfying the inequality
#|cos(k)−z|<0.001 for each component of z.

z <- c(-1,0.5,0,0.5,1)
result <- rep(0,5)
for (i in seq_along(z)){
  k <- 0
  repeat{
    if(abs(cos(k)-z[i])<0.001) break
    k <- k+1
  }
  result[i] <- k
}
