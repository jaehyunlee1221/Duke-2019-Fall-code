#exercise 1 
#what is the type of each vector?
#c(4L, 16, 0) <- double
#c(NaN, NA, -Inf) <- double? -Inf is double and there is coersion
#c(NA, TRUE, FALSE, "TRUE") <- character because of "True"
#c(pi, NaN, NA) <- double

typeof(c(4L, 16, 0))
typeof(c(NaN, NA, -Inf))
typeof(c(NA, TRUE, FALSE, "TRUE"))
typeof(c(pi, NaN, NA))

#exercise2
#Write a conditional statement that prints "Can't proceed NA or NaN present!" 
#if a vector contains NA or NaN. Test your code with vectors x and y below.

detect <- function(x){
  if(any(is.na(x))) print("Can't proceed NA or NaN present!")
}
x <- NA
y <- c(1:5,NaN,NA,sqrt(3))

detect(y)

#exercise3
#creat list using below data
#{
#  "id": {
#    "oid": "5968dd23fc13ae04d9000001"
#  },
#  "product_name": "sildenafil citrate",
#  "supplier": "Wisozk Inc",
#  "quantity": 261,
#  "unit_cost": "$10.47"
#},
#{
#  "id": {
#    "oid": "5968dd23fc13ae04d9000002"
#  },
#  "product_name": "Mountain Juniperus ashei",
#  "supplier": "Keebler-Hilpert",
#  "quantity": 292,
#  "unit_cost": "$8.74"
#}
#]

json <- list(id=c(list(type="oid", number="5968dd23fc13ae04d9000001"),
                  list(type="oid", number="5968dd23fc13ae04d9000002")),
             product_name=c("sildenafil citrate","Mountain Juniperus ashei"),
             supplier =c("Wisozk Inc","Keebler-Hilpert"),
             quantity =c(261,292),
             unit_cost =c("$10,47","$8.74"))

json2 <-list(
         list(
           id = list(oid = "5968dd23fc13ae04d9000001"),
           product_name = "sildenafil citrate",
           supplier = "Wisozk Inc",
           quantity = 261,
           unit_cost = "$10.47"
         ),
         list(
           id = list(oid = "5968dd23fc13ae04d9000002"),
           product_name = "Mountain Juniperus ashei",
           supplier = "Keebler-Hilpert",
           quantity = 292,
           unit_cost = "$8.74"
         )
       )
json2

?mean
