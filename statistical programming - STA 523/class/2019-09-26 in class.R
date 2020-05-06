#install.packages("stringr")
library(stringr)
#Exercise
text <- c(
  "9401-01-1020818",
  "(808) 437-9921",
  "HCY 4831"
)
str_detect(text,"\\(\\d\\d\\d\\)\\s\\d\\d\\d-\\d\\d\\d\\d")

text <- c("MY", "cell: ", "(808)-473-9921")
str_detect(text,"\\(\\d{3}\\)-\\d{3}-\\d{4}")


\\(\\d\\d\\d\\)\\s\\d\\d\\d-\\d\\d\\d\\d
--> \\d{3}


text <- "2 too two 4 for four 8 ate eight"
str_extract(text, "\\d.*\\d")
str_extract(c("fruit flies", "fly faster"), "[aeiou]{1,2}[a-z]+")

#Exercise2