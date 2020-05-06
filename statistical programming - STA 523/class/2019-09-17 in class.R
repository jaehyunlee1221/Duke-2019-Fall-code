metals_usd <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/metals_usd.csv")
congress <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/congress_long.csv")
nfl <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/nfl_combine.csv")
flint <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/flint.csv")

###Exercise cover everything with dataset flint
###bring packages need in analysis
#install.packages("gapminder")
#install.packages("ggcorrplot")
#install.packages("ggpol")
#devtools::install_github("thomasp85/patchwork")
#install.packages("gganimate")
#install.packages("ggiraph")
library(gapminder)  # some data
library(tidyverse)
library(ggcorrplot) # correlogram plots
library(ggpol)      # parliment plot
library(patchwork)  # combining plots
library(gganimate)  # animations
library(ggiraph)    # interactive plots
library(ggplot2)

### correlation matrix
corr_mat <- round(cor(metals_usd[-1]), 2) # compute correlation matrix
ggcorrplot(corr = corr_mat, method = "circle", type = "full", lab = TRUE,
           colors = c("tomato2", "white", "springgreen3"),
           ggtheme = theme_bw)

### parliment plot
ggplot(data = congress[congress$year_start == 1913 & congress$branch == "house", ]) +
  geom_parliament(aes(seats = seats, fill = factor(party)), show.legend = TRUE, color = "black") +
  scale_fill_manual(values = c("#3A89CB", "#D65454", "#BF6FF0", "Grey"), 
                    labels = c("Dem", "GOP", "Other", "Vacant")) +
  labs(fill = "Party") +
  coord_fixed() +
  theme_void(base_size = 20)






tibble(flint) 
### correlation plot
corr_mat <- round(cor(flint[,4:6]),2)
ggcorrplot(corr=corr_mat, method = "circle", type = "lower",
           colors = c("yellow","orange","red"), 
           ggtheme = theme_light) +
  labs(title = "correlation matrix between draws", subtitle = "water quality")


### ggpol
ggplot(data=flint) +
  geom_parliament(aes(seats=ward,fill=factor(zip)), show.legend = T, color="black") +
  #scale_fill_manual(values = draw1)
  labs(fill = "zip")




### free ggplot

ggplot(data=flint,aes(x=factor(zip),y=draw1,fill=ward)) +
  geom_point()

ggplot(data=flint,aes(x=factor(zip),y=draw1,fill=ward)) +
  geom_boxplot()

flint[flint$zip== 48502, ]

tibble(flint)
range(flint$zip)
nlevels(as.factor(flint$zip))
tibble(congress)
