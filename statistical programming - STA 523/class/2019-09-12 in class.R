library(tidyverse)
library(ggplot2)
teams <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/teams.csv")
class(teams)
head(teams)
str(teams)

#Exercise1 YearID HR lgID=AL divID=E
index <- teams$lgID == "AL" & teams$divID == "E"
Exe_team <- teams[index,]

attributes(Exe_team$Rank)
str(Exe_team)

ggplot(data=Exe_team, mapping = aes(x=yearID,y=HR,color=factor(Rank))) +
  geom_point(show.legend = T) +
  facet_wrap(~teamID) +
  xlab("Year") + ylab("Home Runs") +
  labs(color = "Division Rank")


energy <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/energy.csv")
energy
as_tibble(energy)


#Exercise2
ord_energy_mwh <- energy[order(energy$MWhperDay,decreasing = T),]
top_30 <- ord_energy_mwh[1:30,]

ggplot(data=top_30,mapping = aes(x=reorder(name,MWhperDay),y=MWhperDay)) +
  geom_bar(stat="identity",color="white",
           fill=c("#9d8b7e", "#315a70", "#66344c", 
                  "#678b93", "#b5cfe1", "#ffcccc")) +
  labs(y="Daily MWh(in thousands)", x="Power source",
       title = "Top 30 power source energy generators",
       caption = "1 MWh is, on average, enough power for 28 people in the USA") +
  coord_flip()


tibble(top_30)
levels(top_30$type)
