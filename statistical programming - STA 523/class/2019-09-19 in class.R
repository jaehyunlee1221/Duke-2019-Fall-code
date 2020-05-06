###Exercise1
sqf <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/sqf.csv")

library(dplyr)
###Exercise
sqf <- read.csv("https://raw.githubusercontent.com/sta523-fa19/data/master/sqf.csv")
tibble(sqf)

#1.When did the first 4 events occur in 2003 and what was the suspected crime? 
#Return a data frame with datestop, timestop, and crimsusp.

sqf %>% 
  filter(year == 2003) %>%
  arrange(datestop) %>%
  slice(1:4) %>%
  select(datestop, timestop, crimsusp)

#2.What was the reported hair color, eye color, and weight 
#for the first time there was a suspected crime of "HUMICIDE"? 
#Yes, "HUMICIDE", not "HOMICIDE".

sqf %>%
  filter(crimsusp == "HUMICIDE") %>%
  select(haircolr,eyecolor,weight)


#Choose a subset of five variables and 10 random rows from sqf and
#save it as an object named sqf_mini. 
#Experiment renaming variables with select() and rename() on sqf_mini. 
#What is the difference between the two functions?

sqf_mini <- sqf %>%
  select(1:5) %>%
  sample_n(10)

sqf_mini %>%
  select(year) %>%
  rename(years=year)



#Where a race is available, what was the mean age of individuals 
#that were stopped, questioned, or frisked for each racial demographic?
names(sqf)
sqf %>%
  group_by(race,othpers) %>%
  summarise(
    age_mean = mean(age,na.rm = T)
  )

sqf %>% 
  filter(!is.na(race)) %>% 
  group_by(race) %>% 
  summarise(mean = mean(age, na.rm = TRUE))

#Which three precincts had the most individuals frisked and searched, 
#where a pistol was found in the person's possession? Include the counts.
names(sqf)
sqf %>%
  group_by(pct) %>%
  filter(othpers == "Y") %>%
  summarise(
    n=n()
  )
sqf %>% 
  select(pct, frisked, searched, pistol) %>% 
  filter(frisked == "Y", searched == "Y", pistol == "Y") %>% 
  group_by(pct) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:3)

#For each sex race combination, 
#what was the relative frequency of an SQF event?

sqf %>%
  group_by(sex,race) %>%
  summarise(
    n = n()
  )
