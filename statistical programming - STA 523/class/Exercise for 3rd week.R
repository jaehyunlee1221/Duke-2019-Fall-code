library(tidyverse)
flint <- read_csv("https://raw.githubusercontent.com/sta523-fa19/data/master/flint.csv")
index <- flint$zip == 48529 | flint$zip == 48502
flint <- flint[!index,] %>% 
  mutate(zip = as.factor(zip))

ggplot(data = flint, mapping = aes(x = zip, y = draw1)) +
  geom_boxplot(fill = "#256d7b") +
  geom_hline(yintercept = 15, color = "red", linetype = 2, size = 1.3, alpha = 0.8) +
  coord_flip() +
  labs(title = "First draw of lead samples in flint, MI homes", x = "Zip code", y = "Lead content(ppb)") +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0,165,15)) +
  theme(axis.text.x =  element_text(size = 16), axis.text.y =  element_text(size = 16)) +
  annotate("text",x = 0.8, y = 40, label = "EPA action level, 15 ppb", color = "red")
