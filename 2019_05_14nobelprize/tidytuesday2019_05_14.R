library(tidyverse)
library(lubridate)
library(ggthemes)

# import data
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

# turn prize year into a date
nobel_winners$prize_year_date <- ymd(paste(as.character(nobel_winners$prize_year),"-12-31", sep=""))

# create age at time of prize variable
nobel_winners$age <- interval(nobel_winners$birth_date, nobel_winners$prize_year_date) %>% 
  time_length(unit = "year") %>% round %>% as.numeric()

# plot data
p <- nobel_winners %>% arrange(prize_year) %>%
  filter(!is.na(age)) %>% 
  ggplot(aes(x = prize_year, y = age, color = category)) + 
  geom_point(alpha = 0.50) +
  geom_smooth(method = 'loess', formula = y ~ x) +
  facet_wrap(~ category, ncol = 2) +
  labs(x = "Year the Nobel Prize was awarded", y = "Age of prize winner",
       title = "Age of Nobel Prize Winners",
       subtitle = "Average age has increased for all categories except peace",
       caption = "Plot by Carter") + theme_fivethirtyeight()

p + theme(legend.position = "none")
