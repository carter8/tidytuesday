library(tidyverse)
library(ggrepel)

# import data sets
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# get the names of the top winners
top_winners <- grand_slams %>%
  group_by(gender, name) %>%
  summarise(top_n_wins = max(rolling_win_count)) %>%
  group_by(gender) %>%
  filter(top_n_wins >= 8) %>%
  arrange(gender, desc(top_n_wins)) %>%
  pull(name)

# create data set of only the top winners and their last victory
# used to highlight and label the top winners
top_winners_data <- grand_slams %>%
  filter(name %in% top_winners) %>% 
  group_by(name) %>%
  filter(rolling_win_count == max(rolling_win_count)) %>% 
  ungroup()

# plot data
my_plot <- grand_slams %>%
  ggplot(aes(x = tournament_date, y = rolling_win_count)) +
  geom_step(aes(group = name), color = "grey") +
  geom_step(aes(color = name), data = grand_slams %>% filter(name %in% top_winners)) +
  geom_label_repel(data = top_winners_data, aes(label = name, color = name)) +
  facet_wrap(~ gender, ncol = 1) +
  guides(color = FALSE) +
  labs(title = "Number of Grand Slam wins through time",
       subtitle = "Players with 8 or more wins are highlighted",
       x = "Date",
       y = "# of wins",
       caption = "Plot by Carter Schwartz")
