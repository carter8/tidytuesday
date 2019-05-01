# load packages
library(tidyverse)
library(ggthemes)
library(ggridges)

# import dataset
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# remove synopsis and background from data to free up memory
new_tidy_anime <- tidy_anime %>% select(-synopsis, -background)

# filter out observations with missing data
plot_data <-  new_tidy_anime %>% 
  filter( !is.na(score), !is.na(source))

# create plot
plot_data %>%
  filter(type == "TV") %>%
  ggplot(aes(score, reorder(source, score, median))) +
  geom_density_ridges(quantile_lines = TRUE,
                      quantiles = 2,
                      size = .75) +
  theme_solarized() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(0,10,1)) +
  labs(title = "Which source produces the highest rated TV Animes?",
       subtitle = "Mangas and novels have the highest average score",
       x = "Fan Score",
       y = "Source of Anime Series",
       caption = "Data from Tam Nguyen and MyAnimeList.net via Kaggle\nPlot by Carter Schwartz")
