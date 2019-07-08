library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggforce)
library(ggchicklet)

# import data
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# pull the top 15 franchises
top_15_franchises <- media_franchises %>%
  group_by(franchise) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(15, total_revenue) %>%
  pull(franchise)

# pull the top 5 original media types
top_5_media <- media_franchises %>%
  group_by(original_media) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(5, total_revenue) %>%
  pull(original_media)

# top grossing franchises plot
top_grossing_franchises <- media_franchises %>%
  filter(franchise %in% top_15_franchises) %>%
  group_by(franchise) %>% mutate(total_revenue = sum(revenue)) %>% ungroup() %>%
  ggplot(aes(x = reorder(franchise, total_revenue), y = revenue, fill = revenue_category)) +
  geom_chicklet(width = 0.75) +
  ggthemes::scale_fill_tableau("Tableau 20", name = NULL,
                               breaks=c("Book sales", "Box Office", "Comic or Manga", "Home Video/Entertainment", "Merchandise, Licensing & Retail", "Music", "TV", "Video Games/Games"),
                               labels=c("Book Sales", "Box Office", "Comic or Manga", "Home Video/Entertainment", "Merchandise, Licensing & Retail", "Music", "TV", "Games")) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = NULL, y = "Revenue (in billions)", title = "Highest Grossing Media Franchises",
       caption = "Data from Wikipedia\nPlot by Carter") +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom")

top_grossing_franchises

# top original media types plot
original_media_plot <- media_franchises %>%
  unique() %>%
  mutate(revenue = revenue * 1000000000) %>%
  filter(original_media %in% top_5_media) %>%
  ggplot(aes(x = original_media, y = revenue)) +
  geom_violin(alpha = 0.3, aes(fill = original_media)) +
  geom_sina(aes(fill = original_media), color = "black", size = 3, shape = 21) +
  scale_y_continuous(labels = scales::dollar_format()) +
  geom_label_repel(
    data = media_franchises %>%
      unique() %>%
      mutate(revenue_category = recode(revenue_category, "Merchandise, Licensing & Retail" = "Merchandise", "Video Games/Games" = "Games")) %>%
      filter(original_media %in% top_5_media) %>%
      filter(revenue > 19) %>%
      mutate(categ = paste0(franchise, " (", revenue_category, ")")),
    aes(x = original_media, y = revenue * 1000000000, label = categ), color = "black", fill = "wheat", family = "mono", size = 3) +
  labs(x = "Source of Original Media", y = "Total Revenue", title = "Revenue Generated for Major Media Types",
       caption = "Data from Wikipedia\nPlot by Carter") +
  theme_fivethirtyeight()

original_media_plot + theme(legend.position = "none")
