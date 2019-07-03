library(tidyverse)
library(janitor)
library(scales)
library(countrycode)
library(fuzzyjoin)
library(ggthemes)

# import data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

# clean each dataset
coast_vs_waste <- coast_vs_waste %>% clean_names() %>%
  rename(country = entity, country_code = code) %>%
  filter(year == 2010) %>% #2010 is the only year with plastic data
  select(-year)

mismanaged_vs_gdp <- mismanaged_vs_gdp %>% clean_names() %>%
  rename(country = entity, country_code = code) %>%
  filter(year == 2010) %>%
  select(-year)

waste_vs_gdp <- waste_vs_gdp %>% clean_names() %>%
  rename(country = entity, country_code = code) %>%
  filter(year == 2010) %>%
  select(-year)

# join datasets together and rename columns
plastic_waste <- coast_vs_waste %>%
  select(-total_population_gapminder) %>% #variable is already in the other datasets
  inner_join(mismanaged_vs_gdp %>%
               select(-total_population_gapminder), by = c("country", "country_code")) %>%
  inner_join(waste_vs_gdp, by = c("country", "country_code")) %>%
  select(country, country_code,
         mismanaged_waste = mismanaged_plastic_waste_tonnes,
         coastal_population,
         total_population = total_population_gapminder,
         mismanaged_per_capita = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
         gdp_per_capita = gdp_per_capita_ppp_constant_2011_international_rate) %>%
  filter(!is.na(mismanaged_waste))

# get continent names for each country
plastic_waste <- plastic_waste %>%
  mutate(continent = countrycode(sourcevar = plastic_waste$country, origin = "country.name", destination = "continent"),
         continent = case_when(country == "Micronesia (country)" ~ "Oceania",
                               TRUE ~ continent)) %>%
  drop_na(continent)

# plot 1, waste vs income
waste_v_income <- plastic_waste %>%
  arrange(-total_population) %>%
  ggplot(aes(x = gdp_per_capita, y = mismanaged_per_capita)) +
  geom_point(aes(size = total_population, color = continent)) +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = .5) +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10(labels = dollar_format()) +
  scale_y_log10() +
  scale_size_continuous(guide = FALSE) +
  labs(x = "GDP per capita",
       y = "Mismanaged plastic waste (kg per person per day)",
       title = "Mismanaged Waste vs Country Income",
       caption = "Data from Our World in Data, 2010\nPlot by Carter")

waste_v_income

# join plastic data with map data
tbl_df(iso3166)
plastic_waste_data <- plastic_waste %>%
  inner_join(iso3166, by = c("country_code" = "a3"))

# plot 2, world map
map_data("world") %>%
  tbl_df() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(plastic_waste_data, by = c("region" = "mapname")) %>%
  ggplot(aes(long, lat, group = group, fill = mismanaged_per_capita)) +
  geom_polygon() +
  scale_fill_gradient2(trans = "log10", low = "green", high = "red",
                       mid = "pink", midpoint = log10(.02)) +
  coord_fixed(2) +
  theme_map() +
  labs(fill = "Mismanaged plastic waste\nper capita",
       title = "Which countries have the most mismanaged waste?",
       caption = "Data from Our World in Data, 2010\nPlot by Carter")
