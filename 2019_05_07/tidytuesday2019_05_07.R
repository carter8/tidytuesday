# load packages
library(tidyverse)
library(rworldmap)
library(ggthemes)

# import data
student_ratio_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# create data subset with average student ratio
country_data <- student_ratio_data %>%
  filter(!is.na(student_ratio)) %>%
  group_by(country_code) %>%
  summarise(avg_student_ratio = mean(student_ratio))

# convert country data to world map format
world_data <- joinCountryData2Map(country_data, joinCode = "ISO3", nameJoinColumn = "country_code")

# plot world map
my_map <- mapCountryData(world_data, nameColumnToPlot= "avg_student_ratio",
                         addLegend = FALSE, mapTitle = "",
                         colourPalette = "rainbow",
                         catMethod = "quantiles", oceanCol = 'lightblue',
                         missingCountryCol = 'white') +
  title(main = "Average Student to Teacher Ratios",
        sub = "Data from 2012-2017 and includes all education levels courtesy of UNESCO Institute of Statistics\n
        Plot by Carter Schwartz")

# add custom legend to world map
do.call(addMapLegend, c(my_map, legendWidth = 0.5, legendMar = 5, legendShrink = 0.75,
                        legendLabels = "all", labelFontSize = 0.75))

# create data subset of indicators
indicator_data <- student_ratio_data %>%
  filter(str_detect(country, ".countries"),
         indicator %in% c("Primary Education", "Secondary Education"),
         year == "2017") %>% 
  select(country, year, indicator, student_ratio)

# plot indicator data
indicator_data %>%
  ggplot(aes(x = student_ratio, y = reorder(country, -student_ratio), fill = indicator)) +
  geom_point(size = 4, shape = 21, alpha = .75) +
  labs(title = "Student to Teacher Ratios in Countries by Income",
       subtitle = "difference is greater in primary education",
       x = "Student to Teacher Ratio",
       y = "",
       caption = "Data from UNESCO Institute of Statistics\nPlot by Carter Schwartz") +
  theme_solarized() +
  theme(legend.position = "top")
