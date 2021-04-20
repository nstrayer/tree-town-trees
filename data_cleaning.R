# install.packages("foreign")
# install.packages("leaflet")
# install.packages("leafgl")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(stringr)
library(leaflet)
library(leafgl)
library(sf)

diameter_levels <- c(
  '< 5"'   =  "0 - 5" ,
  '6" - 10"'  =  "6 - 10",
  '11" - 15"' =  "11 - 15" ,
  '16" - 20"' =  "16 - 20" ,
  '21" - 25"' =  "21 - 25" ,
  '26" - 30"' =  "26 - 30" ,
  '31" - 35"' =  "31 - 35" ,
  '> 35"'     = "GT35"
)

height_levels <- c(
  "< 10'"     = "0 - 10",
  "10' - 20'" = "10 - 20",
  "20' - 30'" = "20 - 30",
  "30' - 50'" = "30 - 50",
  "50' - 70'" = "50 - 70",
  "> 70'"     = "GT70"
)

# Downloaded from https://data.a2gov.org/feeds/GIS/Trees/A2Trees.zip on April 19th, 2020

x <- st_transform(st_read("A2Trees/"), "+proj=longlat +datum=WGS84 +no_defs") %>%
  select(
    common_genus = COMMONGENU,
    common_name = COMMONNAME,
    # botanical_genus = BOTANICALG,
    # botanical_name = BOTANICAL,
    # cultivar = CULTIVAR,
    # on_street = ONSTREET,
    street_side = SIDE,
    street = STREET,
    # lon = X_COORD,
    # lat = Y_COORD,
    geometry,
    diameter = DBH,
    # diameter_date = DBHDATE,
    height = HEIGHT,
    # height_date = HEIGHTDATE
  ) %>%
  filter(
    common_genus != "Vacant",
    common_name != "Stump",
    height != "N/A",
    !is.na(diameter)
  ) %>%
  mutate(
    diameter = factor(diameter, levels = diameter_levels, labels = names(diameter_levels)),
    height = factor(height, levels = height_levels, labels = names(height_levels))
  )

readr::write_rds(x, "tree-town/processed_trees.rds")

cutoff <- 200

x %>%
  count(common_name) %>%
  filter(n > cutoff) %>%
  mutate(
    name = reorder(paste0(common_name, ": _(", format(n, trim = TRUE,big.mark = ","), ")_" ), n)
  ) %>%
  ggplot(aes(x = n, y = name)) +
  geom_segment(aes(xend = 0, yend = name)) +
  geom_point() +
  ggtext::geom_richtext(
    aes(label = name),
    hjust = 0,
    nudge_x = 20,
    size = 2.8,
    fill = NA, label.color = NA
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0, 1500))
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),

  ) +
  labs(
    y = "",
    x = "Number of trees",
    title = "Most common trees maintained by City in Ann Arbor",
    subtitle = paste("Species with >", cutoff, "specimens. Label: common name, (# of trees)")
  )
