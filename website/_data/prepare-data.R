#
#   Creates World.rds, separation plots
#

library(cshapes)
library(leaflet)
library(sf)
library(rgeos)
library(readr)
library(dplyr)
library(tidyr)
library(countrycode)
library(here)
library(tmap)
library(stringr)
library(ggplot2)


setwd(here::here("website/_data"))


# World map data ----------------------------------------------------------

fcasts <- read_rds("fcasts.rds")
fcasts <- fcasts %>% dplyr::filter(year==max(year))
fcasts$iso_a3 <- countrycode(fcasts[["gwcode"]], "gwn", "iso3c")
fcasts <- fcasts %>% 
  mutate(p = 100*p) %>%
  tidyr::pivot_wider(names_from = "outcome", values_from = "p") %>%
  dplyr::select(iso_a3, attempt, coup, failed)

# base map data is from chshapes
world <- suppressWarnings(cshp(date = as.Date("2016-01-01"))) %>%
  st_as_sf() %>%
  # it has some wrong nodes, remove those 
  st_crop(ymin = -90, ymax = 90, xmin=-180, xmax=180) %>%
  select(ISO1AL3, CNTRY_NAME) %>%
  dplyr::rename(iso_a3 = ISO1AL3, name = CNTRY_NAME) %>%
  mutate(iso_a3 = as.character(iso_a3), name = as.character(name))

# cshapes does not have Antarctica and Greenland, add those in
# But use natural earth for shapes; cshapes does not have Greenland and 
# Antarctica polygons. 
add_in <- st_read("ne_10m_admin_0_countries") %>%
  filter(NAME %in% c("Greenland", "Antarctica")) %>%
  dplyr::rename(iso_a3 = ADM0_A3, name = NAME) %>%
  select(iso_a3, name) 

# combine
world <- world %>%
  st_transform(4326) %>%
  rbind(add_in)

# fix country names
world <- world %>%
  mutate(iso_a3 = case_when(
    name=="Kosovo" ~ "RKS",
    TRUE ~ iso_a3
  )) %>%
  mutate(
    name = str_remove(name, " \\([a-zA-Z]+\\)"),
    name = case_when(
      name=="Macedonia" ~ "North Macedonia",
      name=="Germany Federal Republic" ~ "Germany",
      name=="Egypt (United Arab Republic)" ~ "Egypt",
      TRUE ~ name
    ))

world <- world %>%
  left_join(fcasts) %>%
  select(iso_a3, name, attempt, coup, failed) 

world_simple <- world %>%
  st_transform(crs = "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
  st_simplify(dTolerance = 15000, preserveTopology = FALSE) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 


saveRDS(world_simple, "World.rds")



# Separation plots --------------------------------------------------------

fcasts <- read_rds("fcasts.rds") %>%
  filter(!is.na(observed)) %>%
  # use bottom up attempt
  filter(outcome!="attempt") %>%
  mutate(outcome = ifelse(outcome=="attempt2", "attempt", outcome)) %>%
  group_by(outcome) %>%
  arrange(p) %>%
  # the data are grouped by outcome and ordered by p; the index will become
  # the x-values in the sepplots
  dplyr::mutate(index = 1:n()) %>%
  ungroup() %>%
  # make outcome a factor so we get nicer labels in the plot facets
  # need to do this outside the grouping because otherwise we can't modify 
  # outcome
  mutate(
    outcome = factor(
      outcome, 
      levels = c("attempt", "coup", "failed"),
      labels = c("Any Coup Attempt", "Successful Coup", "Failed Coup Attempt")
    )
  )

cols <- rev(c("#FEF0D9", "#E34A33"))

p <- ggplot(fcasts, aes(x = index, y = 1)) +
  facet_wrap(~ outcome, ncol = 1) + 
  geom_bar(aes(fill = observed), stat = "identity", position = "identity", width = 2) +
  # highlight positive cases
  geom_bar(data = fcasts %>% filter(observed==1),
           aes(fill = observed), stat = "identity", position = "identity", width = 3) +
  geom_step(aes(y = p), colour = "black") +
  scale_fill_manual(guide = FALSE, values = cols) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank()) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(x = "", y = "")

ggsave(plot = p, filename = "../img/sepplot.png", height = 4, width = 5)



# Update downloadable CSV -------------------------------------------------

fcasts <- read_rds("fcasts.rds")
fcasts <- fcasts %>%
  filter(year==max(year), 
         outcome!="attempt") %>%
  select(-observed, -year) %>%
  pivot_wider(names_from = outcome, values_from = p) %>%
  mutate(Country = countrycode(gwcode, "gwn", "country.name"),
         Country = ifelse(gwcode==816, "Vietnam", Country)) %>%
  dplyr::rename(GWcode = gwcode, Year = for_year, Attempt = attempt2, Coup = coup, 
         Failed = failed)  %>% 
  select(Country, Year, GWcode, everything())

write_csv(fcasts, "../../forecasts/coup-2020.csv")


