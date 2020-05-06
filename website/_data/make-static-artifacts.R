#
#   Creates World.rds, separation plots
#

library(leaflet)
library(leaflet.providers)
library(sf)
library(readr)
library(dplyr)
library(countrycode)
library(here)
library(tmap)
library(stringr)
library(ggplot2)

setwd(here::here("website/_data"))


# World map data ----------------------------------------------------------

data("World")
World <- World %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

fcasts <- read_rds("fcasts.rds")
fcasts <- fcasts %>% dplyr::filter(year==max(year))
fcasts$iso_a3 <- countrycode(fcasts[["gwcode"]], "gwn", "iso3c")
fcasts <- fcasts %>% 
  mutate(p = 100*p) %>%
  tidyr::pivot_wider(names_from = "outcome", values_from = "p") %>%
  dplyr::select(iso_a3, attempt, coup, failed)

World <- World %>%
  left_join(fcasts) %>%
  select(iso_a3, name, attempt, coup, failed) 

# Clean up country names
World <- World %>%
  mutate(name = as.character(name),
         name = str_remove(name, " \\([a-zA-Z]+\\)"),
         name = case_when(
           name=="Macedonia" ~ "North Macedonia",
           name=="German Federal Republic" ~ "Germany",
           TRUE ~ name
         ))

saveRDS(World, "World.rds")



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
  mutate(index = 1:n()) %>%
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
  rename(GWcode = gwcode, Year = for_year, Attempt = attempt2, Coup = coup, 
         Failed = failed)  %>% 
  select(Country, Year, GWcode, everything())

write_csv(fcasts, "../../forecasts/coup-2020.csv")


