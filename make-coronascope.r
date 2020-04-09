library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidyr)
library(purrr)
library(anytime)
library(trelliscopejs)

theme_set(theme_bw())

steepest_date <- function(x, var) {
  curve <- diff(x[[var]])
  as.character(x$Date[which.max(c(0, curve))])
}

make_plot <- function(x) {
  g <- x %>% 
    select(-fips) %>%
    pivot_longer(-c(Date, County), names_to = "Type", values_to = "Value") %>%
    ggplot(aes(x = Date, y = Value, group = County, color = County)) + 
      scale_y_log10() +
      geom_line() +
      geom_point() +
      theme(legend.position = "none") +
      facet_grid(Type ~ .)
  ggplotly(g)
}

read_csv(paste("https://raw.githubusercontent.com/nytimes",
               "covid-19-data/master/us-counties.csv", sep = "/")) %>%
  write_csv("us-counties.csv") %>%
  rename(Date = date, County = county, State = state, Cases = cases,
         Deaths = deaths) %>%
  mutate(Date = anydate(Date)) %>%
  nest(data = -c(State)) %>%
  mutate(total_deaths = map_dbl(data, ~ sum((.x)$Deaths+1, na.rm = TRUE)),
         total_cases = map_dbl(data, ~ sum((.x)$Cases, na.rm = TRUE)),
         steepest_death_curve = 
           map_dbl(data, ~ max(c(0, diff(.x$Deaths), na.rm = TRUE))),
         steepest_case_curve = 
           map_dbl(data, ~ max(c(0, diff(.x$Cases), na.rm = TRUE))),
         steepest_death_curve_date = 
           anydate(map_chr(data, steepest_date, "Deaths")),
         steepest_case_curve_date = 
           anydate(map_chr(data, steepest_date, "Cases")),
         plots = map(data, make_plot)) %>%
  trelliscope(name = "Coronascope", panel_col = "plots", path = "docs")
