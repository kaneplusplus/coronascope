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

diff_smoothed_last <- function(x, pop, column_name, num_diffs = 1) {
  totals <- x %>% 
    group_by(Date) %>%
    summarize(state_total = sum({{column_name}})) %>%
    `$`(state_total)
  
  totals <- totals / pop * 10^5
  
  for (i in seq_len(num_diffs)) {
    totals <- diff(totals)
  }
  signif(tail(smooth(totals), 1), digits = 3) 
}

pop <- read_csv("pop-est-2019.csv") %>% 
  select(-County) %>%
  group_by(State) %>%
  summarize(Population = sum(Population))

read_csv(paste("https://raw.githubusercontent.com/nytimes",
               "covid-19-data/master/us-counties.csv", sep = "/")) %>%
  rename(Date = date, County = county, State = state, Cases = cases,
         Deaths = deaths) %>%
  left_join(pop, by = "State") %>%
  write_csv("us-counties.csv") %>%
  mutate(Date = anydate(Date)) %>%
  nest(data = -c(State, Population)) %>%
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
         state_case_1d = 
           map2_dbl(data, Population, diff_smoothed_last, Cases, 1),
         state_case_2d = 
           map2_dbl(data, Population, diff_smoothed_last, Cases, 2),
         state_death_1d = 
           map2_dbl(data, Population, diff_smoothed_last, Deaths, 1),
         state_death_2d = 
           map2_dbl(data, Population, diff_smoothed_last, Deaths, 2),
         plots = map(data, make_plot)) %>%
  trelliscope(name = "Coronascope", panel_col = "plots", path = "docs")
