# April 29, 2020 "Tidy Wednesday"
# Data from Tidy Tuesday release: April 28, 2020
# Data on Broadway Musical box office $, ticket sales, shows, and Consumer Price Index.
# Question of interest: What is the most successful Broadway show of all time?
# Data from 1985 to 2020.

# Get the Data -------------------------------

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

# glimpse() is Similar to str(), but shows as much data as possible.
glimpse(cpi)

# Understand the Data
'
1. What is the CPI?

The Consumer Price Index (CPI) is a measure of the average change overtime in the prices paid by urban consumers 
for a market basket of consumer goods and services.
This market basket includes goods from eight major groups + sales & excise taxes:
Food and beverages, housing, apparel, transportation, medical care, recreation, education and communication, 
and other goods and services.
There are many CPI types (by region, etc). The one used here is:
“All items less food and energy in U.S. city average, all urban consumers, seasonally adjusted”

2. Is CPI the best measure of inflation?
The CPI is generally the best measure for adjusting payments to consumers when the intent is to allow consumers 
to purchase at todays prices, a market basket of goods and services equivalent to one that they could purchase in 
an earlier period.

Note: CPI is not good to represent customers from rural, elderly or poor backgrounds.
We presume Broadway patrons are of urban and wealthier backgrounds so using CPI is OK for inflation-adjustment
in this particular analysis.


3. How is the base CPI value set? I notice these values start above 100.
Most CPI index series have a 1982-84=100 reference base.
Our data is from 1985-2020.
'

# Load packages -----------------
install.packages("scales")
install.packages("fishualize")
library(fishualize)

library(tidyverse)
library(lubridate)
library(rvest)
library(scales)
library(extrafont)

# Data Cleanup - The Original, Dirty Data

grosses_raw <- readr::read_csv("https://raw.githubusercontent.com/tacookson/data/master/broadway-grosses/grosses.csv",
                               guess_max = 10000)
cpi_raw <- readr::read_csv("https://raw.githubusercontent.com/tacookson/data/master/broadway-grosses/cpi.csv")
glimpse(cpi_raw)

# Data Cleanup 1 ---------------

# There are 125 data rows with 0 weekly_gross sales values. What happened, Young Frankenstein?
view(print(grosses_raw[grosses_raw$weekly_gross == 0, ], n = 125))

# Replace any 0 sales values with NA to avoid skewing down average sales calculations.
grosses_fixed_missing <- grosses_raw %>%
  # Turn metrics into missing values if there were no shows
  # OR if metrics have a value of zero
  mutate_at(vars(weekly_gross:pct_capacity),
            ~ ifelse(performances + previews == 0 | . == 0, NA, .))

grosses_fixed_missing[grosses_fixed_missing$weekly_gross ==0, ] # The 0's have transformed into NAs! Goot!

# Data Cleanup 2 ---------------
# Need to account for show revivals, new runs. If there's a gap/break >90 days, consider it a fresh new run.
# Example: Les Mis had two revival runs.
grosses_fixed_missing %>%
  filter(show == "Les Miserables") %>%
  ggplot(aes(week_ending, seats_sold)) +
  geom_col() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Two-four-six-oh-what??",
       subtitle = "Seats sold by week for Les Misérables")

# Group shows by runs, and count week number for each show-run, per year.
grosses_clean_temp <- grosses_fixed_missing %>%
  group_by(show) %>%
  arrange(week_ending) %>%
  mutate(run_number = cumsum(row_number() == 1 |
                               week_ending - lag(week_ending) > 90)) %>%
  group_by(show, run_number) %>%
  mutate(week_of_run = row_number()) %>%
  ungroup()
# The longest runs are 53 weeks long, because some 'years' have 53 weeks.

# Data Cleanup 3--------
# 19  shows were already running when this dataset begins in 1985. They weren't on their first week. Manually add these in.
pre_1985_starts <- readr::read_csv("https://raw.githubusercontent.com/tacookson/data/master/broadway-grosses/pre-1985-starts.csv")

calculate_weeks_since_start <- function(x) {
  as.integer(pmax(1, difftime("1985-06-09", x, units = "weeks")))
}

pre_1985_starts_calculated <- grosses_clean_temp %>%
  group_by(show, run_number) %>%
  filter(min(week_ending) == "1985-06-09") %>%
  ungroup() %>%
  select(week_ending, show) %>%
  left_join(pre_1985_starts, by = "show") %>%
  group_by(show) %>%
  mutate(week_of_run_originals = calculate_weeks_since_start(start_date) + row_number()) %>%
  ungroup() %>%
  select(week_ending, show, week_of_run_originals)

grosses_clean <- grosses_clean_temp %>%
  left_join(pre_1985_starts_calculated, by = c("show", "week_ending")) %>%
  mutate(week_of_run = coalesce(week_of_run_originals, week_of_run)) %>%
  select(-week_of_run_originals)

# Data Cleanup 4
# Adjust for inflation from 1985 to 2020. Convert Nominal dollars into Real dollars, using CPI.
# All dollars will be converted to Jan 2020 value.
# Do this by dividing all years' CPIs by the Jan 2020 CPI.
# So, if we want, say, Nov 1988 dollars in their Jan 2020 equivalent, 
# we just use our newly-calculated multiplier. ($100 in Nov 1988 is equivalent to about $212 in Jan 2020).

cpi <- cpi_raw %>%
  mutate(jan_2020_dollars = cpi[year_month == "2020-01-01"] / cpi)

# CPI data is monthly. Box office figures are weekly. 
# Now we need to get our weekly grosses data to be monthly. 
# We can use the {lubridate} package’s floor_date() function, which can round dates down to the first of the month.
# Then, we join the cpi data based on month and multiply all the dollar figures by our Jan 2020 mulitiplier.
real_grosses <- grosses_clean %>%
  mutate(year_month = floor_date(week_ending, unit = "month")) %>%
  left_join(cpi, by = "year_month") %>%
  mutate_at(
    vars(
      weekly_gross_overall,
      weekly_gross,
      potential_gross,
      avg_ticket_price,
      top_ticket_price
    ),
    ~ . * jan_2020_dollars
  ) %>%
  select(-year_month:-jan_2020_dollars)

# Data cleanup, complete!

# Analysis Time: Which shows made the most money? ---------------

cumulative_grosses_by_year <- real_grosses %>%
  mutate(year_of_run = week_of_run / 52) %>%
  group_by(show, run_number) %>%
  mutate(
    # Use coalesce() for shows that have some NAs
    weekly_gross = coalesce(weekly_gross, 0),
    cumulative_gross = cumsum(weekly_gross),
    show_label = ifelse(year_of_run == max(year_of_run), paste0(" ", show), NA_character_)
  ) %>%
  ungroup() %>%
  mutate(show_and_run = paste(show, run_number),
         show_and_run = fct_reorder(show_and_run, year_of_run, .fun = max))

# Create character vector of top 10 grossing shows
# dplyr::top_n(). What a useful function!
top_10_grossing <- cumulative_grosses_by_year %>%
  group_by(show_and_run) %>%
  summarise(show_total_gross = sum(weekly_gross)) %>%
  top_n(10, wt = show_total_gross) %>%
  pull(show_and_run)

# Graph top 10 shows: ------------------------
cumulative_grosses_by_year %>%
  filter(show_and_run %in% top_10_grossing) %>%
  ggplot(aes(year_of_run, cumulative_gross, col = show_and_run)) +
  geom_line(size = 1) +
  geom_label(
    aes(label = show_label),
    size = 3,
    family = "Bahnschrift",
    fontface = "bold",
    hjust = 0,
    vjust = 0,
    label.size = NA,
    label.padding = unit(0.01, "lines"),
    label.r = unit(0.5, "lines")
  ) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_y_continuous(labels = label_dollar(scale = 1 / 1e6, suffix = "M")) +
  scale_colour_fish(option = "Epibulus_insidiator",
                    discrete = TRUE,
                    direction = 1) +
  expand_limits(x = 40) +
  labs(
    title = "Could Hamilton overtake The Lion King in...20 years?",
    subtitle = paste0("Cumulative box office receipts (Jan. 2020 dollars) for top 10 grossing shows since 1985"),
    caption = paste0("Source: Playbill\n",
                     "Note: Partial data for Cats, which started in 1982 (this data begins in 1985)"),
    x = "Year of run",
    y = ""
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Bahnschrift"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 9)
  )

# Continue: Play with Fishualize package, read intro on https://github.com/nschiett/fishualize
