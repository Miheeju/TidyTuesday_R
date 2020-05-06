# 2020_05_05 Animal Crossing
# Functions I learned:
# filter()
# count(), group_by(), tally()
# add_count(), add_tally()
# dplyr: summarize_at()
# arrange(desc(n))
# Using tidyverse formatting to filter data, not base R [,]


# Tidy Tuesday R Data Wrangling

# The data this week comes from [VillagerDB and MetaCritic]. (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-05/readme.md). 
# This article talks about the subject in greater detail.  (https://www.polygon.com/2020/4/2/21201065/animal-crossing-new-horizons-calm-mindfulness-coronavirus-quarantine)
# Credit: Heeju Suh (https://github.com/Miheeju/TidyTuesday_R)

# Note: I have not played this nintendo video game. However, it's booming in popularity. Even a Flatiron HTML/CSS Website Building intro course made use of putting "Isabelle", a dog character from the game, onto our websites!
# It's intriguing to find out why this game has grown so popular in the midst of quarantimes.

# Load Packages ----------------------------------------------------
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2, tidyverse)

# Get the Data ----------------------------------------------------
# .tsv file = Tab Separated Values

# critic: Critic Reviews, score of 1-100 with source and review text
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
# User Reviews, score of 1-10. Many complaints of needing 1 switch console per customizable island, bad for families.
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
# Items in the game, with category and sell/buy value. Currency is called "bells".
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
# Director of chacters on an island, with their animal type, gender, song, phrase, and ID.
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# Analysis Time!  ----------------------------------------------------
# Note: Some items' buy_currency is in 'miles'. Others are 'bells' or NA.
# sell_currency is either bells or NA.

# Analysis 1: Which items can be bought with 'miles' and then sold for bells?
# Answer: 19 "Nook Inc" items from the Nook Miles System. Not the e-reader Nook, from the Tanuki character Tom Nook.

items_miles <- items %>% filter(buy_currency == "miles" & sell_currency == "bells")
view(items_miles)

# Analysis 2: Which 'items' give the highest profit (sell minus buy value)?
# Answer: None. All items have a sell_value < buy_value.
# Keep items where sell and buy currency are both in bells.
# dpylr::filter() will choose rows/cases where conditions are true. Unlike base subsetting with [, rows where the condition evaluates to NA are dropped.
# 3515 items are bought and sold in 'bells'.

items_bells <- items %>% filter(buy_currency == "bells" & sell_currency == "bells")

items_bells %>% filter(sell_value > buy_value)


# Analysis 3: Which items have a greatest difference between buy and sell value?
# Answer: Royal Crown, Crown, Gold Armor, Golden Casket (?!?), Grand Piano

items_bells %>% filter(sell_value > buy_value)
items_bells %>% top_n(.$buy_value, n = 5) # Most expensive items, but results are not in order...
items_bells %>% top_n(.$buy_value, n = -5) # Cheapest items

# Create value difference column and add to items_bells table
value_dif <- items_bells$buy_value - items_bells$sell_value
items_bells$value_dif <- value_dif
class(value_dif)

items_bells %>% 
  top_n(wt = value_dif, n = 5) # Done!! Expand Console width to view last col. The trick was to create the value_dif in a new column and add it to the table.


top_n(items_bells %>% filter(buy_value > sell_value), n = 5)

# Analysis 4: Which category of items is the most expensive?
# Furniture, Hats, Bugs
# Cheapest: Flowers, Fruit, Photos, Socks, Tools
items_bells %>%
  group_by(category) %>%
  dplyr::summarize_at(vars(buy_value, sell_value), funs(mean(., na.rm = TRUE)))

# Villagers (Characters) Analysis ----------------------------------------------

# Analysis 5: Are there more male or female characters?
# Answer: 187 Females, 204 males! Slightly more males!
# count() does  group_by() + tally() https://dplyr.tidyverse.org/reference/tally.html
villagers %>%
  count(gender)

# Add gender count to villagers table using add_count()
villagers %>%
  add_count(gender)

# Analysis 6: Which Personality Types are the most common?
# Lazy, Normal, Canky/Jock/Snooty, Peppy
villagers %>%
  count(personality) %>%
  arrange(desc(n))

# Analysis 6b:Are there any personality types with only 1 character?
# Nope! But the least common type is 'uchi'.

# Analysis 7: Are there any villagers who share a song?
# Yes: Some have no song, while many others like KK Country, Forest Life, and I Love You are shared.
villagers %>%
  count(song) %>%
  arrange(desc(n))

# Analysis 8: Who has their own song?
# Four special villagers have their own song: Angus, Diva, Gwen, and Zell.
villagers %>%
  add_count(song) %>%
  filter(n == 1)

# Analysis 9: Who shares a birthday with Yoon(2/3), Heeju(3/13) and Heeju's mom (5/6?
# Specify the column name for each logical OR expression

# Olivia the cat & Yoon
# Megan the bear & Heeju
# Tank the rhino & mom

villagers %>%
  filter(birthday == "2-3" | birthday == "3-13" | birthday == "5-6")
