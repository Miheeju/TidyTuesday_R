# 2020_05_12 Volcano Eruptions
# Functions I learned:
# semi_join() & inner_join() & anti_join() https://dplyr.tidyverse.org/reference/join.html

# Tidy Tuesday R Data Wrangling

# The data this week comes from [The Smithsonian Institution]. (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-12/readme.md). 
# This article talks about the subject in greater detail.  (https://www.axios.com/chart-every-volcano-that-erupted-since-krakatoa-467da621-41ba-4efc-99c6-34ff3cb27709.html) & (https://en.wikipedia.org/wiki/Volcano)
# Credit: Heeju Suh (https://github.com/Miheeju/TidyTuesday_R)

# About the data:
# From Wikipedia: Volcanic ash and sulfuric gases form aerosols. "The aerosols increase the Earth's albedo—its reflection of radiation from the Sun back into space—and thus cool the Earth's lower atmosphere or troposphere; however, they also absorb heat radiated up from the Earth, thereby warming the stratosphere. Several eruptions during the past century have caused a decline in the average temperature at the Earth's surface of up to half a degree (Fahrenheit scale) for periods of one to three years"
# Get the Data

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

# Either ISO-8601 date or year/week works!

summary(volcano) #Volcano numbers range from No. 210010 to 390829

# Analysis 1: How many eruptions have there been, in total?
# From year -11345 (BC) to 2020, there have been 11,178 eruptions recorded. Note, some are uncertain/unconfirmed.

summary(eruptions)

eruptions %>%
  count(.)

# Analysis 2: How many eruptions have been recorded since 2000?
# Answer: 794

eruptions %>%
  filter(start_year > 2000) %>%
  count(.)

# Analysis 3: How many confirmed eruptions have been recorded since 2000?
# Answer: 715. So, 79 eruptions are unconfirmed
# Create eruptions2000 list to use as filter for next analysis
eruptions2000 <- eruptions %>%
  filter(start_year > 2000 & eruption_category == "Confirmed Eruption")

count(eruptions2000)

794-715

# Analysis 4: Which regions have had the most eruptions?
# Lets do this for all volcanoes first, over entire period, including unconfirmed cases.
# Answer: South America (117), Japan/Taiwan/Marianas (102), Indonesia (95).
volcano %>%
  group_by(region) %>%
  count(.) %>%
  arrange(desc(n))

# Analysis 5: Which regions have had the most eruptions?
# Answer: USA (99), Indonesia (95), Japan (92).
volcano %>%
  group_by(country) %>%
  count(.) %>%
  arrange(desc(n))

# Analysis 6: Which countries have had the most confirmed eruptions recently (since 2000)?
# Indonesia (30), Japan (15), USA (14)
# Top regions: Indonesia (30), Japan/Taiwan/Marianas (19), South America (18)

# 150 unique volcanoes have erupted since 2000, total of 715 eruptions
volcano2000 <- semi_join(volcano, eruptions2000, by = "volcano_number")

volcano2000 %>%
  group_by(country) %>%
  count(.) %>%
  arrange(desc(n))

volcano2000 %>%
  group_by(region) %>%
  count(.) %>%
  arrange(desc(n))

# 6a Bonus: Top Recent Erupters: Piton de la Fournaise, San Cristobal, and Klyuchevskoy.
eruptions2000 %>%
  semi_join(volcano, eruptions2000, by = "volcano_number") %>%
  group_by(volcano_name) %>%
  count(.) %>%
  arrange(desc(n))

# Volcano Chikurachki, top erupter #10? What an intriguing name.
volcano %>%
  filter(volcano_name == "Chikurachki")

# Analysis 7: Mystery of confirmed eruptions w/ volcano_number, but missing volcano information

# The following result shows 201 unique volcanoes have erupted since 2000, total of 715 eruptions.
# Which of these 201 volcanoes are not in the 150 list?

eruptions2000 %>%
  group_by(volcano_number)

# use anti_join() - return all rows from x where there are not matching values in y, keeping just columns from x

# The following volcanoes had eruptions, but are not on the "Volcano" data list.
eruptions2000 %>%
  group_by(volcano_number) %>%
  anti_join(., volcano2000, by = "volcano_number")

# There are many volcanoes with confirmed eruptions, but are not on the Volcano data list.
# This means that regional/country data for these eruptions is not available.
# Plausible reasons: It may be that these volcanoes are not part of any country.
# Or, the Volcano list is not updated (although, its unusual because they have volcano_numbers)
# However, lattitude and longitude coordinates are available for all eruptions.
# Checking on volcano_number 343100, volcano_name "San Miguel", this is in El Salvador and last erupted in 2015 per wikipedia.
# Verdict: It seems to me that the Volcano List data is not complete/updated, despite recent eruptions bearing volcano_number data.

volcano %>%
  filter(volcano_number == "343100")

# Future Analyses I'm interested in:
# Which have the most eruptions with a population living with 100, 10, and 5km of the volcano?
# Are there more eruptions in Northern or Southern hemisphere?
