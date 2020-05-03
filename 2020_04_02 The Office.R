# April 21, 2020 First Tidy Tuesdayy
# Using data from Tidy Tuesday release on March 17, 2020.
# Data Theme: "The Office" TV Show IMDb ratings. Ratings are on a 1-10 point scale.
# Article Inspiration: https://pudding.cool/2017/08/the-office/

# Load Packages -----------
install.packages("tidyverse")

# Load Data -----------
# 188 episodes, from 3/24/2005 thru 5/16/2013, about 8 years
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

head(office_ratings)
str(office_ratings)
summary(office_ratings)
# Tidying: Turn Season into a factor
office_ratings$season <- factor(office_ratings$season)

# Plot Base Layer
g <- ggplot(data = office_ratings)

# Analysis 1: How have Ratings changed over time?
# Average Rating per Year. Use 'Season', rather than air_date year.

g + geom_point(aes(x = season, y = imdb_rating))
g + geom_boxplot(aes(group = season, x = season, y = imdb_rating))
# Insight: It looks like The Office had its peak around season 3 & 4 (median rating above 8.5), then declined slightly since (7.75-8.6)

# How to get average rating by season?
avg <- aggregate(office_ratings$imdb_rating~office_ratings$season, FUN=mean) #Yessss!
colnames(avg) <- c("season", "imdb_ratingavg")

# Plot average rating by season, no distribution
gavg <- ggplot(data = avg, aes(x = season, y = imdb_ratingavg))
gavg + geom_point() + geom_curve(aes(xend = 4.5, yend = 5)) # A hair massager
