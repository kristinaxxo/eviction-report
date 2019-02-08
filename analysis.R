# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)
dim(evictions)
# Compute some values of interest and store them in variables for the report

# How many evictions were there?
num_evictions <- nrow(evictions)
num_features <- ncol(num_evictions)

# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- evictions %>%
  group_by(Eviction.Notice.Source.Zipcode) %>%
  count() %>% #shortcut to count the rows(should be similar to nrow())
  arrange(-n) %>%
  ungroup() %>% #have to ungroup first to get the first top ten 
  top_n(10, wt = n)


# Create a plot of the number of evictions each month in the dataset
by_month <- evictions %>% 
  mutate(date = as.Date(File.Date, format = "%m/%d%/%y")) %>% 
  mutate(month = floor_date(date, unit = "month")) %>% 
  group_by(month) %>% 
  count() 

# Store plot in a variable
month_plot <- ggplot(data = by_month) + #store plot in a variable by giving it a name 
  geom_line(mapping = aes(x = month,y = n)) +
  labs(x = "Date", y = "Number of Evictions", title = "Evictions over time in SF")

# Map evictions in 2017 

# Format the lat/long variables, filter to 2017

# Create a maptile background

# Add a layer of points on top of the map tiles
