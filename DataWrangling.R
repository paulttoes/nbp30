library(tidyverse)
library(ggthemes)
library(plotly)

nbpData <- read.csv("~/GitHub/npb30/raw_data/NBP database 2023-10-02.csv")

# Let's grab the total number of observations per year for later comparison
tmp1GraphsDf <- nbpData |>
  group_by(Year) |>
  summarise(originalObservations = n())
            

# Based on the explorations (see Exploration.r), we are going to clean the data in the following ways:
# 1. We are going to remove the years 1996 and 2000
# 2. We are going to remove the parks with only a few years of activity
nbpCleanData <-  nbpData |>
  filter(Year != "1996" & Year != "2020") |>
  filter(Park != "Walsh Property" & Park != "Soos Creek" & Park != "Shadow Lake Bog" 
         & Park != "Jenkin's Creek Park" & Park != "Clark Lake Park"
         & Park != "Cheasty Greenspace" & Park != "Bliner Property") 

# let's grab the number of observations after cleaning
tmp2GraphsDf <- nbpCleanData |>
  group_by(Year) |>
  summarise(cleanedObservations = n())

# let's join the original and the cleaned observations for direct comparison in a grap
tmp1GraphsDf <- right_join(tmp1GraphsDf, tmp2GraphsDf, by="Year")

# Let's verify we are left with the correct parks
# Parks included by years
ggplot(nbpCleanData, aes(x = Year, y = Park)) +
  geom_point(aes(color = Park)) +
  labs(
    title = "Active Parks by Year for Clean Data Set"
  ) +
  theme(legend.position="none")


# Let's verify we are left with the correct years 
  ggplot(nbpCleanData, aes(x=Year)) + 
  geom_bar() +
  labs(
    title = "Total Number of Observations",
    subtitle = "Excluding low count years 1996 and 2020",
    x = "Year", y = "Number of Observations"
    ) 

# What percentage of observations is left
  # let's make a percentage of dog obaas part of the total bird count
  tmp1GraphsDf <- tmp1GraphsDf |>
    mutate(obsPercentage = round(100 * cleanedObservations/originalObservations))
  
# let make a table of all obersations and the percentages
  plot_ly(
    type = "table",
    header = list(values = c("Year", "Original Observation", "Cleaned Observations", "Percentage of Observation")),
    cells = list(values = rbind(tmp1GraphsDf$Year, tmp1GraphsDf$originalObservations, 
                                tmp1GraphsDf$cleanedObservations, tmp1GraphsDf$obsPercentage))
  )
  