library(tidyverse)
library(ggthemes)
library(plotly)

nbpData <- read.csv("~/GitHub/npb30/raw_data/NBP database 2023-10-02.csv")

#View(nbpData)

# Add a survey count column based on the Extra.volunteers column plus the number of surveyors separated by a ; plus one extra for the first surveyor
# Because there are diminishing returns as your add more surveyors, I also created a "bin" field where I cap the max at 5 surveyors
# I created ratios of both to weight the rows on counts
# if you want to check the number of surveyor, you can sort the row using |> arrange(desc(surveyorCnt))
nbpDataMutated <- nbpData |>
  mutate(location = paste(Loop, Station),
         .after = Station) |>
  mutate(birdCnt = rowSums(pick(Seen:Fly), na.rm = T),
         .after = "Fly")  |>
  mutate(moreThanOneSurveyor = str_count(surveyors, ";") != 0) |>
  mutate(surveyorCnt = as.integer(Extra.volunteers == "Y") + as.integer(1) + str_count(surveyors, ";")) |>
  mutate(moreThanOneSurveyor = surveyorCnt != 1) |>
  mutate(surveyorRatio = 1 / surveyorCnt) |> 
  mutate(surveyorBins = case_when(
    surveyorCnt > 4 ~ 5,
    .default = as.integer(surveyorCnt))) |>
  mutate(surveyorBinRatio = 1 / surveyorBins) |> 
  arrange(desc(surveyorCnt))

# View(nbpDataMutated)\\

  

# It looks like surveyor data entry is unreliable - based on my own experience where extra surveyors are not recognized
# but maybe we can assume that is multiple surveyors are entered, than this is reliable surveyor data entry
# the plot however suggest that this undercounting of surveyors was particularly a problem early on and
# improved much later
# as a line plot
ggplot(nbpDataMutated, aes(x = Year, color = moreThanOneSurveyor)) + 
  geom_density(linewidth = 0.75) +
  labs(
    title = "Number of Annual Observations and Surveyor Count",
    subtitle = "Is the data entered with more than one surveyor listed in the surveyor column",
    x = "year", y = "count",
  )

# the same data as the previous plot but as a fill bar
ggplot(nbpDataMutated, aes(x = Year, color = moreThanOneSurveyor)) + 
  geom_bar(position = "fill") +
  labs(
    title = "Ratio of Annual Observations with more than one surveyor",
    subtitle = "Is the data entered with more than one surveyor listed in the surveyor column",
    x = "year", y = "relative frequency",
  ) 



# begin snippet this makes new columns for each bird species 
#let's make a df with annual  data
nbpAnnualDf <- nbpDataMutated |>
  group_by(Year) |>
  summarise(observations = n(),
            allBirds = sum(birdCnt)) 

# Total number of birds counted by year
ggplot(nbpAnnualDf, aes(x=Year)) + 
  geom_bar(aes(weight = allBirds))
labs(
  title = "Total Number of Birds",
  x = "year", y = "count",
) +
  scale_color_colorblind()  

# Total numbers of bald eagles by year
tmpDf = nbpDataMutated |>
  filter(Species == "Bald Eagle") |>
  group_by(Year) |>
  summarise(baldEagleTotal = sum(birdCnt))

# let's join the two data frames
nbpAnnualDf <- left_join(nbpAnnualDf, tmpDf, by="Year")

# let's make a percentage of bald eagles as part of the total bird count
nbpAnnualDf <- nbpAnnualDf |>
  mutate(baldEagleRatio = 100 * baldEagleTotal/allBirds)
  

# Total number of bald eagles counted by year
ggplot(nbpAnnualDf, aes(x=Year)) + 
  geom_bar(aes(weight = baldEagleTotal)) +
  labs(
   title = "Total Number of Bald Eagles",
   x = "year", y = "count",
  ) +
  scale_color_colorblind()  

# Ratio of bald eagles counted by year as part of total population
ggplot(nbpAnnualDf, aes(x=Year)) + 
  geom_bar(aes(weight = baldEagleRatio)) +
  labs(
    title = "Total Percentage of Bald Eagles",
    subtitle = "What percentage of all birds observed are bald eagles",
    x = "year", y = "count",
  ) +
  scale_color_colorblind()  



# let's look at the interfering factors -- dogs, off leash dogs, and walkers observed
# how sparse is this data?
tmpDf <- nbpDataMutated |>
  drop_na(Dogs) |>
  group_by(Year) |>
  summarise(allDogsObs = n())

# let's join the two data frames
nbpAnnualDf <- left_join(nbpAnnualDf, tmpDf, by="Year")

tmpDf <- nbpDataMutated |>
  drop_na(Dogs.off.leash) |>
  group_by(Year) |>
  summarise(offLeashDogsObs = n())

# let's join the two data frames
nbpAnnualDf <- left_join(nbpAnnualDf, tmpDf, by="Year")

tmpDf <- nbpDataMutated |>
  drop_na(Walkers) |>
  group_by(Year) |>
  summarise(walkersObs = n())

# let's join the two data frames
nbpAnnualDf <- left_join(nbpAnnualDf, tmpDf, by="Year")

# replace null values caused by the left join
nbpAnnualDf <- nbpAnnualDf %>% replace(is.na(.), 0)


# let's make a percentage of dog obaas part of the total bird count
nbpAnnualDf <- nbpAnnualDf |>
  mutate(obsPercentage = round(100 * allDogsObs/observations))


plot_ly(
  type = "table",
  header = list(values = c("Year", "dogs Obs", "offleash dogs Obs", "walkers Obs", "Percentage of Observation")),
  cells = list(values = rbind(nbpAnnualDf$Year, nbpAnnualDf$allDogsObs, nbpAnnualDf$offLeashDogsObs, nbpAnnualDf$walkersObs, 
                              nbpAnnualDf$obsPercentage))
)


# Parks analysis by year, observation, locations
nbpParksDf <- nbpDataMutated  |>
  group_by(Park, Year) |>
  summarise(surveyDateCnt = n_distinct(Survey.Date),
            locationCnt = n_distinct(location),
            speciesCnt = n_distinct(Species),
            observations = n(),
            allBirds = sum(birdCnt),
            avgBirdsPerLocation = allBirds / locationCnt) 

# Parks included by years
ggplot(nbpParksDf, aes(x = Year, y = Park)) +
  geom_point(aes(color = Park)) +
  labs(
    title = "Active Years by Park"
  ) +
  theme(legend.position="none")


# how many observations per park by year
ggplot(nbpParksDf, aes(x = Year, y = allBirds)) +
  geom_point(aes(color = Park)) +
  labs(
    title = "Bird Numbers by Years and Park",
    y = "number of birds"
  ) 

ggplot(nbpParksDf, aes(x = Year, y = locationCnt)) +
  geom_point(aes(color = Park)) +
  labs(
   title = "Number of Locations by Park and Year",
   y = "Distinct Locations Monitored"
   
) 

# yup, there are really 100 distinct locations
tmpDf = nbpDataMutated |>
  filter(Park == "Discovery Park") |>
  filter(Year == "2015") |>
  distinct(location) |>
  arrange(desc(location))

# average bird numbers per park
parksOverallDf <- nbpDataMutated  |>
  group_by(Park) |>
  summarise(surveyDateCnt = n_distinct(Survey.Date),
            locationCnt = n_distinct(location),
            observations = n(),
            allBirds = sum(birdCnt),
            nMonths = n_distinct(Month),
            nYears = n_distinct(Year),
            avgBirdsPerPark = allBirds / nMonths / nYears / locationCnt,
            ) 

# sort park by average number of birds
ggplot(parksOverallDf, aes(y = avgBirdsPerPark, x = reorder(Park, avgBirdsPerPark))) +
  geom_col() + 
  coord_flip() +
  geom_text(aes(label = round(avgBirdsPerPark)), 
            hjust = -.2) +
  labs(
    title = "Average Number of Birds by Park",
    y = "Number of Birds Counted",
    x = "Park"
) 




# how many bird species per park by year
ggplot(nbpParksDf, aes(x = Year, y = speciesCnt)) +
  geom_point(aes(color = Park)) +
  labs(
    title = "Number of Bird Species by Years and Park",
    y = "number of species"
  ) 

# sort park by average number of bird species over time
ggplot(parksOverallDf, aes(y = speciesCnt, x = reorder(Park, speciesCnt))) +
  geom_col() + 
  coord_flip() +
  geom_text(aes(label = speciesCnt), 
            hjust = -.2) +
  labs(
    title = "Total Number of Bird Species Observed by Park",
    y = "Number of Birds Species",
    x = "Park"
) 

# average number bird species per park location, year and month
tmpDf <- nbpDataMutated  |>
  group_by(Park, Year, Month, location) |>
  summarise(speciesCnt = n_distinct(Species)) 

# now what is the average number of bird species for a given location
tmpDf <- tmpDf |>
  group_by(Park) |>
  summarize(avgSpecies = mean(speciesCnt))

# now plot the average number of bird species for a given location
ggplot(tmpDf, aes(y = avgSpecies, x = reorder(Park, avgSpecies))) +
  geom_col() + 
  coord_flip() +
  geom_text(aes(label = round(avgSpecies)), 
            hjust = -.2) +
  labs(
    title = "Number of Bird Species Observed in an Average Location",
    y = "Number of Birds Species",
    x = "Park"
  ) 

