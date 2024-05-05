library(tidyverse)
library(ggthemes)

nbpData <- read.csv("raw_data/NBP database 2023-10-02.csv")
View(nbpData)

nbpData |>
  count(Year)

ggplot(nbpData, aes(x = Year)) +
  geom_bar() +
  labs(
  title = "Total Number of Observations",
  x = "year", y = "count",
) +
  scale_color_colorblind()


# Add a survey count column based on the Extra.volunteers column plus the number of surveyors separated by a ; plus one extra for the first surveyor
# Because there are diminishing returns as your add more surveyors, I also created a "bin" field where I cap the max at 5 surveyors
# I created ratios of both to weight the rows on counts
# if you want to check the number of surveyor, you can sort the row using |> arrange(desc(surveyorCnt))
nbpDataMutated <- nbpData |>
  mutate(surveyorCnt = as.integer(Extra.volunteers == "Y") + as.integer(1) + str_count(surveyors, ";")) |>
  mutate(surveyorRatio = 1 / surveyorCnt) |> 
  mutate(surveyorBins = case_when(
    surveyorCnt > 4 ~ 5,
    .default = as.integer(surveyorCnt))) |>
  mutate(surveyorBinRatio = 1 / surveyorBins) 
# |> arrange(desc(surveyorCnt))

  

View(nbpDataMutated)

baldEagleCntAnnual <- nbpData |>
  filter(Species == "Bald Eagle") |>
  count(Year)

ggplot(
  data = baldEagleCntAnnual,
  mapping = aes(x = Year, y = n)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Annual Bald Eagle Count",
    subtitle = "These are based on the total number of observations",
    x = "year", y = "count",
  ) +
  scale_color_colorblind()



baldEagleCntAnnualPerSurveyor <- nbpDataMutated |>
  filter(Species == "Bald Eagle") |>
  count(Year, wt=surveyorRatio) 
  

ggplot(
  data = baldEagleCntAnnualPerSurveyor,
  mapping = aes(x = Year, y = n)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Annual Bald Eagle Count",
    subtitle = "These are based on the total number of observations",
    x = "year", y = "count",
  ) +
  scale_color_colorblind()

