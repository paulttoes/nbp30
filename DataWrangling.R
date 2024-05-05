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




# Add a survey count column based on the Extra.volunteers column plus the number of surveyors separated by a ; plus 
# one extra for the first surveyor
# if you want to check the number of surveyor, you can sort the row using |> arrange(desc(surveyorCnt))
nbpDataMutated <- nbpData |>
  mutate(surveyorCnt = as.integer(Extra.volunteers == "Y") + as.integer(1) + str_count(surveyors, ";")) |>
  mutate(surveyorRatio = 1 / surveyorCnt) |> 
  arrange(desc(surveyorCnt))


View(nbpDataMutated)




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

