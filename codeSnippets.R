# Total number of birds counted by year
ggplot(nbpDataMutated, aes(x = Year)) + 
  geom_bar(aes(weight = birdCnt)) +
  labs(   
    title = "Total Number of Birds",
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
    subtitle = "These are based on the total number of birds",
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




#let's make a df with annual  data for all birds. 
# bind rows example
nbpAnnualDf <- nbpDataMutated |>
  group_by(Year) |>
  summarise(type = "All",
            total = sum(birdCnt)) 

# let's make another df for bald eages
baldEagleDf <-  nbpDataMutated |>
  filter(Species == "Bald Eagle") |>
  group_by(Year) |>
  summarise(type = "Bald Eagle", total = sum(birdCnt))

# let's combine the dfs
nbpAnnualDf <- bind_rows(nbpAnnualDf, baldEagleDf)
  

ggplot(nbpAnnualDf, aes(x = Year, y=total)) +
  geom_point()
# end bind rows example
