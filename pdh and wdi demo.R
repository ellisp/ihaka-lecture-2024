library(rsdmx)
library(tidyverse)
library(WDI)
library(janitor)
library(ggrepel)

# Poverty (national definition) rates for Pacific Island countries 
# from Pacific Data Hub:
poverty_df <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_SDG_01,3.0/A.SI_POV_NAHC.._T._T._T.....?startPeriod=2016&endPeriod=2023&dimensionAtObservation=AllDimensions")  |>
  as_tibble() |>
  clean_names() |>
  # filter to just the latest for each country (different years)
  group_by(geo_pict) |>
  arrange(desc(time_period)) |>
  slice(1) |>
  rename(poverty = obs_value)

# Statistical Performance Indicators from the World Bank, selected countries
spi_df <- WDI(
  indicator = "IQ.SPI.OVRL",
  country = c("FJ", "KI", "MH", "FM", "PG", "WS", "SB", "VU"))

# Create combined dataset, latest value of SPI and of poverty
combined_df <- spi_df  |>
  filter(!is.na(IQ.SPI.OVRL)) |>
  group_by(iso2c) |>
  arrange(desc(year)) |>
  slice(1) |>
  left_join(poverty_df, by = c("iso2c" = "geo_pict")) 

# Produce scatter plot
combined_df |>
  ggplot(aes(x = IQ.SPI.OVRL, y = poverty)) +
  geom_smooth(method = "lm", colour = "white") +
  geom_point(size = 2) +
  geom_text_repel(aes(label = country), colour = "brown") +
  labs(x = "Statistical Performance Indicator (World Bank)",
       y = "Population below national poverty line",
       caption = "Source: Pacific Data Hub & World Development Indicators",
       title = "Statistical performance and national poverty",
       subtitle = "No statistically significant relationship")

