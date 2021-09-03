# Impact of European integration on regional life expectancy convergence
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version April 2021

require(dplyr)

regional_data <- read.csv(file = "dataset/regional_data.csv")
national_data <- read.csv(file = "dataset/national_data.csv")

#NMS category
NMS <- c("CZ","EE", "LT", "LV", "HU", "PL", "SI", "SK")

#Countries for which a full trend in regional PLE could not be established
incomplete <- c("DE", "DK", "IE", "NL", "SK", "UK", "SI", "LT")

#Countries, which have only 1 region
one_region <- c("LU", "EE", "LV")

#North Aegean islands, Ionian islands, Ceuta, Melilla, Aland, Aosta Valley all have less than 100,000 inhabitants
small <- c("EL41", "EL62", "ES63", "ES64", "FI20", "ITC2")

regional_data <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(country %in% one_region)) %>%
  filter(!(geo %in% small)) %>%
  mutate(NMS = ifelse(country %in% NMS, "New Member States", "Old Member States"),
         sex = ifelse(sex == "M", "Men", "Women"))

national_data <- national_data %>%
  mutate(NMS = ifelse(country %in% NMS, "New Member States", "Old Member States")) %>%
  filter(year %in% 1990:2017)
