# Impact of EU on life expectancy convergence
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version March 2021

# HMD data extraction script

# Data download and import
# The datasets of national life expectancy is downloaded from the HMD website using HMDHFDplus. 
# We transform the class of the time variable to numeric for easier manipulation later on.
# Since we are interested in life expectancy at birth, we also limit the dataset to this indicator. We chose to do this early on, since it vastly reduces the size of the dataset.

require(dplyr)
require(tidyr)
require(HMDHFDplus)

source("analysis/hmd_ident.R") #This microscript defines username and password variables

grabLE <- function(country) {
  ple <- readHMDweb(CNTRY = country, item = "E0per", username, password, fixup = TRUE) %>%
    mutate(country = country) %>%
    filter(Year >= 1990)
  
  print(paste0("Grabbed ple for ", country))
  
  return(ple)
}

grabPOP <- function(country) {
  
  pop <- readHMDweb(CNTRY = country, item = "Population", username, password, fixup = TRUE) %>% 
    group_by(Year) %>%
    summarise(Female_pop = sum(Female1), Male_pop = sum(Male1)) %>%
    mutate(country = country) %>%
    filter(Year >= 1990)
  
  print(paste0("Grabbed popsize for ", country))
  
  return(pop)
}

#join and clean

national_data_ple <- rbind(grabLE("AUT"), 
                  grabLE("BEL"), 
                  grabLE("CZE"), 
                  grabLE("DNK"), 
                  grabLE("EST"), 
                  grabLE("FIN"),
                  grabLE("FRATNP"), 
                  grabLE("DEUTNP"), 
                  grabLE("GRC"), 
                  grabLE("HUN"), 
                  grabLE("IRL"), 
                  grabLE("ITA"), 
                  grabLE("LVA"), 
                  grabLE("LTU"), 
                  grabLE("LUX"), 
                  grabLE("NLD"), 
                  grabLE("POL"), 
                  grabLE("PRT"), 
                  grabLE("SVK"), 
                  grabLE("SVN"), 
                  grabLE("ESP"), 
                  grabLE("SWE"), 
                  grabLE("GBR_NP"))

national_data_pop <- rbind(grabPOP("AUT"), 
                           grabPOP("BEL"), 
                           grabPOP("CZE"), 
                           grabPOP("DNK"), 
                           grabPOP("EST"), 
                           grabPOP("FIN"),
                           grabPOP("FRATNP"), 
                           grabPOP("DEUTNP"), 
                           grabPOP("GRC"), 
                           grabPOP("HUN"), 
                           grabPOP("IRL"), 
                           grabPOP("ITA"), 
                           grabPOP("LVA"), 
                           grabPOP("LTU"), 
                           grabPOP("LUX"), 
                           grabPOP("NLD"), 
                           grabPOP("POL"), 
                           grabPOP("PRT"), 
                           grabPOP("SVK"), 
                           grabPOP("SVN"), 
                           grabPOP("ESP"), 
                           grabPOP("SWE"), 
                           grabPOP("GBR_NP"))

national_data_ple <- national_data_ple %>% 
  select(year = "Year", country, Women = "Female" , Men = "Male") %>%
  pivot_longer(Women:Men, names_to = "sex", values_to = "ple")
  
national_data_pop <- national_data_pop %>%
  select(year = "Year", country, Women = "Female_pop" , Men = "Male_pop") %>%
  pivot_longer(Women:Men, names_to = "sex", values_to = "pop")

national_data = left_join(national_data_ple, national_data_pop, by = c("year", "country", "sex")) %>%  
  mutate(country = case_when(country == "AUT" ~ "AT",
                             country == "BEL" ~ "BE",
                             country == "CZE" ~ "CZ",
                             country == "DNK" ~ "DK",
                             country == "EST" ~ "EE",
                             country == "FIN" ~ "FI",
                             country == "FRATNP" ~ "FR",
                             country == "DEUTNP" ~ "DE",
                             country == "GRC" ~ "EL",
                             country == "HUN" ~ "HU",
                             country == "IRL" ~ "IE",
                             country == "ITA" ~ "IT",
                             country == "LVA" ~ "LV",
                             country == "LTU" ~ "LT",
                             country == "LUX" ~ "LU",
                             country == "NLD" ~ "NL",
                             country == "POL" ~ "PL",
                             country == "PRT" ~ "PT",
                             country == "SVK" ~ "SK",
                             country == "SVN" ~ "SI",
                             country == "ESP" ~ "ES",
                             country == "SWE" ~ "SE",
                             country == "GBR_NP" ~ "UK"))

#write.csv(national_data, "dataset/national_data.csv", row.names=F)
