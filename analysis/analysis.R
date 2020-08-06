# Impact of European integration on regional life expectancy convergence
# P2
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version August 2020

require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(grid)
require(gridExtra)
require(cowplot)
source("analysis/two-stage decomp.R")

regional_data <- read.csv(file = "dataset/regional_data.csv")
national_data <- read.csv(file = "dataset/national_data.csv")

#NMS category
NMS <- c("CZ","EE", "LT", "LV", "HU", "PL", "SI", "SK")

#Countries for which a full trend in regional PLE could not be established
incomplete <- c("DE", "DK", "IE", "NL", "SK", "UK")

#North Aegean islands, Ionian islands, Ceuta, Melilla, Aland, Aosta Valley all have less than 100000 inhabitants
small <- c("EL41", "EL62", "ES63", "ES64", "FI20", "ITC2")

regional_data <- regional_data %>%
  mutate(NMS = ifelse(country %in% NMS, "New Member States", "Old Member States")) %>%
  mutate(NMS = as.factor(NMS))

national_data <- national_data %>%
  mutate(NMS = ifelse(country %in% NMS, "New Member States", "Old Member States")) %>%
  mutate(NMS = as.factor(NMS))

##LE ranges (text under Figure 1)##

range <- function(x) {
  data.frame(ymin=min(x),
             ymax=max(x))
}

national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year == 1992) %>%
  group_by(NMS, sex) %>%
  summarise(range(ple))

national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year == 2016) %>%
  group_by(NMS, sex) %>%
  summarise(range(ple))

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year == 1992) %>%
  group_by(NMS, sex) %>%
  summarise(range(ple))

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year == 2016) %>%
  group_by(NMS, sex) %>%
  summarise(range(ple))

##Figure 1##

p1 <- national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  ggplot(aes(x = year, y = ple, group = country, colour = NMS), size = 0.8) +
  geom_line() +
  stat_summary_bin(aes(group = NMS), geom = "linerange", 
               fun.data = range, 
               breaks = c(1992,1992.5),
               position = position_nudge(x=-1),
               alpha = 0.7)+
  stat_summary_bin(aes(group = NMS), geom = "linerange", 
                   fun.data = range, 
                   breaks = c(2015.5,2016),
                   position = position_nudge(x=1),
                   alpha = 0.7)+
  geom_vline(xintercept = 2004, size = 0.3, linetype = "dashed") +
  annotate("text", x = 2004.1, y = 60, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  geom_vline(xintercept = 2007, size = 0.3, linetype = "dashed") +
  facet_wrap(. ~ sex) +
  xlab("Year") +
  ylab("Life expectancy at birth (national data)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  labs(colour = "Country group") +
  theme(legend.position = "top", legend.direction = "horizontal")

p2 <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  ggplot(aes(x = year, y = ple, group = geo, colour = NMS)) +
  geom_line(size = 0.3) +
  stat_summary_bin(aes(group = NMS), geom = "linerange", 
                   fun.data = range, 
                   breaks = c(1992,1992.5),
                   position = position_nudge(x=-1),
                   alpha = 0.7)+
  stat_summary_bin(aes(group = NMS), geom = "linerange", 
                   fun.data = range, 
                   breaks = c(2015.5,2016),
                   position = position_nudge(x=1),
                   alpha = 0.7)+
  geom_vline(xintercept = 2004, size = 0.3, linetype = "dashed") +
  annotate("text", x = 2004.1, y = 60, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  geom_vline(xintercept = 2007, size = 0.3, linetype = "dashed") +  scale_colour_grey() +  
  facet_grid( ~ sex) +
  xlab("Year") +
  ylab("Life expectancy at birth (regional data)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  labs(colour = "Country", linetype = "Country") +
  theme_bw() +
  theme(legend.position = "none")

fig1 <- plot_grid(p1, p2, ncol = 1, align = "hv", axis = "tblr", labels = c("a","b"))
ggsave('figures/fig1.png', fig1, scale = 3, width = 4, height = 3, units = "in")

## Changes in Theil index over time ##
national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  ungroup() %>%
  group_by(sex) %>%
  summarise(first = first(Ttotal), last = last(Ttotal), diff = (last-first) / first)

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  ungroup() %>%
  group_by(sex) %>%
  summarise(first = first(Ttotal), last = last(Ttotal), diff = (last-first) / first)

##Figure 2##

male <- national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  filter(year %in% 1996:2003, sex == "M") %>%
  lm(formula=Ttotal ~ year)

female <- national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  filter(year %in% 1996:2003, sex == "F") %>%
  lm(formula=Ttotal ~ year)

f <- data.frame(year = 2004:2016, sex = "F")
f$Ttotal <- predict(female, f)
  
m <- data.frame(year = 2004:2016, sex = "M")
m$Ttotal <- predict(male, m)

extrapol <- rbind(f,m)

p1 <- national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  ggplot(aes(x = year, y = Ttotal))+
  geom_line()+
  geom_line(data=extrapol, aes(x = year, y = Ttotal), linetype = 3) +
  scale_colour_grey() +
  xlab(NULL) +
  ylab("Theil index (national data)") +
  ylim(c(0, 0.003)) +
  #labs(linetype = "Contribution by\ngeographic level")+
  geom_vline(xintercept = 2004, size = 0.3, linetype = "dashed") +
  annotate("text", x = 2004.1, y = 0.0025, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  geom_vline(xintercept = 2007, size = 0.3, linetype = "dashed") +  scale_colour_grey() +  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_grid(. ~ sex)+
  theme_bw()+
  theme(legend.position = "none")

male <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  filter(year %in% 1996:2003, sex == "M") %>%
  lm(formula=Ttotal ~ year)

female <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  filter(year %in% 1996:2003, sex == "F") %>%
  lm(formula=Ttotal ~ year)

f <- data.frame(year = 2004:2016, sex = "F")
f$Ttotal <- predict(female, f)

m <- data.frame(year = 2004:2016, sex = "M")
m$Ttotal <- predict(male, m)

extrapol <- rbind(f,m)

p2 <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  ggplot(aes(x = year, y = Ttotal))+
  geom_line()+
  geom_line(data=extrapol, aes(x = year, y = Ttotal), linetype = 3) +
  scale_colour_grey() +
  xlab(NULL) +
  ylab("Theil index (regional data)") +
  ylim(c(0, 0.003)) +
  #labs(linetype = "Contribution by\ngeographic level")+
  geom_vline(xintercept = 2004, size = 0.3, linetype = "dashed") +
  annotate("text", x = 2004.1, y = 0.0025, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  geom_vline(xintercept = 2007, size = 0.3, linetype = "dashed") +  scale_colour_grey() +  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_grid(. ~ sex)+
  theme_bw()+
  theme(legend.position = "none")

fig2 <- plot_grid(p1,p2, align = "v", axis = "lr", ncol = 1, labels = c("a", "b"))

ggsave('figures/fig2.png', fig2, scale = 3, width = 4, height = 3, units = "in")

##Table 2##

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992, 2004, 2016)) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(twostage.Theil.decomp(df = ., country = "country", macroregion = "NMS", ple = "ple")) %>%
  mutate(All = Ttotal, Between_country_groups = Tbmr, Within_country_groups = Tbc, Within_specific_countries = Twc) %>%
  pivot_longer(cols = All:Within_specific_countries, names_to = "Level", values_to = "Theil") %>%
  ungroup() %>%
  group_by(sex, year) %>%
  summarise(within_countries = last(Twc)/last(Ttotal), 
            within_country_groups = last(Tbc)/last(Ttotal), 
            between_country_groups = last(Tbmr)/last(Ttotal)) %>%
  print(n = 1000)


regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992, 2004)) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(twostage.Theil.decomp(df = ., country = "country", macroregion = "NMS", ple = "ple")) %>%
  mutate(All = Ttotal, Between_country_groups = Tbmr, Within_country_groups = Tbc, Within_specific_countries = Twc) %>%
  group_by(sex) %>%
  summarise((last(Tbmr)-first(Tbmr))/first(Tbmr),
            (last(Tbc)-first(Tbc))/first(Tbc),
            (last(Twc)-first(Twc))/first(Twc),
            (last(Ttotal)-first(Ttotal))/first(Ttotal))

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(2004, 2016)) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(twostage.Theil.decomp(df = ., country = "country", macroregion = "NMS", ple = "ple")) %>%
  mutate(All = Ttotal, Between_country_groups = Tbmr, Within_country_groups = Tbc, Within_specific_countries = Twc) %>%
  group_by(sex) %>%
  summarise((last(Tbmr)-first(Tbmr))/first(Tbmr),
            (last(Tbc)-first(Tbc))/first(Tbc),
            (last(Twc)-first(Twc))/first(Twc),
            (last(Ttotal)-first(Ttotal))/first(Ttotal))
  

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992, 2016)) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(twostage.Theil.decomp(df = ., country = "country", macroregion = "NMS", ple = "ple")) %>%
  mutate(All = Ttotal, Between_country_groups = Tbmr, Within_country_groups = Tbc, Within_specific_countries = Twc) %>%
  group_by(sex) %>%
  summarise((last(Tbmr)-first(Tbmr))/first(Tbmr),
            (last(Tbc)-first(Tbc))/first(Tbc),
            (last(Twc)-first(Twc))/first(Twc),
            (last(Ttotal)-first(Ttotal))/first(Ttotal))

##FIGURE 3##

fig3 <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
  group_by(year, sex) %>%
  do(twostage.Theil.decomp(df = ., country = "country", macroregion = "NMS", ple = "ple")) %>%
  mutate(All = Ttotal, Between_country_groups = Tbmr, Within_country_groups = Tbc, Within_specific_countries = Twc) %>%
  pivot_longer(cols = All:Within_specific_countries, names_to = "Level", values_to = "Theil") %>%
  ggplot(aes(x = year, y = Theil, linetype = Level))+
  geom_line()+
  scale_colour_grey() +
  xlab("Year") +
  ylab("Theil index (regional data)") +
  geom_vline(xintercept = 2004, size = 0.3, linetype = "dashed") +
  annotate("text", x = 2004.1, y = 0.0012, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  geom_vline(xintercept = 2007, size = 0.3, linetype = "dashed") +  scale_colour_grey() +  
  facet_grid(. ~ sex)+
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"),
                        name = "Inequality",
                        labels = c("Total", "Between country groups", "Between countries within country groups", "Within countries"))+
  theme(legend.position = "top", legend.direction = "horizontal")


ggsave('figures/fig3.png', fig3, scale = 3, width = 4, height = 3, units = "in")

##EXPORTS FOR JOINPOINT ANALYSIS##

#Preparing datasets to export to Joinpoint software#

#Overall trend in Theil index (Table 1)#

Overall_Theil_regional <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  mutate(ple = as.numeric(ple)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  arrange(sex, year)

write.csv(Overall_Theil_regional, file = "dataset/theil_regional.csv", row.names = F)

Overall_Theil_national <- national_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(year %in% 1992:2016) %>%
  mutate(ple = as.numeric(ple)) %>%
  group_by(year, sex) %>%
  do(Theil(df = ., ple = "ple")) %>%
  arrange(sex, year)

write.csv(Overall_Theil_national, file = "dataset/theil_national.csv", row.names = F)

#Regional trends (Table 3)#

for (l in NMS){
  
  print(l)
  
  temp <- regional_data %>%
    filter(!(country %in% incomplete)) %>%
    filter(!(geo %in% small)) %>%
    filter(country == l) %>%
    mutate(ple = as.numeric(ple), country = as.character(country), NMS = as.character(NMS)) %>%
    arrange(sex, geo, year)
  
  write.csv(temp, file = paste0("dataset/",as.character(l),".csv"), row.names = F)
  
}


##Supplementary table 2 & associated numbers for the manuscript text##

pre <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992,2004)) %>%
  filter(NMS == "New Member States") %>%
  group_by(sex, geo, country) %>%
  summarise('1992-2004' = last(ple)-first(ple))

post <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(2004,2016)) %>%
  filter(NMS == "New Member States") %>%
  group_by(sex, geo, country) %>%
  summarise('2004-2016' = last(ple)-first(ple))

ST2 <- left_join(pre, post, by = c("sex", "geo", "country"))

NUTS2_labels <- read.csv("dataset/NUTS2_labels.csv", sep = ";")

ST2 <- left_join(ST2, NUTS2_labels, by = c("geo" = "GEO")) %>%
  select(geo, Region = GEO_LABEL, Country = country, Sex = sex, `1992-2004`, `2004-2016`) %>%
  mutate(`1992-2004` = round(`1992-2004`, 1), 
         `2004-2016` = round(`2004-2016`, 1))

write.table(ST2, file = "ST2.txt", sep = ";", row.names = F)

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992,2004)) %>%
  group_by(sex, geo, NMS) %>%
  summarise(diff = last(ple)-first(ple)) %>%
  group_by(NMS) %>%
  summarise(mean(diff))

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992,2004)) %>%
  filter(NMS == "New Member States") %>%
  group_by(sex, geo, country) %>%
  summarise(diff = last(ple)-first(ple)) %>%
  group_by(country, sex) %>%
  summarise(mean(diff))

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(1992,2004)) %>%
  filter(country == "PL") %>%
  group_by(sex, geo, country) %>%
  summarise(diff = last(ple)-first(ple)) %>%
  arrange(diff)

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(2004,2016)) %>%
  filter(NMS == "New Member States") %>%
  group_by(sex, geo, country) %>%
  summarise(diff = last(ple)-first(ple)) %>%
  group_by(country, sex) %>%
  summarise(mean(diff))

regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% c(2004,2016)) %>%
  filter(country == "PL") %>%
  group_by(sex, geo, country) %>%
  summarise(diff = last(ple)-first(ple)) %>%
  arrange(diff)


##Supplementary Figure 1##

p1 <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% 1992:2004) %>%
  filter(NMS == "New Member States") %>%
  group_by(sex, geo) %>%
  mutate(ple_new = ple-first(ple)) %>%
  ggplot(aes(x = year, y = ple_new, group = geo))+
  geom_line(aes(linetype = country, color = country), size = 0.3)+
  facet_grid(. ~ sex) +
  theme_bw()+
  xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab("Change in life expectancy at birth 1992-2004\n(regional data)")

p2 <- regional_data %>%
  filter(!(country %in% incomplete)) %>%
  filter(!(geo %in% small)) %>%
  filter(year %in% 2004:2016) %>%
  filter(NMS == "New Member States") %>%
  group_by(sex, geo) %>%
  mutate(ple_new = ple-first(ple)) %>%
  ggplot(aes(x = year, y = ple_new, group = geo))+
  geom_line(aes(linetype = country, color = country), size = 0.3)+
  facet_grid(. ~ sex)+
  theme_bw()+
  xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab("Change in life expectancy at birth 2004-2016\n(regional data)")

fig4 <- plot_grid(p1,p2, align = "v", axis = "lr", ncol = 1, labels = c("a", "b"))

ggsave('figures/sup_fig1.png', fig4, scale = 3, width = 4, height = 3, units = "in")
