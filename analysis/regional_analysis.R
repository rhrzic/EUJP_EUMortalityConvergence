# Impact of European integration on regional life expectancy convergence
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version March 2021

require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(purrr)
require(grid)
require(gridExtra)
require(cowplot)
source("analysis/two-stage decomp.R")













## Figure 4 ##

require(giscoR)
require(sf)

no_HU_PL_map <- gisco_get_nuts(
  year = "2016",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  verbose = FALSE,
  resolution = "20",
  spatialtype = "RG",
  nuts_level = "2"
) %>%
  filter(!CNTR_CODE %in% c("PL", "HU")) %>%
  select(NAME_LATN, NUTS_ID, geometry)

HU_PL_map <- gisco_get_nuts(
  year = "2013",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  verbose = FALSE,
  resolution = "20",
  spatialtype = "RG",
  nuts_level = "2"
) %>%
  filter(CNTR_CODE %in% c("PL", "HU"))%>%
  select(NAME_LATN, NUTS_ID, geometry)

EU_map <- rbind(no_HU_PL_map, HU_PL_map)

LE_map <- left_join(regional_data, EU_map, by = c("geo" = "NUTS_ID"))

p1 <- LE_map %>%
  filter(year == 1992, sex == "Men") %>%
  ggplot()+
  geom_sf(aes(fill = ple, colour = "white", geometry = geometry), size = 0.1) +
  scale_color_identity()+
  #geom_sf(data = states, aes(geometry = geometry), colour = "darkgrey", size = 0.1, fill = NA)+
  labs(fill = "Male life expectancy in 1992")+
  theme_void()+
  scale_fill_gradient(low = "gray5", high = "gray95", guide = "colourbar", limits = c(62,77), breaks = c(62,77))+
  guides(fill = guide_colourbar(ticks = FALSE, barheight = 0.5, barwidth = 9.5, title.position = "top"))+
  theme(legend.position = "top", legend.direction = "horizontal")

p2 <- LE_map %>%
  filter(year == 1992, sex == "Women") %>%
  ggplot()+
  geom_sf(aes(fill = ple, colour = "white", geometry = geometry), size = 0.1) +
  scale_color_identity()+
  #geom_sf(data = states, aes(geometry = geometry), colour = "darkgrey", size = 0.1, fill = NA)+
  labs(fill = "Female life expectancy in 1992")+
  theme_void()+
  scale_fill_gradient(low = "gray5", high = "gray95", guide = "colourbar", limits = c(73,84), breaks = c(73,84))+
  guides(fill = guide_colourbar(ticks = FALSE, barheight = 0.5, barwidth = 10.5, title.position = "top"))+
  theme(legend.position = "top", legend.direction = "horizontal")

p3 <- LE_map %>%
  filter(year == 2016, sex == "Men") %>%
  ggplot()+
  geom_sf(aes(fill = ple, geometry = geometry), colour = "white", size = 0.1) +
  scale_color_identity()+
  #geom_sf(data = states, aes(geometry = geometry), colour = "darkgrey", size = 0.1, fill = NA)+
  labs(fill = "Male life expectancy in 2016")+
  theme_void()+
  scale_fill_gradient(low = "gray5", high = "gray95", guide = "colourbar", limits = c(69,83), breaks = c(69,83))+
  guides(fill = guide_colourbar(ticks = FALSE, barheight = 0.5, barwidth = 9.5, title.position = "top"))+
  theme(legend.position = "top", legend.direction = "horizontal")

p4 <- LE_map %>%
  filter(year == 2016, sex == "Women") %>%
  ggplot()+
  geom_sf(aes(fill = ple, geometry = geometry), colour = "white", size = 0.1) +
  scale_color_identity()+
  #geom_sf(data = states, aes(geometry = geometry), colour = "darkgrey", size = 0.1, fill = NA)+
  labs(fill = "Female life expectancy in 2016")+
  theme_void()+
  scale_fill_gradient(low = "gray5", high = "gray95", guide = "colourbar", limits = c(78,88), breaks = c(78,88))+
  guides(fill = guide_colourbar(ticks = FALSE, barheight = 0.5, barwidth = 10.5, title.position = "top"))+
  theme(legend.position = "top", legend.direction = "horizontal")

p5 <- LE_map %>%
  filter(year %in% c(1992, 2016), sex == "Men") %>%
  group_by(geo) %>%
  summarise(ple = last(ple)-first(ple),
            geometry = geometry) %>%
  ggplot()+
  geom_sf(aes(fill = ple, colour = "white", geometry = geometry), size = 0.1) +
  scale_color_identity()+
  #geom_sf(data = states, aes(geometry = geometry), colour = "darkgrey", size = 0.1, fill = NA)+
  labs(fill = "Change in male life expectancy 1992-2016")+
  theme_void()+
  scale_fill_gradient(low = "gray5", high = "gray95", guide = "colourbar", limits = c(1,10), breaks = c(1,10))+
  guides(fill = guide_colourbar(ticks = FALSE, barheight = 0.5, barwidth = 14.5, title.position = "top"))+
  theme(legend.position = "top", legend.direction = "horizontal")

p6 <- LE_map %>%
  filter(year %in% c(1992, 2016), sex == "Women") %>%
  group_by(geo) %>%
  summarise(ple = last(ple)-first(ple),
            geometry = geometry) %>%
  ggplot()+
  geom_sf(aes(fill = ple, colour = "white", geometry = geometry), size = 0.1) +
  scale_color_identity()+
  #geom_sf(data = states, aes(geometry = geometry), colour = "darkgrey", size = 0.1, fill = NA)+
  labs(fill = "Change in female life expectancy 1992-2016")+
  theme_void()+
  scale_fill_gradient(low = "gray5", high = "gray95", guide = "colourbar", limits = c(1,8), breaks = c(1,8))+
  guides(fill = guide_colourbar(ticks = FALSE, barheight = 0.5, barwidth = 15, title.position = "top"))+
  theme(legend.position = "top", legend.direction = "horizontal")


fig4 <- plot_grid(p1, p3, p5, p2, p4, p6, nrow = 2, align = "hv", axis = "tblr")
ggsave('figures/fig4.eps', fig4, scale = 3, width = 5, height = 3.5, units = "in", device='eps', dpi=700)


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
