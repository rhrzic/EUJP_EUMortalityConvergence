# Impact of European integration on regional life expectancy convergence
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version April 2021

require(dplyr)
require(ggplot2)
require(directlabels)
require(tidyr)
require(stringr)
require(purrr)
require(grid)
require(gridExtra)
require(cowplot)
source("analysis/two-stage decomp.R")
require(scales)
require(boot)

## Regional LE trends

p1 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(national_data, country %in% c("PT", "LV") & year %in% 1992:2016 & sex == "Men"), aes(x = year, y = ple, group = country))+
  geom_line(data = filter(regional_data, country %in% c("CZ", "HU", "PL") & sex == "Men"), aes(x = year, y = ple, group = geo, linetype=country), size = 0.15)+
  geom_dl(data = filter(national_data, country %in% c("PT", "LV") & year %in% 1992:2016 & sex == "Men"), aes(label = country, x = year, y = ple), method=list("first.points", cex = 0.7))+
  geom_dl(data = filter(regional_data, geo %in% c("CZ01", "HU31", "PL32") & year %in% 1992:2016 & sex == "Men"), aes(label = geo, x = year, y = ple), method=list("last.points", cex = 0.7))+
  annotate("text", x = 2004.1, y = 62, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  facet_wrap(. ~ sex, scales = "free_y") +
  theme_bw()+ 
  scale_linetype_manual(values = c("CZ" = "solid", "HU" = "dotted", "PL" = "longdash"))+
  xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab("Life expectancy at birth")+
  theme(legend.position = "none")

p2 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(national_data, country %in% c("DK", "LV") & year %in% 1992:2016 & sex == "Women"), aes(x = year, y = ple, group = country))+
  geom_line(data = filter(regional_data, country %in% c("CZ", "HU", "PL")  & sex == "Women"), aes(x = year, y = ple, group = geo, linetype=country), size = 0.15)+
  geom_dl(data = filter(national_data, country %in% c("DK", "LV") & year %in% 1992:2016  & sex == "Women"), aes(label = country, x = year, y = ple), method=list("first.points", cex = 0.7))+
  geom_dl(data = filter(regional_data, geo %in% c("CZ01", "HU31", "PL32") & year %in% 1992:2016  & sex == "Women"), aes(label = geo, x = year, y = ple), method=list("last.points", cex = 0.7))+
  facet_wrap(. ~ sex, scales = "free_y") +
  theme_bw()+ 
  scale_linetype_manual(values = c("CZ" = "solid", "HU" = "dotted", "PL" = "longdash"))+
  xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab("")+
  theme(legend.position = "none")

regional_differences <- left_join(regional_data, national_data, by = c("year", "sex", "country", "NMS")) %>%
  mutate(ple = ple.x - ple.y)

fig4 <- plot_grid(p1, p2, nrow = 1, ncol = 2, align = "hv", axis = "tblr", labels = NA)
ggsave('figures/fig4.eps', fig4, scale = 2, width = 7, height = 3, units = "in", device='eps', dpi=700)
ggsave('figures/fig4.png', fig4, scale = 2, width = 7, height = 3, units = "in", device='png', dpi=200)


## Regional Beta-convergence analysis ##

require(purrr)

unconditional_beta <- function(data){lm(e0_diff ~ e0_start, data = data)}
extract_intercept <- function(mod){coef(summary(mod))[1,1]}
extract_coeff <- function(mod){round(coef(summary(mod))[2,1], 3)}
extract_se <- function(mod){round(coef(summary(mod))[2,2], 3)}
extract_CI <- function(mod){round(confint(mod, "e0_start", level = 0.95),3)}
extract_p <- function(mod){round(coef(summary(mod))[2,4], 5)}
extract_adjr2 <- function(mod){round(summary(mod)$adj.r.squared, 3)}

regional_data <- regional_data %>%
  filter(country %in% c("CZ", "HU", "PL"))

beta_convergence_regional_overall <- regional_data %>%
  filter(year %in% c(1992, 2016)) %>%
  group_by(sex, country, geo) %>%
  arrange(year, .by_group = T) %>%
  summarise(start_year=year,
            lag_year = lead(year),
            year = paste0(year, " - ", lag_year),
            ple = ple, 
            e0_diff_1 = (lead(ple)-ple),
            e0_diff = (lead(ple)-ple)/(lag_year-start_year),
            e0_start = ple) %>%
  filter(!is.na(e0_diff))

beta_models_regional_overall <- beta_convergence_regional_overall %>%
  group_by(sex, country, start_year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         intercept = map(models, extract_intercept),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         CI = map(models, extract_CI),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, start_year, intercept, beta, se, CI, pval, adjr2)

require(ggpmisc)

supp_fig3 <- ggplot(data = beta_convergence_regional_overall, aes(x = e0_start, y=e0_diff))+
  geom_text(aes(label = geo), position=position_jitter(width = 0.3), size = 3)+
  geom_smooth(method = "lm", se=FALSE, color = "black")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..p.value.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,
               size = 3,
               label.x = "left",
               label.y = "top") +         
  facet_wrap(country ~ sex, scales = "free", ncol = 2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Life expectancy in 1992",
       y = "Annual change in life expectancy 1992-2016",
       color = "")

ggsave('figures/supp_fig3.eps', supp_fig3, scale = 2, width = 6, height = 6, units = "in", device='eps', dpi=700)
ggsave('figures/supp_fig3.png', supp_fig3, scale = 1, width = 10, height = 10, units = "in", device='png', dpi=300)

beta_convergence_regional <- regional_data %>%
  group_by(sex, country, geo) %>%
  arrange(year, .by_group = T) %>%
  summarise(start_year=year,
            lag_year = lead(year, 4),
            year = paste0(year, " - ", lag_year),
            ple = ple, 
            e0_diff_1 = (lead(ple, 4)-ple),
            e0_diff = (lead(ple, 4)-ple)/(lag_year-start_year),
            e0_start = ple) %>%
  filter(!is.na(e0_diff))

beta_models_regional <- beta_convergence_regional %>%
  group_by(sex, country, start_year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, country, start_year, beta, se, pval, adjr2)

pA <- ggplot(beta_models_regional, aes(x = start_year, y = beta,ymin = beta-1.96*se, ymax = beta+1.96*se, group = interaction(country, sex)))+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line()+
  geom_pointrange()+
  facet_wrap(country ~ sex, ncol = 2, scales = "free")+
  xlab("Starting year") +
  ylab("Beta coefficient (95% confidence interval)") +
  #annotate("text", x = 2004.1, y = 0.05, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  theme(legend.position = "none")


beta_convergence_regional_six <- regional_data %>%
  group_by(sex, country, geo) %>%
  arrange(year, .by_group = T) %>%
  summarise(start_year=year,
            lag_year = lead(year, 6),
            year = paste0(year, " - ", lag_year),
            ple = ple, 
            e0_diff_1 = (lead(ple, 6)-ple),
            e0_diff = (lead(ple, 6)-ple)/(lag_year-start_year),
            e0_start = ple) %>%
  filter(!is.na(e0_diff))

beta_models_regional_six <- beta_convergence_regional_six %>%
  group_by(sex, country, start_year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, country, start_year, beta, se, pval, adjr2)


##In-depth analysis of polish regions

ggplot(filter(beta_models_regional, country == "PL"), aes(x = start_year, y = beta,ymin = beta-1.96*se, ymax = beta+1.96*se, group = interaction(country, sex)))+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line()+
  geom_pointrange()+
  facet_grid(country ~ sex, scales = "free")+
  xlab("Starting year") +
  ylab("Beta coefficient, 4-year lag (95% confidence interval)") +
  #annotate("text", x = 2004.1, y = 0.05, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  theme(legend.position = "none")

ggplot(filter(beta_models_regional_six, country == "PL"), aes(x = start_year, y = beta,ymin = beta-1.96*se, ymax = beta+1.96*se, group = interaction(country, sex)))+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line()+
  geom_pointrange()+
  facet_grid(country ~ sex, scales = "free")+
  xlab("Starting year") +
  ylab("Beta coefficient, 6-year lag (95% confidence interval)") +
  #annotate("text", x = 2004.1, y = 0.05, label = "Short-term\naccession\neffects", hjust = "left", size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  theme(legend.position = "none")

source("analysis/segmented.R")

extract_fit <- function(mod){broken.line(mod)$fit}
extract_psi <- function(mod){round(mod$psi[,2],2)+1991}
extract_psi_l <- function(mod){round(confint.segmented(mod, method = "delta")[,2],2)+1991}
extract_psi_u <- function(mod){round(confint.segmented(mod, method = "delta")[,3],2)+1991}
extract_r2 <- function(mod){round(1 - (summary(mod)$deviance/summary(mod)$null.deviance), 3)}

segmented_poland <- beta_models_regional %>%
  filter(country == "PL") %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$beta, se = .x$se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit <- segmented_poland %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1991)

segmented_poland_six <- beta_models_regional_six %>%
  filter(country == "PL") %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$beta, se = .x$se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_six <- segmented_poland_six %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1991)


ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit, sex == "Men"), aes(x = x, y = y_fit), size = 1)+
  geom_pointrange(data = filter(segmented_poland, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit, sex == "Men")$y_fit[round(psi,0)-1991]), size = 1.2)+
  geom_text(data = filter(segmented_poland, sex == "Men"), aes(x = 2010, y = 0.055, label = paste0("R2 = ",r2)), color = "black", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (4-year lag) - model of trend") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()


ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit, sex == "Men"), aes(x = x, y = y_fit), size = 1)+
  geom_pointrange(data = filter(segmented_poland_six, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit, sex == "Men")$y_fit[round(psi,0)-1991]), size = 1.2)+
  geom_text(data = filter(segmented_poland_six, sex == "Men"), aes(x = 2009, y = 0.055, label = paste0("R2 = ",r2)), color = "black", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (6-year lag) - model of trend") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()

## Regional dispersion analysis ##

dispersion_regional_ci <- regional_data %>%
  group_by(sex, country, year) %>%
  nest() %>%
  mutate(booted_theil = purrr::map(.x = data, ~boot::boot(data = .x, statistic = helper_theil, R = 1000, stype = "i")),
         booted_theil_ci = purrr::map(.x = booted_theil, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         theil = map(.x = booted_theil_ci, ~ .x$t0),
         theil_se = map(.x = booted_theil, ~sd(.x$t)),
         theil_l = map(.x = booted_theil_ci, ~ .x$bca[[4]]),
         theil_u = map(.x = booted_theil_ci, ~ .x$bca[[5]]),
         booted_var = purrr::map(.x = data, ~boot::boot(data = .x, statistic = helper_var, R = 1000, stype = "i")),
         booted_var_ci = purrr::map(.x = booted_var, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         var = map(.x = booted_var_ci, ~ .x$t0),
         var_se = map(.x = booted_var, ~sd(.x$t)),
         var_l = map(.x = booted_var_ci, ~ .x$bca[[4]]),
         var_u = map(.x = booted_var_ci, ~ .x$bca[[5]])) %>%
  unnest(cols = c(theil, theil_se, theil_l, theil_u, var, var_se, var_l, var_u)) %>%
  select(sex, country, year, theil, theil_se, theil_l, theil_u, var, var_se, var_l, var_u) %>%
  mutate_at(4:11, signif, 3)


pB <- ggplot(dispersion_regional_ci, aes(x = year, y = var, ymin = var_l, ymax = var_u,))+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line()+
  geom_pointrange()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_wrap(country ~ sex, ncol = 2, scales = "free_y")+
  theme(legend.position = "top", legend.direction = "horizontal", legend.title=element_blank())+
  xlab("Year")+
  ylab("Variance (95% bootstrap confidence interval)")


supp_fig4 <- plot_grid(pA, pB, ncol = 1, nrow=2)
ggsave('figures/supp_fig4.eps', supp_fig4, scale = 3, width = 4.5, height = 6, units = "in", device='eps', dpi=700)
ggsave('figures/supp_fig4.png', supp_fig4, scale = 2, width = 4.5, height = 6, units = "in", device='png', dpi=300)


ggplot(dispersion_regional_ci, aes(x = year, y = theil, ymin = theil_l, ymax = theil_u,))+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line()+
  geom_pointrange()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_wrap(country ~ sex, ncol = 2, scales = "free_y")+
  theme(legend.position = "top", legend.direction = "horizontal", legend.title=element_blank())+
  xlab("Year")+
  ylab("Theil index (95% bootstrap confidence interval)")

## Trajectory analysis ##

segmented_regional <- regional_data %>%
  filter(country %in% c("CZ", "PL", "HU")) %>%
  group_by(geo, sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(.x$ple)))

plot_seg_reg <- function(model){
  
  y_fit = broken.line(model)$fit
  y_orig = model$y
  year = 1992:2016
  
  psi = round(model$psi[,2],2)
  psi_se = model$psi[,3]
  
  r2 = round(1 - (summary(model)$deviance/summary(model)$null.deviance), 3)
  
  ggplot()+
    geom_line(aes(x = year, y=y_orig), color = "gray")+
    annotate("pointrange", x = psi+1991, xmax = psi+1991+2*psi_se, xmin = psi+1991-2*psi_se,  y=y_orig[round(psi,0)], color = "red")+
    annotate("text", x = 2013, y=y_orig[20]-5, label=paste0("R2 = ", r2), size = 5, color = "red")+
    annotate("text", x = psi+1991, y=y_orig[round(psi,0)]+0.5, label=paste0(psi+1991," (",round(psi+1991-2*psi_se,2),", ",round(psi+1991+2*psi_se,2),")"), size = 5)+
    xlab("Year")+
    ylab("Life expectancy at birth")+
    theme_bw()
}

plots <- segmented_regional %>%
  mutate(plots = map(.x = segmented_model, ~plot_seg_reg(.x)))

result <- segmented_regional %>%
  mutate(psi_ci = map(.x = segmented_model, ~confint.segmented(.x)),
         psi_est = map(.x = psi_ci, ~.x[,"Est."]+1991),
         psi_l = map(.x = psi_ci, ~.x[,"CI(95%).low"]+1991),
         psi_u = map(.x = psi_ci, ~.x[,"CI(95%).up"]+1991),
         r2 = map(.x = segmented_model, ~(1-.x$deviance/.x$null.deviance))) %>%
  unnest(cols = c(geo, sex, r2, psi_est, psi_l, psi_u)) %>%
  select(geo, sex, r2, psi_est, psi_l, psi_u) %>%
  mutate_at(4:6, signif, 5) %>%
  mutate_at(3, signif, 3)

result %>%
  filter(between(psi_est, 2004.00, 2007.00) & between(psi_l, 2004.00, 2007.00) & between(psi_u, 2004.00, 2007.00)) 

labels <- read.csv2(file = "dataset/NUTS2_labels.csv") %>%
  select(GEO, GEO_LABEL)

result %>%
  left_join(., labels, by = c("geo" = "GEO")) %>%
  select(Region = GEO_LABEL, sex, r2, psi_est, psi_l, psi_u) %>%
  write.table(., file = "regions.txt", sep = ",", quote = FALSE, row.names = F)

supp_fig4 <- plot_grid(plotlist = plots$plots, labels = paste0(plots$geo, " (",plots$sex,")"))
ggsave('figures/supp_fig4.png', supp_fig4, scale = 4, width = 9, height = 6, units = "in", device='png', dpi=300)




