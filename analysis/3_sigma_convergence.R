# Impact of European integration on regional life expectancy convergence
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version April 2021

require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(purrr)
require(grid)
require(gridExtra)
require(cowplot)
source("analysis/two-stage decomp.R")
require(scales)
require(boot)


set.seed(1)


## Changes in dispersion over time ##

## Bootstrap overall differences, unweighted and weighted

theil_abs_diff = function(d, i) {
  d=d[i,]
  theil_diff = Theil(d$ple_2017)-Theil(d$ple_1990)
  return(theil_diff)
}

var_abs_diff = function(d, i) {
  d=d[i,]
  var_diff = var(d$ple_2017)-var(d$ple_1990)
  return(var_diff)
}

theil_rel_diff = function(d, i) {
  d=d[i,]
  theil_diff = 100*(Theil(d$ple_2017)-Theil(d$ple_1990))/Theil(d$ple_1990)
  return(theil_diff)
}

var_rel_diff = function(d, i) {
  d=d[i,]
  var_diff = 100*(var(d$ple_2017)-var(d$ple_1990))/var(d$ple_1990)
  return(var_diff)
}


difference_disp <- national_data %>%
  filter(year %in% c(1990, 2017)) %>%
  pivot_wider(names_from = year, names_glue = "{.value}_{year}", values_from = c(ple,pop)) %>%
  group_by(sex) %>%
  nest() %>%
  mutate(booted_theil = purrr::map(.x = data, ~boot::boot(data = .x, statistic = theil_abs_diff, R = 1000, stype = "i")),
         booted_theil_ci = purrr::map(.x = booted_theil, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         theil = map(.x = booted_theil_ci, ~ .x$t0),
         theil_l = map(.x = booted_theil_ci, ~ .x$bca[[4]]),
         theil_u = map(.x = booted_theil_ci, ~ .x$bca[[5]]),
         booted_var = purrr::map(.x = data, ~boot::boot(data = .x, statistic = var_abs_diff, R = 1000, stype = "i")),
         booted_var_ci = purrr::map(.x = booted_var, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         var = map(.x = booted_var_ci, ~ .x$t0),
         var_l = map(.x = booted_var_ci, ~ .x$bca[[4]]),
         var_u = map(.x = booted_var_ci, ~ .x$bca[[5]]),
         booted_theil_w = purrr::map(.x = data, ~boot::boot(data = .x, statistic = theil_rel_diff, R = 1000, stype = "i")),
         booted_theil_w_ci = purrr::map(.x = booted_theil_w, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         theil_w = map(.x = booted_theil_w_ci, ~ .x$t0),
         theil_w_l = map(.x = booted_theil_w_ci, ~ .x$bca[[4]]),
         theil_w_u = map(.x = booted_theil_w_ci, ~ .x$bca[[5]]),
         booted_var_w = purrr::map(.x = data, ~boot::boot(data = .x, statistic = var_rel_diff, R = 1000, stype = "i")),
         booted_var_w_ci = purrr::map(.x = booted_var_w, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         var_w = map(.x = booted_var_w_ci, ~ .x$t0),
         var_w_l = map(.x = booted_var_w_ci, ~ .x$bca[[4]]),
         var_w_u = map(.x = booted_var_w_ci, ~ .x$bca[[5]])) %>%
  unnest(cols = c(theil, theil_l, theil_u, var, var_l, var_u,
                  theil_w, theil_w_l, theil_w_u, var_w, var_w_l, var_w_u)) %>%
  select(sex, theil, theil_l, theil_u, var, var_l, var_u,
         theil_w, theil_w_l, theil_w_u, var_w, var_w_l, var_w_u) %>%
  mutate_at(2:13, signif, 3)

theil_abs_diff_w = function(d, i) {
  d=d[i,]
  theil_diff = Theil_w(d$ple_2017, d$pop_2017)-Theil_w(d$ple_1990, d$pop_1990)
  return(theil_diff)
}

var_abs_diff_w = function(d, i) {
  d=d[i,]
  var_diff = wtd.var(d$ple_2017, d$pop_2017)-wtd.var(d$ple_1990, d$pop_1990)
  return(var_diff)
}

theil_rel_diff_w = function(d, i) {
  d=d[i,]
  theil_diff = 100*(Theil_w(d$ple_2017,d$pop_2017)-Theil_w(d$ple_1990,d$pop_1990))/Theil_w(d$ple_1990,d$pop_1990)
  return(theil_diff)
}

var_rel_diff_w = function(d, i) {
  d=d[i,]
  var_diff = 100*(wtd.var(d$ple_2017,d$pop_2017)-wtd.var(d$ple_1990,d$pop_1990))/wtd.var(d$ple_1990,d$pop_1990)
  return(var_diff)
}

difference_disp_w <- national_data %>%
  filter(year %in% c(1990, 2017)) %>%
  pivot_wider(names_from = year, names_glue = "{.value}_{year}", values_from = c(ple,pop)) %>%
  group_by(sex) %>%
  nest() %>%
  mutate(booted_theil = purrr::map(.x = data, ~boot::boot(data = .x, statistic = theil_abs_diff_w, R = 1000, stype = "i")),
         booted_theil_ci = purrr::map(.x = booted_theil, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         theil = map(.x = booted_theil_ci, ~ .x$t0),
         theil_l = map(.x = booted_theil_ci, ~ .x$bca[[4]]),
         theil_u = map(.x = booted_theil_ci, ~ .x$bca[[5]]),
         booted_var = purrr::map(.x = data, ~boot::boot(data = .x, statistic = var_abs_diff_w, R = 1000, stype = "i")),
         booted_var_ci = purrr::map(.x = booted_var, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         var = map(.x = booted_var_ci, ~ .x$t0),
         var_l = map(.x = booted_var_ci, ~ .x$bca[[4]]),
         var_u = map(.x = booted_var_ci, ~ .x$bca[[5]]),
         booted_theil_w = purrr::map(.x = data, ~boot::boot(data = .x, statistic = theil_rel_diff_w, R = 1000, stype = "i")),
         booted_theil_w_ci = purrr::map(.x = booted_theil_w, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         theil_w = map(.x = booted_theil_w_ci, ~ .x$t0),
         theil_w_l = map(.x = booted_theil_w_ci, ~ .x$bca[[4]]),
         theil_w_u = map(.x = booted_theil_w_ci, ~ .x$bca[[5]]),
         booted_var_w = purrr::map(.x = data, ~boot::boot(data = .x, statistic = var_rel_diff_w, R = 1000, stype = "i")),
         booted_var_w_ci = purrr::map(.x = booted_var_w, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         var_w = map(.x = booted_var_w_ci, ~ .x$t0),
         var_w_l = map(.x = booted_var_w_ci, ~ .x$bca[[4]]),
         var_w_u = map(.x = booted_var_w_ci, ~ .x$bca[[5]])) %>%
  unnest(cols = c(theil, theil_l, theil_u, var, var_l, var_u,
                  theil_w, theil_w_l, theil_w_u, var_w, var_w_l, var_w_u)) %>%
  select(sex, theil, theil_l, theil_u, var, var_l, var_u,
         theil_w, theil_w_l, theil_w_u, var_w, var_w_l, var_w_u) %>%
  mutate_at(2:13, signif, 3)

## Adding bootstrapped CIs to ineq measures

helper_theil = function(d, i) { 
  d=d[i,]
  theil = Theil(d$ple)
  return(theil)
}

helper_var <- function(d, i){
  d=d[i,]
  var = var(d$ple)
  return(var)
}

helper_theil_w = function(d, i) { 
  d=d[i,]
  theil_w = Theil_w(d$ple, d$pop)
  return(theil_w)
}

helper_var_w <- function(d, i){
  d=d[i,]
  var_w = wtd.var(d$ple, d$pop)
  return(var_w)
}

dispersion_national_ci <- national_data %>%
  group_by(sex, year) %>%
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
         var_u = map(.x = booted_var_ci, ~ .x$bca[[5]]),
         booted_theil_w = purrr::map(.x = data, ~boot::boot(data = .x, statistic = helper_theil_w, R = 1000, stype = "i")),
         booted_theil_w_ci = purrr::map(.x = booted_theil_w, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         theil_w = map(.x = booted_theil_w_ci, ~ .x$t0),
         theil_w_se = map(.x = booted_theil_w, ~sd(.x$t)),
         theil_w_l = map(.x = booted_theil_w_ci, ~ .x$bca[[4]]),
         theil_w_u = map(.x = booted_theil_w_ci, ~ .x$bca[[5]]),
         booted_var_w = purrr::map(.x = data, ~boot::boot(data = .x, statistic = helper_var_w, R = 1000, stype = "i")),
         booted_var_w_ci = purrr::map(.x = booted_var_w, ~ boot::boot.ci(.x, conf = 0.95, type = "bca")),
         var_w = map(.x = booted_var_w_ci, ~ .x$t0),
         var_w_se = map(.x = booted_var_w, ~sd(.x$t)),
         var_w_l = map(.x = booted_var_w_ci, ~ .x$bca[[4]]),
         var_w_u = map(.x = booted_var_w_ci, ~ .x$bca[[5]])) %>%
  unnest(cols = c(theil, theil_se, theil_l, theil_u, var, var_se, var_l, var_u,
                  theil_w, theil_w_se, theil_w_l, theil_w_u, var_w, var_w_se, var_w_l, var_w_u)) %>%
  select(sex, year, theil, theil_se, theil_l, theil_u, var, var_se, var_l, var_u,
         theil_w, theil_w_se, theil_w_l, theil_w_u, var_w, var_w_se, var_w_l, var_w_u) %>%
  mutate_at(3:18, signif, 3)

p1 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year, y = var, colour = "Unweighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year, y = var, ymin = var_l, ymax = var_u, colour = "Unweighted"))+
  geom_line(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year+0.1, y = var_w, colour = "Population weighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year+0.1, y = var_w, ymin = var_w_l, ymax = var_w_u, colour = "Population weighted"))+
  scale_colour_manual(values = c("Unweighted" = "black", "Population weighted" = "gray"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_wrap(. ~ sex, scales = "free_y")+
  theme(legend.position = "top", legend.direction = "horizontal", legend.title=element_blank())+
  xlab("Year")+
  ylab("Variance (95% bootstrap confidence interval)")+
  ggtitle("Variance in life expectancy")

p2 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year, y = var, colour = "Unweighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year, y = var, ymin = var_l, ymax = var_u, colour = "Unweighted"))+
  geom_line(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year+0.1, y = var_w, colour = "Population weighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year+0.1, y = var_w, ymin = var_w_l, ymax = var_w_u, colour = "Population weighted"))+
  scale_colour_manual(values = c("Unweighted" = "black", "Population weighted" = "gray"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_wrap(. ~ sex, scales = "free_y")+
  theme(legend.position = "none")+
  xlab("Year")+
  ylab("")

source("analysis/segmented.R")

extract_fit <- function(mod){broken.line(mod)$fit}
extract_psi <- function(mod){round(mod$psi[,2],2)+1989}
extract_psi_l <- function(mod){round(confint.segmented(mod, method = "delta")[,2],2)+1989}
extract_psi_u <- function(mod){round(confint.segmented(mod, method = "delta")[,3],2)+1989}
extract_r2 <- function(mod){round(1 - (summary(mod)$deviance/summary(mod)$null.deviance), 3)}

segmented_sigma <- dispersion_national_ci %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$var, se = .x$var_se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit <- segmented_sigma %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)


segmented_sigma_w <- dispersion_national_ci %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$var_w, se = .x$var_w_se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_w <- segmented_sigma_w %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)

p3 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit, sex == "Men"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w, sex == "Men"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_sigma, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit, sex == "Men")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_sigma_w, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w, sex == "Men")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_sigma, sex == "Men"), aes(x = 2010, y = 4.5, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_sigma_w, sex == "Men"), aes(x = 2010, y = 3.5, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Year") +
  ylab("Variance - model of trend") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  ggtitle("Changes in the variance trend")
  

p4 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit, sex == "Women"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w, sex == "Women"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_sigma, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit, sex == "Women")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_sigma_w, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w, sex == "Women")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_sigma, sex == "Women"), aes(x = 2010, y = 2, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_sigma_w, sex == "Women"), aes(x = 2010, y = 1.7, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Year") +
  ylab("") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()

fig3 <- plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2, align = "hv", axis = "tblr", labels = NA)
ggsave('figures/fig3.eps', fig3, scale = 1, width = 8, height = 8, units = "in", device='eps', dpi=700)
ggsave('figures/fig3.png', fig3, scale = 1, width = 8, height = 8, units = "in", device='png', dpi=100)


## Supplementary figure for Theil index

pA <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year, y = theil, colour = "Unweighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year, y = theil, ymin = theil_l, ymax = theil_u, colour = "Unweighted"))+
  geom_line(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year+0.1, y = theil_w, colour = "Population weighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Men"), aes(x = year+0.1, y = theil_w, ymin = theil_w_l, ymax = theil_w_u, colour = "Population weighted"))+
  scale_colour_manual(values = c("Unweighted" = "black", "Population weighted" = "gray"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = comma) +
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_wrap(. ~ sex, scales = "free_y")+
  theme(legend.position = "top", legend.direction = "horizontal", legend.title=element_blank())+
  xlab("Year")+
  ylab("Theil index (95% bootstrap confidence interval)")+
  ggtitle("Theil index of life expectancy")

pB <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year, y = theil, colour = "Unweighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year, y = theil, ymin = theil_l, ymax = theil_u, colour = "Unweighted"))+
  geom_line(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year+0.1, y = theil_w, colour = "Population weighted"))+
  geom_pointrange(data = filter(dispersion_national_ci, sex == "Women"), aes(x = year+0.1, y = theil_w, ymin = theil_w_l, ymax = theil_w_u, colour = "Population weighted"))+
  scale_colour_manual(values = c("Unweighted" = "black", "Population weighted" = "gray"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = comma) +
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  facet_wrap(. ~ sex, scales = "free_y")+
  theme(legend.position = "none")+
  xlab("Year")+
  ylab("")

segmented_sigma_t <- dispersion_national_ci %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$theil, se = .x$theil_se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_t <- segmented_sigma_t %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)


segmented_sigma_w_t <- dispersion_national_ci %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$theil_w, se = .x$theil_w_se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_w_t <- segmented_sigma_w_t %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)

pC <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit_t, sex == "Men"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w_t, sex == "Men"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_sigma_t, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_t, sex == "Men")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_sigma_w_t, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w_t, sex == "Men")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_sigma_t, sex == "Men"), aes(x = 2010, y = 0.002, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_sigma_w_t, sex == "Men"), aes(x = 2010, y = 0.0019, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Year") +
  ylab("Theil index - model of trend") +
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  ggtitle("Changes in the Theil index trend")

pD <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit_t, sex == "Women"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w_t, sex == "Women"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_sigma_t, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_t, sex == "Women")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_sigma_w_t, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w_t, sex == "Women")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_sigma_t, sex == "Women"), aes(x = 2010, y = 0.00048, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_sigma_w_t, sex == "Women"), aes(x = 2010, y = 0.00046, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Year") +
  ylab("") +
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()

supp_fig2 <- plot_grid(pA, pB, pC, pD, nrow = 2, ncol =2, align = "hv", axis = "tblr", labels = NA)
ggsave('figures/supp_fig2.eps', supp_fig2, scale = 1, width = 9, height = 8, units = "in", device='eps', dpi=700)
ggsave('figures/supp_fig2.png', supp_fig2, scale = 1, width = 9, height = 8, units = "in", device='png', dpi=300)

## Decomposition

theil_d <- national_data %>%
  group_by(year, sex) %>%
  do(onestage.Theil.decomp(df = ., macroregion = "NMS", ple = "ple")) %>%
  select(-Additive) %>%
  pivot_longer(cols = Tbetween:Ttotal, names_to = "Component", values_to = "dispersion") %>%
  mutate(Measure = "Theil index",
         Component = case_when(Component == "Tbetween" ~ "Between",
                               Component == "Twithin" ~ "Within",
                               Component == "Ttotal" ~ "Total"),
         Component = factor(Component, levels = c("Total", "Between", "Within")))

var_d <- national_data %>%
  group_by(year, sex) %>%
  do(onestage.var.decomp(df = ., macroregion = "NMS", ple = "ple"))  %>%
  select(-Vartotal) %>%
  pivot_longer(cols = Varbetween:Additive, names_to = "Component", values_to = "dispersion") %>%
  mutate(Measure = "Variance",
         Weighted = "Unweighted",
         dispersion = dispersion,
         Component = case_when(Component == "Varbetween" ~ "Between",
                               Component == "Varwithin" ~ "Within",
                               Component == "Additive" ~ "Total"),
         Component = factor(Component, levels = c("Total", "Between", "Within")))

theil_d_w <- national_data %>%
  group_by(year, sex) %>%
  do(onestage.Theil.decomp.w(df = ., macroregion = "NMS", ple = "ple", pop = "pop")) %>%
  select(-Additive) %>%
  pivot_longer(cols = Tbetween:Ttotal, names_to = "Component", values_to = "dispersion") %>%
  mutate(Measure = "Theil index",
         Component = case_when(Component == "Tbetween" ~ "Between",
                               Component == "Twithin" ~ "Within",
                               Component == "Ttotal" ~ "Total"),
         Component = factor(Component, levels = c("Total", "Between", "Within")))

var_d_w <- national_data %>%
  group_by(year, sex) %>%
  do(onestage.var.decomp.w(df = ., macroregion = "NMS", ple = "ple", pop = "pop")) %>%
  select(-Additive) %>%
  pivot_longer(cols = Varbetween:Vartotal, names_to = "Component", values_to = "dispersion") %>%
  mutate(Measure = "Variance",
         Weighted = "Population weighted",
         dispersion = dispersion,
         Component = case_when(Component == "Varbetween" ~ "Between",
                               Component == "Varwithin" ~ "Within",
                               Component == "Vartotal" ~ "Total"),
         Component = factor(Component, levels = c("Total", "Between", "Within")))

decomposed_variance<- rbind(var_d, var_d_w) %>%
  mutate(Weighted = factor(Weighted, levels = c("Unweighted", "Population weighted")))
