# Impact of European integration on life expectancy convergence
# Rok Hrzic (r.hrzic (at) maastrichtuniversity.nl)
# Version April 2021

require(dplyr)
require(ggplot2)
require(tidyr)
require(purrr)
require(ggtext)

#Helper functions for extracting regression results

unconditional_beta <- function(data){lm(e0_diff ~ e0_start, data = data)}
weighted_beta <- function(data){lm(e0_diff ~ e0_start, weights = pop, data = data)}
extract_intercept <- function(mod){coef(summary(mod))[1,1]}
extract_coeff <- function(mod){coef(summary(mod))[2,1]}
extract_se <- function(mod){coef(summary(mod))[2,2]}
extract_CI <- function(mod){round(confint(mod, "e0_start", level = 0.95),3)}
extract_p <- function(mod){round(coef(summary(mod))[2,4], 5)}
extract_adjr2 <- function(mod){round(summary(mod)$adj.r.squared, 3)}


# Beta convergence at country level for the entire period (1990-2017), unweighted version

beta_convergence_national_overall <- national_data %>%
  filter(year %in% c(1990,2017)) %>%
  group_by(sex, country, NMS) %>%
  summarise(start_year=year,
            pop = first(pop),
            lag_year = lead(year),
            year = paste0(year, " - ", lag_year),
            ple = ple, 
            e0_diff_1 = (lead(ple)-ple),
            e0_diff = (lead(ple)-ple)/(lag_year-start_year),
            e0_start = ple) %>%
  filter(!is.na(e0_diff))


beta_models_national_overall <- beta_convergence_national_overall %>%
  group_by(sex, start_year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         intercept = map(models, extract_intercept),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         CI = map(models, extract_CI),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(intercept, beta, se, pval, adjr2))%>%
  select(sex, start_year, intercept, beta, se, CI, pval, adjr2)

# Beta convergence at country level for the entire period (1990-2017), population weighted version

beta_models_national_overall_w <- beta_convergence_national_overall %>%
  group_by(sex, start_year) %>%
  nest() %>%
  mutate(models = map(data, weighted_beta),
         intercept = map(models, extract_intercept),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         CI = map(models, extract_CI),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(intercept, beta, se, pval, adjr2))%>%
  select(sex, start_year, intercept, beta, se, CI, pval, adjr2)

p1 <- filter(beta_convergence_national_overall, sex == "Men") %>%
  ggplot(aes(x = e0_start, y=e0_diff))+
  geom_text(aes(label = country), position=position_jitter(width = 0.3), size = 3)+
  geom_abline(aes(slope = filter(beta_models_national_overall, sex == "Men")$beta, intercept = filter(beta_models_national_overall, sex == "Men")$intercept, colour = "Unweighted regression"), key_glyph = "path")+
  geom_abline(aes(slope = filter(beta_models_national_overall_w, sex == "Men")$beta, intercept = filter(beta_models_national_overall_w, sex == "Men")$intercept, colour = "Population weighted regression"), key_glyph = "path")+
  annotate("text", x = 71, y = 0.34, label = paste0("beta = -0.004 (-0.009, 0.002), adj. R\u00b2 = 0.031 "), size = 3)+
  annotate("text", x = 71, y = 0.33, label = paste0("beta = -0.005 (-0.009, -0.001), adj. R\u00b2 = 0.217 "), size = 3, color = "gray")+
  facet_wrap(sex ~ ., scales = "free")+
  theme_bw()+
  scale_colour_manual(values = c("Unweighted regression" = "black", "Population weighted regression" = "gray"))+
  theme(legend.position = "top", legend.key = )+
  labs(x = "Life expectancy in 1990",
       y = "Annual change in life expectancy 1990-2017",
       color = "",
       title = "Overall beta convergence")

p2 <- filter(beta_convergence_national_overall, sex == "Women") %>%
  ggplot(aes(x = e0_start, y=e0_diff))+
  geom_text(aes(label = country), position=position_jitter(width = 0.3), size = 3)+
  geom_abline(aes(slope = filter(beta_models_national_overall, sex == "Women")$beta, intercept = filter(beta_models_national_overall, sex == "Women")$intercept, colour = "Unweighted regression"))+
  geom_abline(aes(slope = filter(beta_models_national_overall_w, sex == "Women")$beta, intercept = filter(beta_models_national_overall_w, sex == "Women")$intercept, colour = "Population weighted regression"))+
  annotate("text", x = 78.5, y = 0.277, label = paste0("beta = -0.01 (-0.016, -0.004), adj. R\u00b2 = 0.317 "), size = 3)+
  annotate("text", x = 78.5, y = 0.27, label = paste0("beta = -0.01 (-0.014, -0.005), adj. R\u00b2 = 0.443 "), size = 3, color = "gray")+
  facet_wrap(sex ~ ., scales = "free")+
  theme_bw()+
  scale_colour_manual(values = c("Unweighted regression" = "black", "Population weighted regression" = "gray"))+
  theme(legend.position = "none")+
  labs(x = "Life expectancy in 1990",
       y = "")

#Beta convergence at country level for 4-year periods, unweighted

beta_convergence_national <- national_data %>%
  group_by(country, NMS, sex) %>%
  arrange(year, .by_group = T) %>%
  summarise(start_year=year,
            pop = pop,
            lag_year = lead(year, 4),
            year = paste0(year, " - ", lag_year),
            ple = ple, 
            e0_diff_1 = (lead(ple, 4)-ple),
            e0_diff = (lead(ple, 4)-ple)/(lag_year-start_year),
            e0_start = ple) %>%
  filter(!is.na(e0_diff))

beta_models_national <- beta_convergence_national %>%
  group_by(sex, start_year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, start_year, beta, se, pval, adjr2)

#Beta convergence at country level for 4-year periods, population weighted

beta_models_national_w <- beta_convergence_national %>%
  group_by(sex, start_year) %>%
  nest() %>%
  mutate(models = map(data, weighted_beta),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, start_year, beta, se, pval, adjr2)


p3 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(beta_models_national, sex == "Men"), aes(x = start_year, y = beta))+
  geom_pointrange(data = filter(beta_models_national, sex == "Men"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se))+
  geom_line(data = filter(beta_models_national_w, sex == "Men"), aes(x = start_year, y = beta), color = "gray")+
  geom_pointrange(data=filter(beta_models_national_w, sex == "Men"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se), color = "gray")+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (95% confidence interval)") +
  annotate("text", x = 2004.1, y = 0.05, label = "Short-term\naccession\neffects", hjust = "left", size = 2.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  theme(legend.position = "none")+
  ggtitle("Trend in 4-year beta convergence")

p4 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(beta_models_national, sex == "Women"), aes(x = start_year, y = beta))+
  geom_pointrange(data = filter(beta_models_national, sex == "Women"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se))+
  geom_line(data = filter(beta_models_national_w, sex == "Women"), aes(x = start_year, y = beta), color = "gray")+
  geom_pointrange(data=filter(beta_models_national_w, sex == "Women"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se), color = "gray")+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  theme(legend.position = "none")


# Segmented regression to detect significant break points, helper functions to extract and visualise results

source("analysis/segmented.R")

extract_fit <- function(mod){broken.line(mod)$fit}
extract_psi <- function(mod){round(mod$psi[,2],2)+1989}
extract_psi_l <- function(mod){round(confint.segmented(mod, method = "delta")[,2],2)+1989}
extract_psi_u <- function(mod){round(confint.segmented(mod, method = "delta")[,3],2)+1989}
extract_r2 <- function(mod){round(1 - (summary(mod)$deviance/summary(mod)$null.deviance), 3)}

# Segmented regression to detect significant break points, unweighted

segmented_beta <- beta_models_national %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$beta, se = .x$se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit <- segmented_beta %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)
  
# Segmented regression to detect significant break points, unweighted


segmented_beta_w <- beta_models_national_w %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$beta, se = .x$se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_w <- segmented_beta_w %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)

p5 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit, sex == "Men"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w, sex == "Men"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_beta, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit, sex == "Men")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_beta_w, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w, sex == "Men")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_beta, sex == "Men"), aes(x = 2010, y = 0.056, label = paste0("R\u00b2 = ",r2), colour = "Unweighted regression"),  size = 3, key_glyph = "rect")+
  geom_text(data = filter(segmented_beta_w, sex == "Men"), aes(x = 2010, y = 0.049, label = paste0("R\u00b2 = ",r2), colour = "Population weighted regression",),  size = 3)+
  facet_grid(. ~ sex)+
  labs(x = "Starting year",
       y = "Beta coefficient - model of trend",
       color = "")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_colour_manual(values = c("Unweighted regression" = "black", "Population weighted regression" = "gray"))+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("Changes in 4-year beta convergence trend")

p6 <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit, sex == "Women"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w, sex == "Women"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_beta, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit, sex == "Women")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_beta_w, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w, sex == "Women")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_beta, sex == "Women"), aes(x = 2010, y = 0.053, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_beta_w, sex == "Women"), aes(x = 2010, y = 0.047, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  theme(legend.position = "none")


fig2 <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2, align = "hv", axis = "tblr", labels = NA)
ggsave('figures/fig2.eps', fig2, scale = 1.3, width = 8, height = 10, units = "in", device='eps', dpi=700)
ggsave('figures/fig2.png', fig2, scale = 1.3, width = 8, height = 10, units = "in", device='png', dpi=200)

## Sensitivity analysis - different time periods (6 year intervals), unweighted beta convergence

beta_convergence_national_six <- national_data %>%
  group_by(country, NMS, sex) %>%
  arrange(year, .by_group = T) %>%
  summarise(start_year=year,
            pop = pop,
            lag_year = lead(year, 6),
            year = paste0(year, " - ", lag_year),
            ple = ple, 
            e0_diff_1 = (lead(ple, 6)-ple),
            e0_diff = (lead(ple, 6)-ple)/(lag_year-start_year),
            e0_start = ple) %>%
  filter(!is.na(e0_diff))


beta_models_national_six <- beta_convergence_national_six %>%
  group_by(sex, start_year) %>%
  nest() %>%
  mutate(models = map(data, unconditional_beta),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, start_year, beta, se, pval, adjr2)

## Sensitivity analysis - different time periods (6 year intervals), population weighted beta convergence


beta_models_national_w_six <- beta_convergence_national_six %>%
  group_by(sex, start_year) %>%
  nest() %>%
  mutate(models = map(data, weighted_beta),
         beta = map(models, extract_coeff),
         se = map(models, extract_se),
         pval = map(models, extract_p),
         adjr2=map(models, extract_adjr2)) %>%
  unnest(cols = c(beta, se, pval, adjr2))%>%
  select(sex, start_year, beta, se, pval, adjr2)


pA <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(beta_models_national_six, sex == "Men"), aes(x = start_year, y = beta, colour = "Unweighted regression"))+
  geom_pointrange(data = filter(beta_models_national_six, sex == "Men"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se, colour = "Unweighted regression"))+
  geom_line(data = filter(beta_models_national_w_six, sex == "Men"), aes(x = start_year, y = beta, colour = "Population weighted regression"), color = "gray")+
  geom_pointrange(data=filter(beta_models_national_w_six, sex == "Men"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se, colour = "Population weighted regression"))+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (95% confidence interval)") +
  annotate("text", x = 2004.1, y = 0.025, label = "Short-term\naccession\neffects", hjust = "left", size = 2.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_colour_manual(values = c("Unweighted regression" = "black", "Population weighted regression" = "gray"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank())+
  ggtitle("Trend in 6-year beta convergence")

pB <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(beta_models_national_six, sex == "Women"), aes(x = start_year, y = beta))+
  geom_pointrange(data = filter(beta_models_national_six, sex == "Women"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se))+
  geom_line(data = filter(beta_models_national_w_six, sex == "Women"), aes(x = start_year, y = beta), color = "gray")+
  geom_pointrange(data=filter(beta_models_national_w_six, sex == "Women"), aes(x = start_year, y = beta, ymin = beta-1.96*se, ymax = beta+1.96*se), color = "gray")+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (95% confidence interval)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  scale_colour_grey() +  
  theme(legend.position = "none")

## Sensitivity analysis - different time periods (6 year intervals), segmented regression of coefficients from the unweighted beta convergence analysis

source("analysis/segmented.R")

extract_fit <- function(mod){broken.line(mod)$fit}
extract_psi <- function(mod){round(mod$psi[,2],2)+1989}
extract_psi_l <- function(mod){round(confint.segmented(mod, method = "delta")[,2],2)+1989}
extract_psi_u <- function(mod){round(confint.segmented(mod, method = "delta")[,3],2)+1989}
extract_r2 <- function(mod){round(1 - (summary(mod)$deviance/summary(mod)$null.deviance), 3)}

segmented_beta_six <- beta_models_national_six %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$beta, se = .x$se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_six <- segmented_beta_six %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)


## Sensitivity analysis - different time periods (6 year intervals), segmented regression of coefficients from the population weighted beta convergence analysis

segmented_beta_w_six <- beta_models_national_w_six %>%
  group_by(sex) %>%
  nest() %>%
  mutate(segmented_model = map(.x = data, ~segmented_wrapper(y = .x$beta, se = .x$se)),
         psi = map(segmented_model, extract_psi),
         psi_l = map(segmented_model, extract_psi_l),
         psi_u = map(segmented_model, extract_psi_u),
         r2 = map(segmented_model, extract_r2)) %>%
  unnest(cols = c(sex, psi, psi_l, psi_u, r2))

fit_w_six <- segmented_beta_w_six %>%
  group_by(sex) %>%
  summarise(y_fit = first(map(segmented_model, extract_fit)),
            x = 1:length(y_fit)+1989)

pC <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit_six, sex == "Men"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w_six, sex == "Men"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_beta_six, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_six, sex == "Men")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_beta_w_six, sex == "Men"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w_six, sex == "Men")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_beta_six, sex == "Men"), aes(x = 1995, y = 0.025, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_beta_w_six, sex == "Men"), aes(x = 1995, y = 0.020, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (95% confidence interval)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()+
  ggtitle("Changes in 6-year beta convergence trend")

pD <- ggplot()+
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf), color = "gray95", fill = "gray95") +
  geom_line(data = filter(fit_six, sex == "Women"), aes(x = x, y = y_fit), size = 1)+
  geom_line(data = filter(fit_w_six, sex == "Women"), aes(x = x, y = y_fit), color = "gray", size = 1)+
  geom_pointrange(data = filter(segmented_beta_six, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_six, sex == "Women")$y_fit[round(psi,0)-1989]), size = 1.2)+
  geom_pointrange(data = filter(segmented_beta_w_six, sex == "Women"), aes(x = psi, xmax = psi_u, xmin = psi_l,  y=filter(fit_w_six, sex == "Women")$y_fit[round(psi,0)-1989]), color = "gray", size = 1.2)+
  geom_text(data = filter(segmented_beta_six, sex == "Women"), aes(x = 1995, y = 0.025, label = paste0("R\u00b2 = ",r2)), color = "black", size = 3)+
  geom_text(data = filter(segmented_beta_w_six, sex == "Women"), aes(x = 1995, y = 0.020, label = paste0("R\u00b2 = ",r2)), color = "gray", size = 3)+
  facet_grid(. ~ sex)+
  xlab("Starting year") +
  ylab("Beta coefficient (95% confidence interval)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_bw()

supp_fig1 <- plot_grid(pA, pB, pC, pD, nrow = 2, ncol = 2, align = "hv", axis = "tblr", labels = NA)
ggsave('figures/supp_fig1.eps', supp_fig1, scale = 1.3, width = 8, height = 8, units = "in", device='eps', dpi=700)
ggsave('figures/supp_fig1.png', supp_fig1, scale = 1.3, width = 8, height = 8, units = "in", device='png', dpi=300)

