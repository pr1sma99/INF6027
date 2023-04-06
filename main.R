# ------------------------------------------------------------------
# Include packages and libraries
library(readr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(showtext)
library(zoo)
library(tseries)
library(forecast)

# Load fonts from Google
font_add_google("Roboto Mono")
font_add_google("Roboto Slab")
font_add_google("Open Sans")
font_add_google("Lato")
showtext_auto()


# ------------------------------------------------------------------
# Load data from table
df_covid <- read_csv("owid-covid-data.csv") %>% select(-tests_units)

# Summary statistics
summary(df_covid)

# Summarise Europe
df_europe_major <- df_covid %>%
  subset(continent == 'Europe' &
           iso_code %in% c('FRA', 'DEU', 'GBR', 'ITA', 'RUS', 'ESP'))

# Add timeline
mutations <- tribble(
  ~ date,
  ~ event,
  "2020-03-11",
  "Pandemic\ndeclared",
  "2020-10-31",
  "Delta",
  "2021-11-26",
  "Omicron",
) %>%
  mutate(date = as.Date(date))

# Visualise Europe
df_europe_major %>% ggplot() +
  geom_line(aes(x = date, y = total_cases, color = location), lwd = 0.75) +
  labs(x = "", y = 'total cases #') +
  scale_x_date(date_breaks = '3 months', date_labels = '%b %Y') +
  geom_vline(aes(xintercept = date),
             mutations,
             linetype = "longdash",
             alpha = 0.5) +
  geom_text(
    aes(x = date, label = event, y = 20000000),
    mutations,
    family = "Roboto Slab",
    alpha = 0.75
  ) +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_tableau()


# ------------------------------------------------------------------
# Find correlation

# Generate overall correlation matrix
df_numeric <- select_if(df_covid, is.numeric)
cor_matrix <-
  round(cor(df_numeric, use = 'pairwise.complete.obs'), 2)
View(cor_matrix)

# Generate correlation matrix for new deaths
death_regressor <- df_numeric %>%
  select(names(sort(cor_matrix[, 'new_deaths'][cor_matrix[, 'new_deaths'] > 0.5],
                    decreasing = TRUE)))
death_cor <-
  round(cor(death_regressor, use = 'pairwise.complete.obs'), 2)

# Correlation heatmap
ggplot(data = melt(death_cor), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "Roboto Slab")) +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  )


# ------------------------------------------------------------------
# Multilinear regression on daily death

# Sort indicators by correlation
names(sort(abs(cor_matrix[, 'new_deaths']), decreasing = TRUE))

# Select regressor
df_death <- df_covid %>%
  select(iso_code,
         date,
         new_deaths,
         hosp_patients,
         population,
         new_cases,
         total_cases) %>%
  drop_na()

# Fit model
death_model <-
  lm(new_deaths ~ hosp_patients + population + new_cases + total_cases,
     data = df_death)

# Model diagnostics
vif(death_model)
summary(death_model)
plot(death_model)

# Model prediction
df_death <- df_death %>%
  bind_cols(predict(death_model, df_death, interval = "predict", level = 0.80))

# Select UK deaths
death_uk <- df_death %>%
  subset(iso_code == 'GBR') %>%
  drop_na()

# Visualise UK deaths
death_uk %>% ggplot(aes(x = date)) +
  geom_line(aes(y = new_deaths, color = 'actual'), alpha = 0.75) +
  geom_line(aes(y = fit, color = 'predict'), lwd = 0.75) +
  geom_line(aes(y = lwr, color = '80% interval'), lty = 'dotted') +
  geom_line(aes(y = upr, color = '80% interval'), lty = 'dotted') +
  labs(x = "", y = 'new death #') +
  scale_x_date(date_labels = '%b %Y') +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_tableau()


# ------------------------------------------------------------------
# UK analysis

# Select UK
df_uk <- subset(df_covid, iso_code == 'GBR')

# Generate monthly data
total_cases_monthly_uk <- df_uk %>%
  subset(date > as.Date('2020-01-31') &
           date < as.Date('2022-07-01')) %>%
  group_by(format(as.Date(date), "%Y %m")) %>%
  summarise(monthly_cases = mean(total_cases, na.rm = TRUE)) %>%
  select(monthly_cases) %>%
  ts(frequency = 12, start = c(2020, 02))

new_cases_monthly_uk <- df_uk %>%
  subset(date > as.Date('2020-01-31') &
           date < as.Date('2022-07-01')) %>%
  group_by(format(as.Date(date), "%Y %m")) %>%
  summarise(monthly_cases = sum(new_cases, na.rm = TRUE)) %>%
  select(monthly_cases) %>%
  ts(frequency = 12, start = c(2020, 02))

# Visualise monthly data
# Total cases UK
autoplot(total_cases_monthly_uk, lwd = 0.5) +
  aes(col = 'actual') +
  geom_col(
    col = 'transparent',
    fill = time(monthly_cases_uk),
    width = 0.05,
    alpha = 0.2
  ) +
  geom_point(col = 'lightblue4') +
  geom_smooth(aes(col = 'smooth'),
              lwd = 0.75,
              se = FALSE) +
  labs(x = "", y = 'total cases by month #') +
  scale_x_yearmon(n = 10) +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_tableau()

# New cases UK
autoplot(new_cases_monthly_uk,
         color = 'lightpink2',
         lwd = 0.75) +
  geom_col(
    col = 'transparent',
    fill = time(monthly_cases_uk),
    width = 0.05,
    alpha = 0.2
  ) +
  geom_point(col = 'lightblue4') +
  geom_vline(aes(xintercept = date),
             mutations,
             linetype = "longdash",
             alpha = 0.5) +
  geom_text(aes(x = date, label = event, y = 2000000),
            mutations,
            family = "Roboto Slab",
            alpha = 0.75) +
  labs(x = "", y = 'new cases by month #') +
  scale_x_yearmon(n = 10) +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_hc()


# ------------------------------------------------------------------
# ARIMA on UK total cases

# Generate time series
total_cases_uk <- df_uk %>%
  select(total_cases) %>%
  drop_na() %>%
  ts(frequency = 365,
     start = c(2020, 01, 31))

# Plot decomposed ts
autoplot(decompose(total_cases_uk, type = 'additive'), main = '') +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab"))

# Plot differenced ts and P/ACF
total_cases_uk %>%
  diff(2) %>%
  ggtsdisplay(lag.max = 30, theme = theme_hc())

# Stationarity test
adf.test(diff(total_cases_uk, 2))
kpss.test(diff(total_cases_uk, 2))

# Fit model and plot residuals
case_model <- auto.arima(total_cases_uk)
summary(case_model)
checkresiduals(case_model)

# Plot forecast result
autoplot(
  forecast(case_model, h = 200),
  fcol = 'red',
  flwd = 0.75,
  main = ''
) +
  labs(x = "", y = 'total cases #') +
  scale_x_yearmon() +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------------
# ARIMA on UK reproduction rate

# Select and convert into ts
rep_rate_uk <- df_uk %>%
  select(reproduction_rate) %>%
  drop_na() %>%
  ts(frequency = 365,
     start = c(2020, 03, 03))

# Plot ts
autoplot(rep_rate_uk) +
  geom_hline(yintercept = mean(rep_rate_uk), lty = 'longdash') +
  labs(x = "", y = 'reproduction number R0') +
  scale_x_yearmon() +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stationarity test
adf.test(diff(rep_rate_uk, 2))
kpss.test(diff(rep_rate_uk, 2))

# P/ACF
rep_rate_uk %>%
  diff(2) %>%
  ggtsdisplay(lag.max = 30, theme = theme_hc())

# Decomposition
autoplot(decompose(rep_rate_uk, type = 'additive'), main = '') +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab"))

# Fit model
rep_model <- auto.arima(rep_rate_uk)
summary(rep_model)
checkresiduals(rep_model)

# Plot forecast
autoplot(
  forecast(rep_model, h = 30),
  fcol = 'red',
  flwd = 0.75,
  main = ''
) +
  labs(x = "", y = 'reproduction number R0') +
  scale_x_yearmon() +
  theme_hc() +
  theme(text = element_text(family = "Roboto Slab")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
