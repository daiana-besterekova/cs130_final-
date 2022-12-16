library(here)
library(gghighlight)
library(tidysynth)
library(stargazer)
library(tidyverse)
library(tidylog)
library(foreach)
library(augsynth)
library(furrr)

# This script pulls raw data directly from online sources and saves to the 
# data-raw/ directory. 
# This file pulls vaccination rate and case data. 
# Note: auxillary covariate data is obtained in the python/data_processing.py

library(readr)
library(here)

# Vaccination Data --------------------------------------------------------

us_vaccines <- read_csv("https://raw.githubusercontent.com/williamlief/synth_vax/main/data-raw/us_vaccines_2021-09-12.csv")
write_csv(us_vaccines, here(paste0("data-raw/us_vaccines_1", Sys.Date(), ".csv")))


# Case Data ---------------------------------------------------------------


us_cases <- read_csv("https://raw.githubusercontent.com/williamlief/synth_vax/main/data-raw/us_cases_2021-09-12.csv")
write_csv(us_cases, here(paste0("data-raw/us_cases_1", Sys.Date(), ".csv")))

date <- "2022-12-15"

fips <- read_csv(here::here("data-raw/fips.csv"))
raw_cases <- read_csv(here::here(str_glue("data-raw/us_cases_1{date}.csv")))
raw_vax <- read_csv(here::here(str_glue("data-raw/us_vaccines_1{date}.csv")))

# Lottery Announrawcement Date
announce_date = mdy("05/12/2021")
cases <- raw_cases %>% 
  mutate(day = mdy(submission_date)) %>% 
  tidylog::inner_join(fips %>% select(abb, fips),
             by = c("state" = "abb"))

vax <- raw_vax %>% 
  mutate(day = ymd(date), 
         location = recode(location, "New York State" = "New York")) %>% 
  tidylog::inner_join(fips %>% select(state, fips),
             by = c("location" = "state"))

daily <- cases %>% 
  inner_join(vax, by = c("fips", "day")) %>% 
  mutate(centered_time = day - announce_date, 
         # use vax per million rate to convert cases to per millions
         pop_pmr = daily_vaccinations / daily_vaccinations_per_million, 
         across(c(tot_cases, new_case, tot_death, new_death), 
                function(x) x / pop_pmr, .names = "{.col}_per_million"))

# Note: all data is within 2021, so don't need to worry about years
weekly <- daily %>% 
  mutate(week = isoweek(day)) %>% 
  group_by(state, fips, week) %>% 
  summarize(across(c(new_case_per_million, new_death_per_million, 
                     daily_vaccinations_per_million), 
                   sum, na.rm = T), 
            across(c(tot_cases_per_million, tot_death_per_million, 
                     people_fully_vaccinated_per_hundred,
                     total_vaccinations_per_hundred,
                     people_vaccinated_per_hundred), 
                   max, na.rm = T),
            n_rec = n(), 
            first_day = min(day), last_day = max(day),
            .groups = "drop") %>% 
  mutate(centered_week = week - isoweek(announce_date))

write_rds(daily, str_glue(here("data/daily_data_1{date}.rds")))
write_rds(weekly, str_glue(here("data/weekly_data_1{date}.rds")))

library(here)
library(tidyverse)
library(tidysynth)
library(stargazer)
library(augsynth)

dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 

# Train Synthetic Control Model
vaccine_out <-
  dat  %>%
  # initial the synthetic control object
  synthetic_control(outcome = people_fully_vaccinated_per_hundred, # outcome
                    unit = state, # unit index in the panel data
                    time = centered_week, # time index in the panel data
                    i_unit = "CA", # unit where the intervention occurred
                    i_time = 0, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  # Matching on fully vaccinated the weeks before the intervention  
  generate_predictor(time_window = -17, lagged_vaccinations_week17 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -16, lagged_vaccinations_week16 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -15, lagged_vaccinations_week15 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -14, lagged_vaccinations_week14 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -13, lagged_vaccinations_week13 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -12, lagged_vaccinations_week12 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -11, lagged_vaccinations_week11 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -10, lagged_vaccinations_week10 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -09, lagged_vaccinations_week09 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -08, lagged_vaccinations_week08 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -07, lagged_vaccinations_week07 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -06, lagged_vaccinations_week06 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -05, lagged_vaccinations_week05 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -04, lagged_vaccinations_week04 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -03, lagged_vaccinations_week03 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -02, lagged_vaccinations_week02 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -01, lagged_vaccinations_week01 = people_fully_vaccinated_per_hundred) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = -17:-1, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()

# NOTE: instead of loading the pre-registered weights we simply rerun the model
# this results in the exact same weights since the weights are based only on 
# pre-treatment data. 
# Confirm in table below that weights are identical to three decimal places to 
# the pre-registered weights.
vaccine_out %>%
  grab_unit_weights() %>%
  mutate(weights = round(weight, digits = 4)) %>%
  select(unit, weights) %>%
  filter(weights>0.0001) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE)

vaccine_out %>% plot_weights() + 
  labs(title="Synthetic Control Weights")   
# Note: not included in paper writeup
ggsave(here("figures/weights.jpg"), 
       bg = "white", width = 7, height = 7)

# Balance Table
vaccine_out %>%
  grab_balance_table() %>%
  mutate(difference = CA - synthetic_CA) %>%
  select(variable, CA, synthetic_CA, difference, donor_sample) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE, 
            caption = "Balance Table", 
            label = "balancetable")

# Plot Model Trends
vaccine_out %>% plot_trends() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "California and Synthetic California",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  ) 
# NOTE: not included in paper writeup
ggsave("figures/treatment_trends.jpg", 
       bg = "white", width = 7, height = 7)

# Plot Model Differences
vaccine_out %>% plot_differences() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "Difference between California and Synthetic California",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Difference in Percent Fully Vaccinated"
  ) 
ggsave("figures/treatment_differences.jpg", 
       bg = "white", width = 7, height = 7)



