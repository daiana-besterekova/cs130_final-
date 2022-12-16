library(here)
library(gghighlight)
library(tidysynth)
library(stargazer)
library(tidyverse)
library(tidylog)
library(foreach)
library(augsynth)
library(furrr)

library(readr)
library(here)

# Vaccination Data --------------------------------------------------------

us_vaccines <- read_csv("https://raw.githubusercontent.com/williamlief/synth_vax/main/data-raw/us_vaccines_2021-09-12.csv")
write_csv(us_vaccines, here(paste0("data-raw/us_vaccines_", Sys.Date(), ".csv")))


# Case Data ---------------------------------------------------------------


us_cases <- read_csv("https://raw.githubusercontent.com/williamlief/synth_vax/main/data-raw/us_cases_2021-09-12.csv")
write_csv(us_cases, here(paste0("data-raw/us_cases_", Sys.Date(), ".csv")))

date <- "2022-12-15"

fips <- read_csv(here::here("data-raw/fips.csv"))
raw_cases <- read_csv(here::here(str_glue("data-raw/us_cases_{date}.csv")))
raw_vax <- read_csv(here::here(str_glue("data-raw/us_vaccines_{date}.csv")))

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

write_rds(daily, str_glue(here("data/daily_data_{date}.rds")))
write_rds(weekly, str_glue(here("data/weekly_data_{date}.rds")))

library(augsynth)

dat <- readRDS(here("data/weekly_data_2021-06-24.rds")) 

# Train Synthetic Control Model
vaccine_out <-
  dat  %>%
  # initial the synthetic control object
  synthetic_control(outcome = people_fully_vaccinated_per_hundred, # outcome
                    unit = state, # unit index in the panel data
                    time = centered_week, # time index in the panel data
                    i_unit = "OH", # unit where the intervention occurred
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
  mutate(difference = OH - synthetic_OH) %>%
  select(variable, OH, synthetic_OH, difference, donor_sample) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE, 
            caption = "Balance Table", 
            label = "balancetable")

# Plot Model Trends
vaccine_out %>% plot_trends() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "Ohio and Synthetic Ohio",
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
    title = "Difference between Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Difference in Percent Fully Vaccinated"
  ) 
ggsave("figures/treatment_differences.jpg", 
       bg = "white", width = 7, height = 7)






# Placebo Test ------------------------------------------------------------
# This test shifts the pre-treatment window back five weeks.
# This analysis was included in our pre-registration as a demonstration of the 
# method and to show that we did not find treatment effects before the lottery 
# was announced. 

placebo_out <-
  dat %>% 
  filter(centered_week <= 0) %>% 
  # initial the synthetic control object
  synthetic_control(outcome = people_fully_vaccinated_per_hundred, # outcome
                    unit = state, # unit index in the panel data
                    time = centered_week, # time index in the panel data
                    i_unit = "OH", # unit where the intervention occurred
                    i_time = -5, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
                    ) %>%
# Matching on fully vaccination the weeks before the intervention  
  generate_predictor(time_window = -17, people_fully_vaccinated_per_hundred17 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -16, people_fully_vaccinated_per_hundred16 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -15, people_fully_vaccinated_per_hundred15 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -14, people_fully_vaccinated_per_hundred14 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -13, people_fully_vaccinated_per_hundred13 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -12, people_fully_vaccinated_per_hundred12 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -11, people_fully_vaccinated_per_hundred11 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -10, people_fully_vaccinated_per_hundred10 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -09, people_fully_vaccinated_per_hundred09 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -08, people_fully_vaccinated_per_hundred08 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -07, people_fully_vaccinated_per_hundred07 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -06, people_fully_vaccinated_per_hundred06 = people_fully_vaccinated_per_hundred) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = -17:-6, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()


placebo_out %>% plot_trends()  + 
  labs(
    title = "Placebo Analysis: Ohio and Synthetic Ohio",
    caption = "Timing of The Placebo Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  )
ggsave(here("figures/placebo_analysis.jpg"), 
       bg = "white", width = 7, height = 7)


placebo_out %>% plot_differences()  + 
  labs(
    title = "Placebo Analysis:  Difference between Ohio and Synthetic Ohio",
    caption = "Timing of The Placebo Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  )
ggsave(here("figures/placebo_difference.jpg"), 
       bg = "white", width = 7, height = 7)

placebo_out %>% grab_signficance() %>% filter(unit_name=="OH")
placebo_out %>% grab_unit_weights() %>% arrange(desc(weight))

placebo_out %>% plot_mspe_ratio() 
ggsave(here("figures/placebo_mspe.jpg"), 
       bg = "white", width = 7, height = 7)
