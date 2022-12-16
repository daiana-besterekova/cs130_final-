# Leave-One-Out --> state == 'PA'

dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 
dat <-dat[!(dat$state=="PA"),]


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
       
-------------------------------------------------------------------------------------
# Leave-One-Out --> state == 'UT'
dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 
dat <-dat[!(dat$state=="UT"),]


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
       
-------------------------------------------------------------------------------------
# Leave-One-Out --> state == 'GA'
dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 
dat <-dat[!(dat$state=="GA"),]


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
 
-------------------------------------------------------------------------------------
# Leave-One-Out --> state == 'MA'

dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 
dat <-dat[!(dat$state=="MA"),]


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
       
-------------------------------------------------------------------------------------
# Leave-One-Out --> state == 'RI'

dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 
dat <-dat[!(dat$state=="RI"),]


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
-------------------------------------------------------------------------------------
# Leave-One-Out --> state == 'DE'

dat <- readRDS(here("data/weekly_data_12022-12-15.rds")) 
dat <-dat[!(dat$state=="DE"),]


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
