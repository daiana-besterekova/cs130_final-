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
                    i_unit = "CA", # unit where the intervention occurred
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
    title = "Placebo Analysis: CA and Synthetic CA",
    caption = "Timing of The Placebo Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  )
ggsave(here("figures/placebo_analysis.jpg"), 
       bg = "white", width = 7, height = 7)


placebo_out %>% plot_differences()  + 
  labs(
    title = "Placebo Analysis:  Difference between CA and Synthetic CA",
    caption = "Timing of The Placebo Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  )
ggsave(here("figures/placebo_difference.jpg"), 
       bg = "white", width = 7, height = 7)

placebo_out %>% grab_signficance() %>% filter(unit_name=="CA")
placebo_out %>% grab_unit_weights() %>% arrange(desc(weight))

placebo_out %>% plot_mspe_ratio() 
ggsave(here("figures/placebo_mspe.jpg"), 
       bg = "white", width = 7, height = 7)
