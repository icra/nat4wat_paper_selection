library(targets)
library(tarchetypes)

controller = crew::crew_controller_local(workers = 4, seconds_idle = 60)

# Set target options:
tar_option_set(
  # controller = controller,
  packages = c("httr2", "tidytable", "stringr", "purrr", "ggplot2") # Packages that your targets need for their tasks.
)

tar_source()
n <- 1e4
api_url <- "http://localhost:3001"
set.seed(1121)

list(
  tar_target(inputs_treatment_raw, generate_inputs(wastewater, n)),
  tar_target(inputs_treatment, fix_household(inputs_treatment_raw)),
  tar_target(inputs_swm, generate_inputs(swm_water, n)),
  tar_target(selection_treatment, select_nbs_treatment(inputs_treatment)),
  tar_target(selection_swm, select_nbs_swm(inputs_swm))

)


