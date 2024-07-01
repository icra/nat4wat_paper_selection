library(targets)
library(tarchetypes)

controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)

# Set target options:
tar_option_set(
  # controller = controller,
  packages = c("httr2", "tidytable", "stringr", "lhs", "purrr") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
n <- 1e5
set.seed(1121)

# Replace the target list below with your own:
list(
  tar_target(inputs_treatment_raw, generate_inputs(wastewater, n)),
  tar_target(inputs_treatment, fix_household(inputs_treatment_raw)),
  tar_target(inputs_swm, generate_inputs(swm_water, n))
)
