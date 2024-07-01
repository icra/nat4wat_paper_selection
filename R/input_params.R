
wastewater <- c(
  "raw_domestic_wastewater",
  "greywater",
  "secondary_treated_wastewater",
  "pretreated_domestic_wastewater"
  # "river_diluted_wastewater",
  # "camping_wastewater",
  # "offices_wastewater",
  # "cso_discharge_water",
)

swm_water <- c(
  "rain_water",
  "runoff_water"
)


# Params for treatment technologies ---------------------------------------------------------------------

treatment_inputs <- list(
  inhabitants = 2^c(2:12),
  climates = c("Tropical", "Temperate", "Continental"),
  bod_removal = c(TRUE, FALSE),
  cod_removal = c(TRUE, FALSE),
  nh4_removal = c(TRUE, FALSE),
  no3_removal = c(TRUE, FALSE),
  tn_removal = c(TRUE, FALSE),
  p_removal = c(TRUE, FALSE),
  pathogens_reduction = c(TRUE, FALSE),
  min_performances = seq(50, 95, 5)
)


# Params for SWM ----------------------------------------------------------------------------------------

swm_inputs <- list(
  only_infiltration = c(TRUE, FALSE),
  cum_rains = seq(10:100),
  durations = 1:24,
  drainage_pipe_diameter = seq(0, 0.4, 0.1),
  infiltration_soils = c(
    "sand",
    "loamySand",
    "sandyLoam",
    "loam",
    "siltLoam",
    "sandyClayLoam",
    "clayLoam",
    "siltyClayLoam",
    "sandyClay",
    "siltyClay",
    "clay"
  )
)

# Generic params ----------------------------------------------------------------------------------------

generic_inputs <- list(
  household = c(TRUE, FALSE),
  energy = c("yes", "no")
)


# MCDA params -------------------------------------------------------------------------------------------

mcda_inputs <- list(
  w_multifunctionality = 0:5,
  w_operation = 0:5,
  w_spaceRequirements = 0:5,
  w_envImpact = 0:5,
  w_cost = 0:5
)

