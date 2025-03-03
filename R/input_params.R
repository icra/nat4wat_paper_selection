
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
  climates = c("tropical", "temperate", "continental"),
  bod_removal = c(TRUE, FALSE),
  cod_removal = c(TRUE, FALSE),
  nh4_removal = c(TRUE, FALSE),
  no3_removal = c(TRUE, FALSE),
  tn_removal = c(TRUE, FALSE),
  p_removal = c(TRUE, FALSE),
  pathogens_reduction = c(TRUE, FALSE)
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
  household = c(TRUE, FALSE)
)


# MCDA params -------------------------------------------------------------------------------------------

mcda_inputs <- list(
  wBiodiversity = 0:5,
  wOperation = 0:5,
  wSpaceRequirements = 0:5,
  wEnvImpact = 0:5,
  wCapex = 0:5,
  wSocialBenefits = 0:5,
  wCircularity = 0:5,
  wRemovalPerformance = 0:5
)

