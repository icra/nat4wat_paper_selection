library("httr2")
library("tidytable")
library("stringr")
library("purrr")
walk(list.files("R"), \(f) source(file.path("R", f)))

n <- 1e2
api_url <- "http://localhost:3001"
set.seed(1121)

inputs_treatment <- generate_inputs(wastewater, n) |> 
  fix_household()

inputs_swm <- generate_inputs(swm_water, n)

find_nbs_treatment(inputs_treatment[1, ])
x <- inputs_treatment[1, ]

