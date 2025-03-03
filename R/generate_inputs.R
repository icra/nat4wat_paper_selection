generate_inputs <- function(water_types, n){
  water_types |>
    map(\(wt) generate_inputs_sp(wt, n)) |>
    list_rbind()
}

generate_inputs_sp <- function(wt, n){
  if (str_ends(wt, "_water")) {
    sp_inputs <- swm_inputs
  } else {
    sp_inputs <- treatment_inputs
  }

  inputs <- c(sp_inputs, mcda_inputs)

  inputs_tb <- inputs |>
    imap(\(x, y) tidytable(!!sym(y) := sample(x, n, replace = TRUE)))

  do.call(cbind, inputs_tb) |>
    setNames(names(inputs)) |>
    mutate(water_type = wt, .before = everything())

}

fix_household <- function(inputs){
  household_inh <- treatment_inputs$inhabitants[treatment_inputs$inhabitants < 40]

  inputs |>
    mutate(inhabitants = if_else(household, sample(household_inh, n(), replace = TRUE), inhabitants))
}

constraint_greywater <- function(inputs){
  inputs |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(ecosystem_services = if_else(
      water_type == "greywater",
      dplyr::lst(es_water_reuse = 2),
      dplyr::lst(es_water_reuse = 0)
    ))
}
