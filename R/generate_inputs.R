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

correct_inconsistencies <- function(inputs_treatment){

  incorrect_bod <- with(inputs_treatment, which(cod_removal & !bod_removal))

  bod_correct <- list(
    c(bod = FALSE, cod = FALSE),
    c(bod = TRUE, cod = FALSE),
    c(bod = TRUE, cod = TRUE)
  )

  bod_sampled <- sample(bod_correct, length(incorrect_bod), replace = TRUE)

  inputs_treatment$bod_removal[incorrect_bod] <- map_lgl(bod_sampled, \(x) x[["bod"]])
  inputs_treatment$cod_removal[incorrect_bod] <- map_lgl(bod_sampled, \(x) x[["cod"]])

  if(with(inputs_treatment, which(cod_removal & !bod_removal)) |> length() > 0) rlang::abort("Incorrect combinations for BOD and COD")
  
  incorrect_tn <- with(inputs_treatment, which(tn_removal & (!no3_removal | !nh4_removal)))

  tn_correct <- list(
    c(tn = TRUE, nh4 = TRUE, no3 = TRUE),
    c(tn = FALSE, nh4 = FALSE, no3 = FALSE),
    c(tn = FALSE, nh4 = FALSE, no3 = TRUE),
    c(tn = FALSE, nh4 = TRUE, no3 = FALSE)
  )

  tn_sampled <- sample(tn_correct, length(incorrect_tn), replace = TRUE)

  inputs_treatment$tn_removal[incorrect_tn] <- map_lgl(tn_sampled, \(x) x[["tn"]])
  inputs_treatment$nh4_removal[incorrect_tn] <- map_lgl(tn_sampled, \(x) x[["nh4"]])
  inputs_treatment$no3_removal[incorrect_tn] <- map_lgl(tn_sampled, \(x) x[["no3"]])

  if(with(inputs_treatment, which(tn_removal & (!no3_removal | !nh4_removal))) |> length() > 0) rlang::abort("Incorrect combinations for TN")

  inputs_treatment
}
