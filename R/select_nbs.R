find_nbs <- function(body){
  request(paste(api_url, "technologies", "find-nbs", sep = "/")) |> 
    req_headers("Accept" = "application/json") |> 
    req_body_json(body) |> 
    req_perform() |> 
    resp_body_json()

}

mcda <- function(body){
  request(paste(api_url, "technologies", "mcda", sep = "/")) |> 
    req_headers("Accept" = "application/json") |> 
    req_body_json(body) |> 
    # req_dry_run()
    req_perform() |> 
    resp_body_json(simplifyDataFrame = TRUE)
}

select_nbs_treatment <- function(inputs_treatment){
  map(seq_len(nrow(inputs_treatment)), \(i) select_nbs_treatment_one(inputs_treatment[i, ]), .progress = TRUE) |> 
    list_rbind()
}

select_nbs_treatment_one <- function(x){
  pollutants <- names(x)[str_detect(names(x), "_re")]
  pollutants <- pollutants[x[1, ..pollutants] |> as_vector()]


  body <- list(
    waterType = x$water_type,
    inhabitants = x$inhabitants,
    climate = x$climates,
    pollutants = pollutants,
    minPerformance = x$min_performances,
    household = x$household
  )

  find_nbs(body) |> 
    apply_mcda(x)
}

select_nbs_swm <- function(inputs_swm){
  map(seq_len(nrow(inputs_swm)), \(i) select_nbs_swm_one(inputs_swm[i, ]), .progress = TRUE) |> 
    list_rbind()
}

select_nbs_swm_one <- function(x){
  body <- list(
    waterType = x$water_type,
    onlyInfiltration = x$only_infiltration,
    cumRain = x$cum_rains,
    catchmentArea = 10000,
    duration = x$durations,
    drainagePipeDiameter = x$drainage_pipe_diameter,
    infiltrationSoils = x$infiltration_soils,
    household = x$household
  )

  find_nbs(body) |> 
    apply_mcda(x)
}

apply_mcda <- function(selection, x){
  if (!is.null(selection$error)){
    return(
      tidytable(id = NA, total_score = NA) |> 
        bind_cols(x)
    )
  }

  criteria <- x |> 
    select(starts_with("w"), -water_type) |> 
    as.list()

  mcda(list(techs = selection, weights = criteria)) |> 
    select(id, starts_with("weighted")) |> 
    pivot_longer(-id) |> 
    mutate(total_score = sum(value), .by = id) |> 
    pivot_wider() |> 
    slice_max(total_score) |> 
    bind_cols(x)
}
