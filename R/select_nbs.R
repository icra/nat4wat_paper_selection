find_nbs <- function(body, as_data_frame = FALSE){
  request(paste(api_url, "technologies", "find-nbs", sep = "/")) |> 
    req_headers("Accept" = "application/json") |> 
    req_body_json(body) |> 
    req_perform() |> 
    resp_body_json(simplifyDataFrame = as_data_frame)
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
  map(
    seq_len(nrow(inputs_treatment)), 
    \(i) select_nbs_treatment_one(inputs_treatment[i, ]), 
    .progress = TRUE
  ) |> 
    list_rbind()
}

create_body_treatment <- function(x){

  body <- list(
    waterType = x$water_type,
    inhabitants = x$inhabitants,
    climate = x$climates
  )
  
  pollutants <- names(x)[str_detect(names(x), "_re")]
  pollutants <- pollutants[x[1, ..pollutants] |> as_vector()]

  if (length(pollutants) > 0){
    if (length(pollutants) == 1) body$pollutants <- list(pollutants)
    else body$pollutants <- pollutants
  }

  if (body$waterType == "greywater"){
    body$ecosystemServices = list(es_water_reuse = 2)
  }

  body
}

select_nbs_treatment_one <- function(x){
  body <- create_body_treatment(x)

  resp <- find_nbs(body)
  selected <- map_chr(resp, \(x) if("id" %in% names(x)) x$id else "No suitable solutions")
  mcda_resp <- apply_mcda(resp, x)

  mcda_resp |> 
    mutate(selected = list(selected))
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
    write(selection$error, "log.txt", append = TRUE)

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
    mutate(n_solutions = n()) |> 
    slice_max(total_score) |> 
    bind_cols(x)
}

get_techs <- function(api_url){
  request(paste(api_url, "technologies", "technologies", sep = "/")) |> 
  req_perform() |> 
  resp_body_json(simplifyDataFrame = T)
}
