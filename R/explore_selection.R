
explore_no_solutions <- function(selection){
  no_solution <- selection |> 
    filter(is.na(id)) |> 
    select(-starts_with("weighted"))

  map(seq_len(nrow(no_solution)), \(i) explore_one_no_solution(no_solution[i, ], i), .progress = TRUE) |> 
      list_rbind()
}

explore_one_no_solution <- function(x, i){
  body <- create_body_treatment(x)
  body$filterTable <- TRUE

  find_nbs(body, as_data_frame = T) |> 
    filter(water_type == TRUE) |> 
    mutate(
      water_type = body$waterType,
      idx = i
    )
}

model_number_solutions <- function(selection_treatment){
  df <- selection_treatment |> 
    filter(!is.na(id))
  
  glm_rec <- recipes::recipe(id ~ ., data = df) |> 
    recipes::step_dummy(all_nominal(), -all_outcomes()) |> 
    recipes::step_mutate_at(all_logical(), fn = \(x) as.integer(x))
  
  df_glm <- recipes::prep(glm_rec) |>
    recipes::juice() |> 
    mutate(n_solutions = if_else(is.na(n_solutions), 0, n_solutions)) |> 
    select(-id, -total_score, -matches("w[A-Z]", ignore.case = FALSE), -starts_with("weig"))
  
  
  glm(n_solutions ~ ., df_glm, family = "poisson")
}

