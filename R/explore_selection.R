
selection <- tar_read(selection_treatment)

explore_no_solutions <- function(selection){
  no_solution <- selection |> 
    filter(is.na(id)) |> 
    select(-starts_with("weighted")) |> 
    slice_sample(n = 10)

  map(seq_len(nrow(no_solution)), \(i) explore_one_no_solution(no_solution[i, ])) |> 
      list_rbind()
}

explore_one_no_solution <- function(x){
  body <- create_body_treatment(x)
  body$filterTable <- TRUE

  find_nbs(body, as_data_frame = T) |> 
    filter(water_type == TRUE) |> 
    mutate(water_type = body$waterType)
}

