tar_load_globals()

raw_ww <- tar_read(selection_treatment) |> 
  filter(water_type == "raw_domestic_wastewater")

no_raw <- tar_read(no_solution) |> 
  filter(water_type == "greywater")

no_raw |> tail() |> glimpse()

raw_ww |> filter(is.na(id)) |> count()

no_raw |> 
  mutate(idx_id = row_number()) |> 
  pivot_longer(is.logical, names_to = "criteria") |> 
  mutate(reason = case_when(
    value ~ 0L,
    !value ~ 1L,
    .default = 0L
  )) |> 
  mutate(reasons = sum(reason), .by =idx_id) |> 
  filter(reasons == 1) |> 
  summarize(reason = sum(reason), .by = criteria) |> 
  arrange(desc(reason))





