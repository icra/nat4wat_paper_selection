tar_load_globals()
tar_load(selection_treatment)
tar_load(techs)

df <- selection_treatment |> 
  filter(!is.na(id)) |> 
  select(-total_score, -n_solutions, -starts_with("weighted_"))

top3 <- df |>
  summarize(freq = n(), .by = c(id, water_type)) |> 
  tidytable::slice_max(n = 3, order_by = freq, by = water_type) |> 
  pull(id) |> 
  unique()

df |> 
  filter(id %in% top3) |> 
  select(id, matches("w[A-Z]", ignore.case = FALSE)) |> 
  pivot_longer(-id, names_to = "criteria") |> 
  summarize(mean_w = mean(value), .by = c(id, criteria)) |> 
  summarize(mean_c = mean(mean_w), sd_c = sd(mean_w), .by = criteria) |> 
  arrange(desc(sd_c))

df |> 
  filter(id %in% top3) |> 
  select(id, matches("w[A-Z]", ignore.case = FALSE)) |> 
  pivot_longer(-id, names_to = "criteria") |> 
  summarize(mean_w = mean(value), .by = c(id, criteria)) |> 
  summarize(mean_c = mean(mean_w), sd_c = sd(mean_w), .by = id) |> 
  arrange(desc(sd_c)) |> 
  left_join(select(techs, id, name), by = "id")
