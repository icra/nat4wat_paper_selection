tar_load_globals()
tar_load(selection_treatment)
tar_load(techs)
gw_no <- tar_read(no_solution) |> 
  filter(water_type == "greywater")

glimpse(selection_treatment)

df <- selection_treatment |> 
  filter(water_type == "greywater") |> 
  select(id, starts_with("w"), -starts_with("wei"), -water_type) |> 
  mutate(n = n(), .by = id) |>
  filter(n > 500) |> 
  select(-n) |> 
  pivot_longer(-id)

df |> 
  ggplot(aes(x = id, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(name))

df |> 
  summarise(mean = mean(value), .by = name)

gw_techs <- techs |> 
  filter(id %in% df$id)

gw_no |> 
  count(idx)


