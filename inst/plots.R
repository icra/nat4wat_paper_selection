tar_load_globals()
tar_load(selection_treatment)
tar_load(selection_swm)
tar_load(techs)

selection_treatment |> 
  summarize(n = n(), n_solutions = mean(n_solutions), .by = c(id, water_type)) |> 
  left_join(techs |> select(id, name), .by = "id") |> 
  ggplot(aes(x = n, y = name, fill = n_solutions)) +
  geom_col() +
  facet_wrap(vars(water_type)) +
  theme(
    axis.text.y = element_text(hjust = 1, vjust = 0.5)
  )
ggsave("inst/selection_treatment.png", width = 8, height = 8)

selection_swm |> 
  left_join(techs |> select(id, name), .by = "id") |> 
  ggplot(aes(y = name)) +
  geom_bar() +
  facet_wrap(vars(water_type)) +
  theme(
    axis.text.y = element_text(hjust = 1, vjust = 0.5)
  )

selection_treatment |> 
  filter(water_type == "greywater") |> 
  filter(is.na(id))

selection_treatment |> 
  ggplot(aes(water_type, n_solutions)) +
  geom_violin()


tar_read(selection_treatment) |> 
  count(id, water_type) |> 
  arrange(n) |> 
  print(n = 45)
