tar_load_globals()
tar_load(selection_treatment)
tar_load(selection_swm)
tar_load(techs)

selection_treatment |> 
  left_join(techs |> select(id, name), .by = "id") |> 
  ggplot(aes(y = name)) +
  geom_bar() +
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
  
