# tar_load(selection_treatment)
# tar_load(selection_swm)

# techs <- request(paste(api_url, "technologies", "technologies", sep = "/")) |> 
#   req_perform() |> 
#   resp_body_json(simplifyDataFrame = T)

# selection_treatment |> 
#   left_join(techs |> select(id, name), .by = "id") |> 
#   ggplot(aes(y = name)) +
#   geom_bar() +
#   facet_wrap(vars(water_type)) +
#   theme(
#     axis.text.y = element_text(hjust = 1, vjust = 0.5)
#   )

# selection_swm |> 
#   left_join(techs |> select(id, name), .by = "id") |> 
#   ggplot(aes(y = name)) +
#   geom_bar() +
#   facet_wrap(vars(water_type)) +
#   theme(
#     axis.text.y = element_text(hjust = 1, vjust = 0.5)
#   )
  
