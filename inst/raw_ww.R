tar_load_globals()

raw_ww <- tar_read(selection_treatment) |> 
  filter(water_type == "raw_domestic_wastewater")

