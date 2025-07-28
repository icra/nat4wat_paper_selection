tar_load_globals()
tar_load(selection_treatment)
library(modelbased)

df <- selection_treatment |> 
    filter(!is.na(id))
  
  glm_rec <- recipes::recipe(id ~ ., data = df) |> 
    recipes::step_relevel(water_type, ref_level = "raw_domestic_wastewater") |> 
    recipes::step_dummy(all_nominal(), -all_outcomes()) |> 
    recipes::step_mutate_at(all_logical(), fn = \(x) as.integer(x))
  
  df_glm <- recipes::prep(glm_rec) |>
    recipes::juice() |> 
    mutate(n_solutions = if_else(is.na(n_solutions), 0, n_solutions)) |> 
    select(-id, -total_score, -matches("w[A-Z]", ignore.case = FALSE), -starts_with("weig"))
  
df_glm <- df |> 
  mutate(n_solutions = if_else(is.na(n_solutions), 0, n_solutions)) |> 
  select(-id, -total_score, -matches("w[A-Z]", ignore.case = FALSE), -starts_with("weig"))

  
poisson_mod <- glm(n_solutions ~ ., df_glm, family = "poisson")

poisson_mod <- model_number_solutions(selection_treatment)

summary(poisson_mod)

broom::tidy(poisson_mod) |> 
  select(term, estimate) |> 
  mutate(
    increase = exp(estimate),
    perc = (exp(estimate) - 1) * 100
  )

means <- estimate_means(poisson_mod, by = "water_type")

ggplot(df_glm, aes(x = water_type, y = n_solutions)) +
  # Add base data
  geom_boxplot(fill = palette["brown"]) +
  # geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
  # Add pointrange and line for means
  geom_line(data = means, aes(y = Mean, group = 1), linewidth = 1, color = palette["blue2"]) +
  geom_pointrange(
    data = means,
    aes(y = Mean, ymin = CI_low, ymax = CI_high),
    size = 1,
    color = palette["green"]
  ) +
  # Improve colors
  theme_custom()
