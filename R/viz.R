palette <- c(
  beige = "#ffeaee", 
  brown = "#7f675b", 
  blue1 = "#0075f2", 
  blue2 = "#7DCFB6", 
  green = "#096b72"
)

theme_custom <- function(){
  theme_bw() +
  theme(
    strip.background = element_rect(fill = palette["brown"]),
    strip.text = element_text(color = "white")
  )
}

replace_water_type <- function(water_type){
  case_match(
    water_type,
    "greywater" ~ "Greywater",
    "pretreated_domestic_wastewater" ~ "Primary treated wastewater",
    "secondary_treated_wastewater" ~ "Secondary treated wastewater",
    "raw_domestic_wastewater" ~ "Raw domestic wastewater"
  )
}

replace_criteria <- function(criteria){
  case_match(
    criteria,
    "es_water_reuse" ~ "Water reuse",
    "p_removal" ~ "Phosphates removal",
    "tn_removal" ~ "Total nitrogen removal",
    "bod_removal" ~ "BOD removal",
    "cod_removal" ~ "COD removal",
    "nh4_removal" ~ "Ammonia removal",
    "no3_removal" ~ "Nitrates removal",
    "pathogens_reduction" ~ "Pathogens reduction",
    "climate" ~ "Climate",
    "household" ~ "Household scale",
    "wBiodiversity" ~ "Biodiversity",
    "wCapex" ~ "CAPEX",
    "wCircularity" ~ "Circularity",
    "wEnvImpact" ~ "Environmental impact",
    "wOperation" ~ "Operation & maintenance",
    "wRemovalPerformance" ~ "Removal performance",
    "wSocialBenefits" ~ "Social benefits",
    "wSpaceRequirements" ~ "Space requirements",
    "water_type_secondary_treated_wastewater" ~ "Secondary treated wastewater",
    "water_type_pretreated_domestic_wastewater" ~ "Primary treated wastewater",
    "water_type_raw_domestic_wastewater" ~ "Raw domestic wastewater",
    "water_type_greywater" ~ "Greywater",
    "climates_temperate" ~ "Temperate climate",
    "climates_tropical" ~ "Tropical climate",
    "min_performances" ~ "Minimal performance",
    "inhabitants" ~ "Population served",
    .default = NA
  )
}

plot_selection <- function(selection, techs, file){

  water_types <- sort(unique(selection$water_type))

  map(water_types, \(wt) plot_selection_water_type(wt, selection, techs)) |> 
    wrap_plots(ncol = 1)


  ggsave(file, p, width = 10, height = 6)
}

plot_selection_water_type <- function(wt, selection, techs){
  selection |> 
    filter(water_type == wt) |> 
    summarize(n = n(), n_solutions = mean(n_solutions), .by = c(id, water_type)) |> 
    mutate(water_type = replace_water_type(water_type)) |> 
    left_join(techs |> select(id, name), by = "id") |> 
    mutate(name = str_trunc(name, 40)) |> 
    ggplot(aes(x = n, y = name, fill = n_solutions)) +
    geom_col() + #(fill = palette["blue2"]) +
    scale_fill_distiller(
      name = "Suitable solutions", 
      palette = "Greens",
      direction = 1, 
      na.value = "grey80" #palette["brown"]
    ) +
    scale_x_break(c(1100, 3500)) +
    theme_custom() +
    theme(
      axis.text.y = element_text(hjust = 1, vjust = 0.5),
      axis.title = element_blank(),
      legend.position = "bottom"
    )
}

plot_selection_log2 <- function(selection, techs, file){
  p <- selection |> 
    summarize(n = n(), n_solutions = mean(n_solutions), .by = c(id, water_type)) |> 
    mutate(water_type = replace_water_type(water_type)) |> 
    left_join(techs |> select(id, name), by = "id") |> 
    mutate(name = str_trunc(name, 40)) |> 
    ggplot(aes(x = n, y = name)) +
    geom_col(aes(fill = n_solutions)) +
    scale_fill_distiller(
      name = "Suitable solutions", 
      palette = "Greens",
      breaks = c(5, 10, 15),
      labels = c(5, 10, 15),
      limits = c(0, 15),
      direction = 1, 
      na.value = "grey80" #palette["brown"]
    ) +
    scale_x_continuous(transform = "log2") +
    facet_wrap(
      vars(water_type), scales = "free",
      labeller = as_labeller(
        \(x) paste("(", letters[1:4], ")", x)
      )
    ) +
    theme_custom() +
    theme(
      axis.text.y = element_text(hjust = 1, vjust = 0.5),
      axis.title = element_blank(),
      legend.position = "bottom"
    )
  ggsave(file, p, width = 10, height = 6)
}

plot_rejection_reasons <- function(no_solution, file){
  p <- no_solution |> 
    mutate(idx_id = row_number()) |> 
    pivot_longer(where(is.logical), names_to = "criteria") |> 
    mutate(reason = case_when(
      value ~ 0L,
      !value ~ 1L,
      .default = 0L
    )) |> 
    mutate(reasons = sum(reason), .by = idx_id) |> 
    filter(reasons == 1) |> 
    summarize(reason = sum(reason), .by = c(water_type, criteria)) |> 
    filter(reason > 0) |> 
    mutate(
      water_type = replace_water_type(water_type),
      criteria = replace_criteria(criteria)
    ) |> 
    ggplot(aes(x = reason, y = reorder(criteria, reason))) +
    geom_col(fill = palette["green"]) +
    facet_wrap(~water_type, scales = "free_x") +
    theme_custom() +
    labs(
      x = "Number of times the criteria has been the unique reason for rejection",
      y = "Criteria used in selection"
    )
  ggsave(file, p, width = 8, height = 4)
}

plot_mcda_relevance <- function(selection_treatment, techs, file){
  df <- selection_treatment |> 
    filter(!is.na(id)) |> 
    select(-total_score, -n_solutions, -starts_with("weighted_"))

  top3 <- df |> 
  summarize(freq = n(), .by = c(id, water_type)) |> 
  tidytable::slice_max(n = 3, order_by = freq, by = water_type) |> 
  pull(id) |> 
  unique()

  p <- df |> 
    filter(id %in% top3) |> 
    select(id, matches("w[A-Z]", ignore.case = FALSE)) |> 
    pivot_longer(-id, names_to = "criteria") |>
    left_join(techs, by = "id") |> 
    mutate(criteria = replace_criteria(criteria)) |> 
    mutate(criteria = str_trunc(criteria, 20)) |> 
    ggplot(aes(x = value, y = name)) +
    stat_summary(fun.data = mean_se, fun.args = list(mult = 3), color = palette["green"]) +
    scale_y_discrete(labels = \(x) str_trunc(x, 40)) +
    facet_wrap(~ criteria, nrow = 2) +
    labs(
      x = "Average criteria's weight when each solution is selected",
      y = "Top 3 solutions for each wastewater type"
    ) +
    theme_custom()
  ggsave(file, p, width = 8, height = 5)
}

plot_number_solutions <- function(selection_treatment, file){
  mod <- selection_treatment |> 
    select(-selected) |> 
    model_number_solutions()  

  p <- broom::tidy(mod) |> 
    filter(term != "(Intercept)") |> 
    mutate(term = replace_criteria(term)) |> 
    mutate(color = case_when(
      p.value > 0.1 ~ "grey80",
      estimate > 0 & p.value < 0.1 ~ palette["green"],
      estimate < 0 & p.value < 0.1 ~ palette["brown"],
      .default = "red"
    )) |> 
    ggplot(aes(y = reorder(term, estimate), color = color)) +
    geom_segment(aes(x = 0, xend = estimate), linewidth = 1) +
    geom_point(aes(x = estimate), size = 4) +
    scale_color_identity() +
    annotate("text", x = 0.25, y = 1, size = 3.5,
            label = paste0("Intercept = ", round(mod$coefficients[[1]], 2)),
            ) +
    labs(
      x = "Estimate of Poisson model",
      y = "Criteria for selection of suitable solutions"
    ) +
    theme_custom() 
  ggsave(file, p, width = 8, height = 4)
}

create_loosers_table <- function(loosers, file){
  loosers |> 
    select(Solution = name, type, greywater, pretreated_domestic_wastewater, raw_domestic_wastewater, secondary_treated_wastewater) |> 
    rename_with(.fn = \(x) replace_water_type(x), .cols = ends_with("water")) |> 
    arrange(type) |> 
    mutate(type = case_match(
      type,
      "CW" ~ "Constructed wetlands",
      "HA" ~ "Hydroponics and aquaponics",
      "MS" ~ "Multi-stage solutions",
      "PL" ~ "Ponds and lagoons",
      .default = NA
    )) |> 
    mutate(across(ends_with("water"), \(x) case_match(x, 0 ~ "No", 1 ~ "Yes", 2 ~ "Not ideal"))) |> 
    group_by(type) |> 
    gt() |> 
    gtsave(file)
}

plot_scores <- function(scores, scores_by, techs, file, size = c(10, 6)){
  plot <- scores |>  
    mutate(water_type = replace_water_type(water_type)) |> 
    summarize(
      score_chosen = mean(n_chosen_std), 
      score_solutions = mean(n_solutions_std),
      score = score_chosen + score_solutions,
      .by = all_of(scores_by)) |> 
    left_join(techs |> select(id, name), by = "id") |> 
    pivot_longer(c(score_chosen, score_solutions), names_to = "score_type") |> 
    # arrange(water_type, desc(score)) |>
    # mutate(position = row_number()) |> 
    # mutate(position = if_else(position %% 2 == 0, position - 1, position)) |> 
    ggplot(aes(x = value, y = reorder(name, score), fill = score_type)) +
    geom_col() +
    scale_x_continuous(expand = expansion(add = c(0, 0.15))) +
    scale_fill_manual(
      values = unname(palette[c("green", "brown")]),
      labels = c("Times chosen", "Competitors"),
      name = "Score type"
    ) +
    scale_y_discrete(labels = \(x) str_trunc(x, 40)) +
    labs(x = "Total score") +
    theme_custom() +
    theme(
      axis.title.y = element_blank()
    )
  if ("water_type" %in% scores_by){
    plot <- plot + 
      facet_wrap(~water_type, scales = "free_y")
  }
  ggsave(file, plot, width = size[1], height = size[2])
}

move_files <- function(..., path = "C:/Users/jpueyo/ICRA/MULTISOURCE - General/Publications/nbs_selection/figures/"){
  if (!dir.exists(path)) rlang::abort("path doesn't exist")
  
  files <- c(...)
  for (f in files){
    file.copy(f, path, overwrite = TRUE)
  }
  path
}

plot_means_water_type <- function(selection_treatment, file){
  df <- selection_treatment |> 
    select(-selected) |> 
    filter(!is.na(id))
  
  glm_rec <- recipes::recipe(id ~ ., data = df) |> 
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

  means <- estimate_means(poisson_mod, by = "water_type")

  plot <- ggplot(df_glm, aes(x = water_type, y = n_solutions)) +
    # Add base data
    geom_boxplot(fill = palette["brown"]) +
    # geom_jitter(width = 0.1, height = 0, alpha = 0.5, size = 3) +
    # Add pointrange and line for means
    geom_line(data = means, aes(y = Mean, group = 1), linewidth = 1, color = palette["blue2"]) +
    geom_pointrange(
      data = means,
      aes(y = Mean, ymin = CI_low, ymax = CI_high),
      size = 0.5,
      color = palette["green"]
    ) +
    scale_x_discrete(labels = \(x) replace_water_type(x)) +
    labs(y = "Number of suitable solutions") +
    theme_custom() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
    ggsave(file, plot, width = 6, height = 4)  
}
