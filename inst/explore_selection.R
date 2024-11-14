tar_load_globals()

df <- tar_read(selection_treatment) |> 
  filter(!is.na(id)) |> 
  select(-total_score, -n_solutions, -starts_with("weighted_"))

df |> 
  tidytable::summarize(n_id = n(), .by = c(water_type, id)) |> 
  tidytable::slice_max(order_by = n_id, n = 3, .by = water_type)

# Random forest ---------------------------------------------------------------

library(tidymodels)

tree_rec <- recipe(id ~ ., data = df) |> 
  step_dummy(all_nominal(), -all_outcomes()) |> 
  step_mutate_at(all_logical(), fn = \(x) as.integer(x))

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) |> 
  set_mode("classification") |> 
  set_engine("ranger")

tune_wf <- workflow() |> 
  add_recipe(tree_rec) |> 
  add_model(tune_spec)

folds <- vfold_cv(df, v = 5)

doParallel::registerDoParallel(cl = 5, cores = 5)

cat("start tuning\n")

if(FALSE){
  tune_res <- tune_grid(
    tune_wf,
    resamples = folds,
    grid = 10
  )
}


saveRDS(tune_res, "inst/tune_res.rds")

tune_res <- readRDS("inst/tune_res.rds")

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

best_auc <- select_best(tune_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

if(FALSE){
  final_rf |> 
    set_engine("ranger", importance = "permutation") |> 
    fit(
      id ~ .,
      data = juice(prep(tree_rec))
    ) |>
    saveRDS("inst/final_rf.rds")
}


fitted_rf <- readRDS("inst/final_rf.rds")
vip::vip(fitted_rf, geom = "point")

# PCA ------------------------------------------------------------------------------

library(FactoMineR)
library(factoextra)

pca_res <- prep(tree_rec) |> 
  juice() |> 
  select(-starts_with("water_type")) |> 
  select(starts_with("w"), id) |> 
  PCA(quali.sup = "id")

fviz_pca_var(pca_res) |> 
fviz_add(pca_res$quali.sup$coord[, 1:2])

pca_res$quali.sup$coord[, 1:2]

# MCDA ------------------------------------------------------------------------------

top3 <- df |> 
  summarize(freq = n(), .by = c(id, water_type)) |> 
  tidytable::slice_max(n = 3, order_by = freq, by = water_type) |> 
  pull(id) |> 
  unique()

df |> 
  filter(id %in% top3) |> 
  select(id, matches("w[A-Z]", ignore.case = FALSE)) |> 
  pivot_longer(-id) |>
  # summarize(
  #   mean = mean(value),
  #   sd = sd(value),
  #   .by = name
  # )
  ggplot(aes(x = value, y = id)) +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 3), color = palette["green"]) +
  facet_wrap(~ name, nrow = 2) +
  theme_custom()

# GLM ------------------------------------------------------------------------

df <- tar_read(selection_treatment) |> 
  filter(!is.na(id))

glm_rec <- recipes::recipe(id ~ ., data = df) |> 
  recipes::step_dummy(all_nominal(), -all_outcomes()) |> 
  recipes::step_mutate_at(all_logical(), fn = \(x) as.integer(x))

df_glm <- recipes::prep(glm_rec) |>
  recipes::juice() |> 
  mutate(n_solutions = if_else(is.na(n_solutions), 0, n_solutions)) |> 
  select(-id, -total_score, -matches("w[A-Z]", ignore.case = FALSE), -starts_with("weig"))


mod <- glm(n_solutions ~ ., df_glm, family = "poisson")

summary(mod)

broom::tidy(mod) |> 
  filter(term != "(Intercept)") |> 
  mutate(term = replace_criteria(term)) |> 
  ggplot(aes(y = reorder(term, estimate))) +
  geom_segment(aes(x = 0, xend = estimate), color = palette["blue2"], linewidth = 1) +
  geom_point(aes(x = estimate), color = palette["green"], size = 4) +
  annotate("text", x = 0.25, y = 1, label = paste0("Intercept = ", round(mod$coefficients[[1]], 2))) +
  labs(
    x = "Estimate of Poisson model",
    y = "Criteria for selection of suitable solutions"
  ) +
  theme_custom()

# Correlation n_solutions ~ freq_selected --------------------------------------

df <- tar_read(selection_treatment) |> 
    summarize(n = n(), n_solutions = mean(n_solutions), .by = c(id, water_type)) |> 
    mutate(water_type = replace_water_type(water_type))

df |> 
  ggplot(aes(x = n, y = n_solutions)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10()

cor.test(df$n, df$n_solutions, method = "spearman")

tar_read(techs) |> 
  as_tidytable() |> 
  filter(
    p_removal == 1,
    raw_domestic_wastewater == 1
  )

scores <- tar_read(selection_treatment) |> 
  filter(!is.na(id)) |> 
  summarize(
    n_chosen = n(), 
    n_solutions = mean(n_solutions), 
    .by = c(id, water_type)
  ) |> 
  mutate(
    # n_chosen_std = scale(n_chosen, center = FALSE), # / max(n_chosen),
    n_chosen_std = n_chosen / max(n_chosen),
    # n_solutions_std = scale(n_solutions, center = FALSE), # / max(n_solutions), 
    n_solutions_std = n_solutions / max(n_solutions), 
    .by = water_type
  ) |> 
  mutate(score_water = (n_chosen_std + n_solutions_std) / 2)

scores_glm <- scores |>
  summarize(score = sum(score_water) / 4, .by = id) |> 
  left_join(techs) |> 
  select(id, where(is.numeric)) |> 
  select(!where(\(x) any(is.na(x)))) |> 
  tibble::column_to_rownames("id")

glimpse(scores_glm)

y <- scores_glm$score
x <- data.matrix(scores_glm[, 2:ncol(scores_glm)])

library(glmnet)

cv_model <- cv.glmnet(x, y, alpha = 1)

best_lambda <- cv_model$lambda.min

plot(cv_model)

best_mod <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_mod)

plot(best_mod)

library(FactoMineR)
library(factoextra)

pca_res <- PCA(scores_glm, quanti.sup = "score")

fviz_pca_var(pca_res)


