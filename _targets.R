library(targets)
library(tarchetypes)

controller = crew::crew_controller_local(workers = 4, seconds_idle = 60)

# Set target options:
tar_option_set(
  # controller = controller,
  packages = c(
    "httr2", "tidytable", "stringr", 
    "purrr", "ggplot2", "recipes", 
    "ggbreak", "patchwork", "gt",
    "modelsummary", "modelbased"
  ) # Packages that your targets need for their tasks.
)

tar_source()
n <- 1e4
api_url <- "http://localhost:3001"


set.seed(1121)

list(
  tar_target(techs, get_techs(api_url)),
  tar_target(pca_techs_plot, plot_pca(techs, "plots/pca_techs_plot.png"), format = "file"),
  tar_target(inputs_treatment_raw, generate_inputs(wastewater, n)),
  tar_target(inputs_treatment, correct_inconsistencies(inputs_treatment_raw)),
  # tar_target(inputs_treatment, fix_household(inputs_treatment_raw)),
  tar_target(inputs_swm, generate_inputs(swm_water, n)),
  tar_target(inputs_summary, summarize_inputs(inputs_treatment, "plots/summary_inputs.docx"), format = "file"),
  tar_target(selection_treatment, select_nbs_treatment(inputs_treatment)),
  tar_target(selection_treatment_plot, plot_selection_log2(selection_treatment, techs, "plots/selection_treatment_plot.png"), format = "file"),
  # tar_target(selection_swm, select_nbs_swm(inputs_swm)),
  tar_target(no_solution, explore_no_solutions(selection_treatment)),
  tar_target(rejection_reasons_plot, plot_rejection_reasons(no_solution, "plots/rejection_reasons_plot.png"), format = "file"),
  tar_target(loosers_treatment, find_loosers(selection_treatment, techs)),
  tar_target(loosers_treatment_table, create_loosers_table(loosers_treatment, "plots/loosers_table.docx"), format = "file"),
  tar_target(number_solutions_plot, plot_number_solutions(selection_treatment, "plots/number_solutions_plot.png"), format = "file"),
  tar_target(water_type_means, plot_means_water_type(selection_treatment, "plots/means_water_type.png")),
  tar_target(mcda_relevance_plot, plot_mcda_relevance(selection_treatment, techs, "plots/mcda_relevance_plot.png"), format = "file"),
  tar_target(scores_treatment, calc_scores(selection_treatment)),
  tar_target(scores_treatment_plot, plot_scores(scores_treatment, "id", techs, "plots/scores.png", c(8, 5)), format = "file"),
  tar_target(scores_treatment_plot_wt, plot_scores(scores_treatment, c("id", "water_type"), techs, "plots/scores_wt.png"), format = "file"),
  tar_force(
    files_to_paper_folder, 
    move_files(
      selection_treatment_plot,
      inputs_summary,
      loosers_treatment_table,
      rejection_reasons_plot,
      number_solutions_plot,
      mcda_relevance_plot,
      scores_treatment_plot,
      scores_treatment_plot_wt,
      water_type_means
    ),
    force = TRUE
  )
)

