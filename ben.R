source("setup.R")
model_input = read_model_input(scientificname = "Mola mola", 
                               mon = "Aug")
cfg = read_configuration(version = "Aug", path = data_path("models"))
db = brickman_database()
covars = read_brickman(db |> filter(scenario == "PRESENT", interval == "mon"))|>
  select(all_of(cfg$keep_vars))
all_data = prep_model_data(model_input, 
                           month = "Aug",
                           covars = covars, 
                           form = "sf") |>
  select(all_of(c("class", cfg$keep_vars)))
split_all_data = spatial_initial_split(all_data, 
                                       prop = 1 / 5,     # 20% for testing
                                       strategy = spatial_block_cv) # see ?spatial_block_cv
tr_data = training(split_all_data)
cv_tr_data <- spatial_block_cv(tr_data,
                               v = 5,     
                               cellsize = grid_cellsize(all_data),
                               offset = grid_offset(all_data) + 0.00001)
one_row_of_training_data = dplyr::slice(tr_data,1)
rec = recipe(one_row_of_training_data, formula = class ~ .)
rec = rec |> 
  step_naomit()
if ("depth" %in% cfg$keep_vars){
  rec = rec |>
    step_log(depth,  base = 10)
}
if ("Xbtm" %in% cfg$keep_vars){
  rec = rec |>
    step_log(Xbtm,  base = 10)
}

wflow = workflow_set(
  preproc = list(default = rec),
  models = list(
    glm = sdm_spec_glm(),
    rf = sdm_spec_rf(trees = tune()),
    gbm = sdm_spec_boost_tree(),
    maxent = sdm_spec_maxent()) )

wflow <- wflow |>
  workflow_map("tune_grid",
               resamples = cv_tr_data, 
               grid = 3,
               metrics = sdm_metric_set(yardstick::accuracy), 
               verbose = TRUE)

m = metrics_table(wflow, best = TRUE)

models = wflow$wflow_id |>
  sapply(
    function(id){
      w = workflowsets::extract_workflow(wflow, id)
      tune::finalize_workflow(w, filter(m, wflow_id == id)) |>
        tune::last_fit(split_all_data, 
                       metrics = tidysdm::sdm_metric_set(yardstick::accuracy))
    }, simplify = FALSE)



############
if (FALSE){
rf = wflow |>
  extract_workflow("default_rf") |>
  select_best()
  finalize_workflow(best_hyperparams(rf_model_ranks))

rf_ws_fit_final = rf_ws_workflow_final |>
  tune::last_fit(split, metrics = ws_metrics) 

rf_fit_final_metrics = rf_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_rf_final_metrics.csv")))

final_rf_workflow = extract_workflow(rf_ws_fit_final) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_final_rf_wf.Rds")))

final_rf_model = extract_fit_engine(rf_ws_fit_final)
}



  