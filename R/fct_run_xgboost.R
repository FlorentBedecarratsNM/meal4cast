#' run_xgboost 
#'
#' @description Execute the run function sourced from Github.
#'
#' @return Values are saved to the filesystem.
#'
#' @noRd
run_xgboost <- function(begin_date = '2017-09-30',
                         column_to_predict = 'reel', 
                         data_path = "tests/data",
                         confidence = 0.90,
                         end_date = '2017-12-15',
                         prediction_mode=TRUE,
                         preprocessing=TRUE,
                         remove_no_school=TRUE,
                         remove_outliers=TRUE,
                         school_cafeteria='',
                         start_training_date='2012-09-01',
                         training_type='xgb',
                         weeks_latency=10) {
  # On passe les arguments à pyton au travers d'une classe
  args <- reticulate::PyClass(classname = "arguments", 
                              defs = list(
                                begin_date = begin_date,
                                column_to_predict = column_to_predict,
                                data_path = data_path,
                                confidence = confidence,
                                end_date = end_date,
                                prediction_mode = prediction_mode,
                                preprocessing = preprocessing,
                                remove_no_school = remove_no_school,
                                remove_outliers = remove_outliers,
                                school_cafeteria = school_cafeteria,
                                start_training_date = start_training_date,
                                training_type = training_type,
                                weeks_latency = weeks_latency))
  run(args)
}