#' Function to Aggregate Results of modeling functions.
#'
#' This function aggregate coefficients of lmer models to one data.frame
#'
#' @param save_model_directory directory of saved models.
#' @param model_names_pattern  model name or pattern of models name.
#' @param save_model_path aggregated models save path.
#'
#' @return
#' @export
#'
aggregate_models <- function(
  save_model_directory,
  model_names_pattern,
  save_model_path
){

  # -----------------------------
  # Load Libraries
  suppressMessages(library(data.table))
  suppressMessages(library(magrittr))


  # -----------------------------
  # Read Data
  list.files(save_model_directory, pattern = model_names_pattern, full.names = T) %>%
    lapply(readr::read_rds) %>%
    rbindlist() -> total_coefficientes

  # -----------------------------
  # Write Models
  readr::write_rds(total_coefficientes, save_model_path)
}

