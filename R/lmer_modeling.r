#' LMER Modeling Function
#'
#' This function is designed to run `lmer` models on data partition.
#' It takes a formula based on variables that are set in function
#' `wgs_adni_partitioner` (clinical and PCA variables).
#' To set WGS in the `lmer` formula, use `copy_number` term.
#'
#' @param data_path the path of partition data
#' @param simulation_name when save output models, is used in name of save model file.
#' @param formula Character variable for `lmer` formula.
#' @param save_model_path The directory of saving model outout.
#'
#' @return
#' @export
#'
#' @examples
lmer_modeling <- function(
  data_path,
  simulation_name,
  formula ,
  save_model_path
){

  # ----------------------------- #
  # Load Libraries
  suppressMessages(library(nlme))
  suppressMessages(library(lme4))
  suppressMessages(library(data.table))
  setDTthreads(1) # Set data.table working cores
  suppressMessages(library(magrittr))
  suppressMessages(library(lmerTest))
  suppressMessages(library(stringr))
  suppressMessages(library(dplyr))

  # ----------------------------- #
  # Turn off warning and messages
  options(warn=-1)
  options(message=-1)


  # ----------------------------- #
  # Find Name of File
  file_name <- stringr::str_split(data_path, "/") %>%
    unlist() %>%
    last() %>%
    gsub("csv","rds",.) %>%
    gsub("partition","",.)

  # ----------------------------- #
  # Read Data
  model_data <- fread(data_path) %>%
    mutate(VISID = as.factor(VISID), PTID = as.factor(PTID))

  # TODO: remove this line
  if("GENDER" %in% colnames(model_data)){
    model_data$GENDER = as.factor(model_data$GENDER)
  }

  # --------------------- #
  # Run Model

  fitted_model <- model_data %>%
    group_by(snp) %>%
    summarise(coefficients = list(summary(lmerTest::lmer(eval(formula), REML = TRUE))$coefficients))
  # --------------------- #
  # Save coefficients
  readr::write_rds(fitted_model,
                   file = paste0(save_model_path, simulation_name, file_name))
}
