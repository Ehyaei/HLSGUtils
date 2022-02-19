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

  # ----------------------------- #
  # Turn off warning and messages
  options(warn=-1)
  options(message=-1)


  # ----------------------------- #
  # Find Name of File
  file_name <- str_split(data_path, "/") %>%
    unlist() %>%
    last() %>%
    gsub("csv","rds",.) %>%
    gsub("partition","",.)

  # ----------------------------- #
  # Read Data
  model_data <- fread(data_path) %>%
    .[,c("GENDER", "PTID", "VISID"):= list(as.factor(GENDER), as.character(PTID), as.character(VISID))]


  # --------------------- #
  # Run Model
  model_data[,.(
    coefficients = list(summary(lmer(as.formula(formula), REML = TRUE))$coefficients)
  ), by = "snp"] -> fitted_model


  # --------------------- #
  # Save coefficients
  readr::write_rds(fitted_model,
                   file = paste0(save_model_path, simulation_name, file_name))
}
