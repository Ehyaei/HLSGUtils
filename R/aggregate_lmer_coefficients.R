#' Function to Aggregate Results of `lmer_modeling` function.
#'
#' This function aggregate coefficients of lmer models per each SNP
#'
#' @param save_model_directory directory of saved models.
#' @param model_names_pattern  model name or pattern of models name.
#' @param save_model_path aggregated models save path.
#'
#' @return
#' @export
#'
aggregate_lmer_coefficients <- function(
  save_model_directory,
  model_names_pattern,
  save_model_path
){

  # -----------------------------
  # Load Libraries
  suppressMessages(library(data.table))
  suppressMessages(library(magrittr))
  suppressMessages(library(dplyr))

  # -----------------------------
  # Define summary functions
  summary_to_table <- function(x){
    as.data.table(x) %>%
      mutate(Coefficients = row.names(x)) %>%
      select(c("Coefficients", "Estimate", "Std. Error", "df", "t value", "Pr(>|t|)"))
  }

  # -----------------------------
  # Read Data
  list.files(save_model_directory, pattern = model_names_pattern, full.names = T) %>%
    lapply(readr::read_rds) %>%
    rbindlist() -> coef_table_list

  # -----------------------------
  # Add SNP Labels
  cbind(
    data.table(SNP = rep(coef_table_list$snp,  rep(nrow(coef_table_list$coefficients[1][[1]]), nrow(coef_table_list)))),
    lapply(coef_table_list$coefficients, summary_to_table) %>% rbindlist()
  ) -> total_coefficientes

  # -----------------------------
  # Correct Column Names
  colnames(total_coefficientes)[4:7] = c("std_error","df","t_value","p_value")
  total_coefficientes$Coefficients = gsub("\\(|\\)","",  total_coefficientes$Coefficients)
  # -----------------------------
  # Write Models
  readr::write_rds(total_coefficientes, save_model_path)
}

