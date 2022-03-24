#' LMER Modeling Function
#'
#' This function is designed to run `lmer` models on data partition.
#' It takes a formula based on variables that are set in function
#' `phonotype_gwas_partitioner` (clinical and PCA variables).
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
gwas_lmer_modeling <- function(
  data_path,
  simulation_name,
  formula ,
  save_model_path
){

  # ----------------------------- #
  # Load Libraries
  suppressMessages(library(nlme))
  suppressMessages(library(lme4))
  suppressMessages(library(readr))
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
  model_data <- read_rds(data_path)

  # ----------------------------- #
  # lmer modeling

  fit =  lmerTest::lmer(eval(formula), REML = TRUE,data = model_data,
                 control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa"))

  formula %>% gsub("\\)|\\(","",.) %>%
    strsplit("\\+|~|\\|") %>% unlist() -> formula_terms
  geno_terms = setdiff(colnames(model_data), formula_terms)

  x = geno_terms[1]

  geno_to_coef = function(x){
    tryCatch({
      summary(update(fit, eval(paste(". ~ . +",x))))$coefficients %>%
        as_tibble() %>% slice(n()) %>%
        mutate(SNP = x) %>%
        select("SNP", "Estimate", std = "Std. Error", "df",
               t_value = "t value", p_value = "Pr(>|t|)")
    },
    error = function(e){return(NULL)}
    )
  }

  coef_table = lapply(geno_terms, geno_to_coef) %>%
    bind_rows()

  # --------------------- #
  # Save coefficients
  readr::write_rds(coef_table,
                   file = paste0(save_model_path, simulation_name, file_name))
}
