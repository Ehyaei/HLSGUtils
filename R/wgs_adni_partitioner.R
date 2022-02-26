#' Create Data Partitions for LMER Modeling
#'
#' This function creates data partitions for `lmer` modeling.
#' To create data, first we load adnimerge and select specific columns.
#' Then we load the WGS PCA data and select the required columns.
#' In the next step, merge the two datasets into one.
#' Finally, we merge WGS data by `PTID`. Data is melted and created into a
#' tidy version before being used in data.table format.
#'
#' @param partition_number is an integer number that represents the number of data is divided.
#' @param clinical_variables character vector contains names of variables in ADNI clinical data.
#' @param pca_components character vector contains names of variables in WGS PCA data.
#' @param wgs_path is path of ADNI WGS data.
#' @param pca_path is path of PCA components WGS data.
#' @param adni_path is path of adnimerge.RData. It must contains clinical_variables columns.
#' @param snp_names_save_path  is the save path of SNP names.
#' @param partitions_save_path  is the save path of data partitions.
#' @param fill_col Fills missing values in selected columns using previous entry.
#'
#' @return
#' @export
#'
wgs_adno_partitioner <- function(
  partition_number = 300,
  clinical_variables = c("MMSE", "GENDER","AGE", "MMSE.bl", "PTEDUCAT", "PTID", "VISID"),
  pca_components = c("PC1", "PC2", "PC3"),
  wgs_path,
  pca_path,
  adni_path,
  partitions_save_path,
  fill_col =  NULL
){

  ############################################################
  #                                                          #
  #                       Load Library                       #
  #                                                          #
  ############################################################

  suppressMessages(library(data.table))
  suppressMessages(library(magrittr))

  ############################################################
  #                                                          #
  #                        Load Data                         #
  #                                                          #
  ############################################################

  # --------------------- #
  # Load ADNI Clinical Data

  load(adni_path)

  # --------------------- #
  # Fill Up fill_col columns (sorted by age)
  if(!is.null(fill_col)){
    adnimerge <- adnimerge %>%
      arrange(AGE) %>%
      group_by(PTID) %>%
      tidyr::fill(fill_col)
  }

  # --------------------- #
  # Drop NA rows and create data.table
  print("Load ADNI Data...")
  adnimerge <- adnimerge %>%
    mutate(
      GENDER  = as.factor(GENDER),
      AGE = as.numeric(AGE),
      VISID = as.character(VISID)) %>%
    dplyr::select(clinical_variables) %>%
    tidyr::drop_na() %>%
    as.data.table()

  # --------------------- #
  # Load PCA Data
  print("Load PCA Data...")
  pca <- fread(pca_path) %>%
    .[,c("IID", pca_components), with = F]

  # --------------------- #
  # Create Clinical Data
  clinical <- adnimerge %>%
    merge.data.table(pca, by.x = "PTID", by.y = "IID")

  # --------------------- #
  # Read Genetics Data
  print("Load WGS Data...")
  snp_data <- fread(wgs_path) %>%
    .[, "PTID" := as.character(IID)] %>%
    .[,IID:= NULL]

  # --------------------- #
  # Create SNP Names vector
  snp_names = grep("\\d+", colnames(snp_data), value = T)
  snp_length = length(snp_names)

  # --------------------- #
  # Remove clinical columns from WGS data
  remove_columns <- setdiff(colnames(snp_data), c(snp_names, "PTID"))
  snp_data[,(remove_columns):= NULL]

  # --------------------- #
  # Add Clinical Data to SNP data
  print("Merge ADNI with ADNI Data...")
  snp_clinical_data <- merge.data.table(clinical, snp_data, by = "PTID")

  # --------------------- #
  # create SNP ID list for Each Partition
  breaks = floor(seq(0, snp_length, length.out = partition_number + 1))

  ############################################################
  #                                                          #
  #                  Create Data Partition                   #
  #                                                          #
  ############################################################

  # --------------------- #
  # Partitions SNPs
  breaks = floor(seq(0, snp_length, length.out = partition_number + 1))

  # --------------------- #
  # Extract Model Features
  model_variables = c(clinical_variables, pca_components)

  # --------------------- #
  # Add Progress Bar
  print("Start Partitions Writing...")

  pb <- txtProgressBar(min = 0, max = partition_number, style = 3)
  for(i in 1:partition_number){
    snp_cases <- snp_names[(breaks[i]+1):min(breaks[i+1], snp_length)]
    select_columns = c(model_variables, snp_cases)
    snp_clinical_data[,select_columns, with = F] %>%
      melt(snp_data, id.vars = model_variables,
           variable.name = "snp", value.name = "copy_number",
           measure.vars = snp_cases) %>%
      fwrite(file = paste0(partitions_save_path,"partition_", partition_number,
                           "_",(breaks[i]+1), "_to_", min(breaks[i+1], snp_length),".gz"),
             compress = "gzip")
    setTxtProgressBar(pb, i)
  }
}
