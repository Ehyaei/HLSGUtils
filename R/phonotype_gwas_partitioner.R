#' Create Data Partitions for LMER Modeling
#'
#' This function creates data partitions for `lmer` modeling.
#' To create data, first we load adnimerge and select specific columns.
#' Then we load the WGS PCA data and select the required columns.
#' In the next step, merge the two datasets into one.
#' Finally, we merge WGS data by `PTID`. Data is melted and created into a
#' tidy version before being used in data.table format.
#'
#' @param pheno_data phenotype data.frame
#' @param geno_data GWAS data frame that contains one ID and SNP columns
#' @param joinBy The character value joinBy is common between pheno_data and geno_data columns.
#' @param partition_number is an integer number that represents the number of data is divided.
#' @param partitions_save_path  is the save path of data partitions.
#'
#' @return
#' @export
#'
phonotype_gwas_partitioner <- function(
  pheno_data,
  geno_data,
  joinBy,
  partition_number,
  partitions_save_path
){

  ############################################################
  #                                                          #
  #                       Load Library                       #
  #                                                          #
  ############################################################

  suppressMessages(library(magrittr))
  suppressMessages(library(data.table))
  suppressMessages(library(dplyr))


  # --------------------- #
  # convert data to data.table

  pheno_data =  as.data.table(pheno_data)
  geno_data = as.data.table(geno_data)

  # --------------------- #
  # Create geno and pheno colnames
  pheno_cols = colnames(pheno_data)
  geno_cols = setdiff(colnames(geno_data),pheno_cols)

  # --------------------- #
  # Merge pheno_data with geno_data
  print("Merge pheno_data with geno_data")
  pheno_geno_data <- merge.data.table(pheno_data, geno_data, by = joinBy)


  ############################################################
  #                                                          #
  #                  Create Data Partition                   #
  #                                                          #
  ############################################################

  # --------------------- #
  # create SNP ID list for Each Partition
  geno_number = length(geno_cols)
  breaks = floor(seq(0, geno_number, length.out = partition_number + 1))

  # --------------------- #
  # Add Progress Bar
  print("Start Partitions Writing...")

  pb <- txtProgressBar(min = 0, max = partition_number, style = 3)

  for(i in 1:partition_number){

    geno_cases <- geno_cols[(breaks[i]+1):min(breaks[i+1], geno_number)]
    select_columns = c(pheno_cols, geno_cases)

    pheno_geno_data[,select_columns, with = F] %>%
      write_rds(file = paste0(partitions_save_path,"partition_", partition_number,
                           "_",(breaks[i]+1), "_to_", min(breaks[i+1], geno_number),".rds"))
    setTxtProgressBar(pb, i)
  }
}
