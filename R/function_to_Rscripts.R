#' Convert R Functions to Scripts that are given arguments from the Console
#'
#' @param script_name path of generated script.
#' @param function_from_source the path of function that add function from source.
#' @param function_name name of function to run in script.
#' @param packages A vector of package names that have been loaded into the script.
#' @param arguments A vector of arguments are given from console.
#' @param arguments_class A vector of input arguments types.
#'
#' @return
#' @export
#'
function_to_Rscript <- function(
  script_name = "Parallel_Modeling.R",
  function_from_source = NULL,
  function_name = "lmer_modeling",
  packages =  c("IRISUtils"),
  arguments = c("data_path", "simulation_name", "formula", "save_model_path"),
  arguments_class = c("character", "character", "character", "character")
){

  # -----------------------------
  # Create File

  if (file.exists(script_name)) {
    cat("The file already exists")
  } else {
    file.create(script_name)
  }

  # -----------------------------
  # Open File
  SourceF <- file(script_name, open = "r")
  SourceF_lines <- readLines(SourceF)

  # -----------------------------
  # Create Header
  n_sharp  = 60
  nchar_name = nchar(function_name)
  rl_pad = floor((n_sharp-nchar_name-2)/2)
  add_char = ifelse(2*rl_pad+nchar_name == n_sharp-2, "", " ")

  SourceF_lines[1] = paste0(rep("#",n_sharp),collapse = "")
  SourceF_lines[2] = paste0("#",stringr::str_pad(paste0(function_name,add_char),n_sharp-2, side = "both", pad = " "),"#")
  SourceF_lines[3] = paste0(rep("#",n_sharp),collapse = "")
  SourceF_lines[4] = ""
  SourceF_lines[5] = "args <- commandArgs(trailingOnly = TRUE)"
  SourceF_lines[6] = sprintf("if (length(args) < %s){stop('I think you forgot your parameters')}",length(arguments))
  SourceF_lines[7] = ""
  SourceF_lines = c(SourceF_lines, "# Get Arguments and Convert Type")
  for(i in 1:length(arguments)){
    SourceF_lines[7+i] = ifelse(arguments_class[i] != "character",
                                paste0(arguments[i], " <- ", "as.",arguments_class[i],"(",sprintf("args[%d]",i),")"),
                                paste0(arguments[i], " <- ", sprintf("args[%d]",i)))
  }

  SourceF_lines = c(SourceF_lines, "")
  SourceF_lines = c(SourceF_lines, "flush.console()")
  SourceF_lines = c(SourceF_lines, "")
  SourceF_lines = c(SourceF_lines, "# Load Libraries")
  for(i in 1:length(packages)){
    SourceF_lines = c(SourceF_lines, sprintf("suppressMessages(library(%s))", packages[i]))
  }

  if(!is.null(function_from_source)){
    SourceF_lines = c(SourceF_lines, sprintf("source(%s)",function_from_source))
  }
  SourceF_lines = c(SourceF_lines, "")

  # Add Function
  SourceF_lines = c(SourceF_lines, "# Add Function Its Arguments")
  SourceF_lines = c(SourceF_lines, paste0(function_name,"("))
  for(i in 1:length(arguments)){
    SourceF_lines = c(SourceF_lines, ifelse(i!= length(arguments),
                                            paste0(arguments[i]," = ",arguments[i],","),
                                            paste0(arguments[i]," = ",arguments[i])
    ))
  }
  SourceF_lines = c(SourceF_lines,")")

  writeLines(SourceF_lines, script_name)
}
