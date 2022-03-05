#' Convert R Functions to Scripts that are given arguments from the Console
#'
#' @param function_from_source the path of function that add function from source.
#' @param function_name name of function to run that is defined in R source file.
#' @param function_from_package name of function to run in script.
#' @param packages A vector of package names that have been loaded into the script.
#' @param arguments A vector of arguments are given from console.
#' @param arguments_class A vector of input arguments types.
#' @param script_save_path path of generated script.

#'
#' @return
#' @export
#'
function_to_Rscript <- function(
  function_from_source = NULL,
  function_name = NULL,
  function_from_package = NULL,
  packages =  c("tidyverse"),
  arguments ,
  arguments_class ,
  script_save_path
){

  # -----------------------------
  # Check Input function
  if(is.null(function_from_source) & is.null(function_from_package)){
    return("Please set function_from_source or function_from_package!!!")
  }

  if(!is.null(function_from_source) & is.null(function_name)){
    return("Please set function_name to specific function to Run from source file!!")
  }


  # -----------------------------
  # Create File
  file.create(script_save_path, overwrite = TRUE)

  # -----------------------------
  # Open File
  SourceF <- file(script_save_path, open = "r")
  script_lines <- readLines(SourceF)

  # -----------------------------
  # Function Name

  if(is.null(function_from_source)){
    function_name = function_from_package
  }


  # -----------------------------
  # Create Header
  n_sharp  = 60
  nchar_name = nchar(function_name)
  rl_pad = floor((n_sharp-nchar_name-2)/2)
  add_char = ifelse(2*rl_pad+nchar_name == n_sharp-2, "", " ")

  script_lines[1] = paste0(rep("#",n_sharp),collapse = "")
  script_lines[2] = paste0("#",stringr::str_pad(paste0(function_name, add_char),n_sharp-2, side = "both", pad = " "),"#")
  script_lines[3] = paste0(rep("#",n_sharp),collapse = "")
  script_lines[4] = ""
  script_lines[5] = "args <- commandArgs(trailingOnly = TRUE)"
  script_lines[6] = sprintf("if (length(args) < %s){stop('I think you forgot your parameters')}",length(arguments))
  script_lines[7] = ""
  script_lines = c(script_lines, "# Get Arguments and Convert Type")

  # -----------------------------
  # Convert Arguments Types
  for(i in 1:length(arguments)){
    script_lines[7+i] = ifelse(arguments_class[i] != "character",
                                paste0(arguments[i], " <- ", "as.",arguments_class[i],"(",sprintf("args[%d]",i),")"),
                                paste0(arguments[i], " <- ", sprintf("args[%d]",i)))
  }

  # -----------------------------
  script_lines = c(script_lines, "")
  script_lines = c(script_lines, "flush.console()")
  script_lines = c(script_lines, "")
  script_lines = c(script_lines, "# Load Libraries")
  for(i in 1:length(packages)){
    script_lines = c(script_lines, sprintf("suppressMessages(library(%s))", packages[i]))
  }

  if(!is.null(function_from_source)){
    script_lines = c(script_lines, sprintf("source(\"%s\")", function_from_source))
  }
  script_lines = c(script_lines, "")

  # Add Function
  script_lines = c(script_lines, "# Add Function Its Arguments")
  script_lines = c(script_lines, paste0(function_name, "("))
  for(i in 1:length(arguments)){
    script_lines = c(script_lines, ifelse(i!= length(arguments),
                                            paste0(arguments[i]," = ",arguments[i],","),
                                            paste0(arguments[i]," = ",arguments[i])
    ))
  }
  script_lines = c(script_lines,")")

  writeLines(script_lines, script_save_path)
}

