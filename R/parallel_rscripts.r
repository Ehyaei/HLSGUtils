#' Run R Scripts Parallel with System Control
#'
#' @param rscript_path path of rscript.
#' @param args input arguments of rscript.
#' @param core_numbers number of cores to run parallel jobs
#' @param free_memory_treshold upper bound for memory usage 
#' @param free_cpu_treshold upper bound for CPU usage
#' @param sleep_time sleep time between two jobs in seconds
#'
#' @return
#' @export
#'
#' @examples
parallel_rscripts <- function(
  rscript_path = "/data/array/mshoai/ADprogressionData/ADNI/scripts/data_modeling.r",
  args,
  core_numbers = 50,
  free_memory_treshold = 75,
  free_cpu_treshold = 75,
  sleep_time = 10
){
  
  # ----------------------------
  # Load Library
  suppressMessages(library(magrittr))
  suppressMessages(library(reticulate))
  
  # ----------------------------
  # Load psutil python package
  psutil <- import("psutil")

  # ----------------------------
  # Extract job number from args input
  job_number <- lapply(args, length) %>% unlist() %>% max()
  
  # ----------------------------
  # Convert input args same size
  args_df <-  as.data.frame(args,stringsAsFactors = F)
  
  # ----------------------------
  # Create R Script Base Command 
  
  script_command = paste("Rscript --vanilla", rscript_path)
  

  # ----------------------------
  # Run Parallel Tasks
  pb <- txtProgressBar(min = 0, max = job_number, style = 3)
  start_time = Sys.time()
  for(i in 1:job_number){
    
    # ----------------------------
    # Check System Status
    cpu_percent <- psutil$cpu_percent(interval=2)
    virtual_memory <- psutil$virtual_memory()["percent"]
    swap_memory <- psutil$swap_memory()["used"]
    

    while(virtual_memory > free_memory_treshold & cpu_percent > free_cpu_treshold){
      
      # ----------------------------
      # If Swap is filled Kill all Process
      if(swap_memory > 95){system("pkill -u mshoai")}
      
      # ----------------------------
      # Check system stats
      cpu_percent <- psutil$cpu_percent(interval=2)
      virtual_memory <- psutil$virtual_memory()["percent"]
      swap_memory <- psutil$swap_memory()["used"]
      
      # ----------------------------
      # every 10 second check system
      Sys.sleep(10)
    }
    
    # Add start, end and simulation name to R Script
    system(paste(script_command, paste0(unlist(args_df[i,]),collapse = " ")), wait = FALSE)
    Sys.sleep(sleep_time)
    setTxtProgressBar(pb, i)
  }
  print(paste("Parallel Task Duration: ", round(Sys.time() - start_time,2)))
}