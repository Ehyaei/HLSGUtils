#' Read VCF Data and Convert to Delta Format
#'
#' @param vcfPath path of VCF data
#' @param savePath save path for Delta file
#' @param cores Spark local number of cores
#'
#' @return
#' @export
#'
VCF2Delta <- function(vcfPath, savePath, cores = 4){
  library(magrittr)
  jar_path <- paste0(system.file("jars", package = "HLSGUtils"),"/GenomicsUtils.jar")
  spark_submit <- paste0(Sys.getenv("SPARK_HOME"),"/bin/spark-submit")
  cache <- list.files("~/.ivy2/jars/")
  libs <- c("io.delta_delta-core_2.12-1.0.1.jar", "io.projectglow_glow-spark3_2.12-1.1.2.jar")

  has_jars <- lapply(libs,FUN = function(x) grep(x,cache)) %>% unlist() %>% length()>1
  load_libraries = ifelse(has_jars,
                          "--jars \"/home/ahmad/.ivy2/jars/*\"",
                          "--packages io.projectglow:glow-spark3_2.12:1.1.2,io.delta:delta-core_2.12:1.0.1"
                          )
  command = paste(spark_submit,
                  "--class \"io.hlsg.utils.VCF2Delta\"",
                  sprintf("--master local[%s]", cores),
                  load_libraries,
                  jar_path,
                  "-v", vcfPath,
                  "-s", savePath
                  )
  system(command)
}
