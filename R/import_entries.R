#' Importing Entry data
#' 
#' @param entries_path Path to Entry data
#' @param site Betting site. Default is "draftkings"
#' @param csv Logical. Not currently used.
#' @param cleansed Logical. Also not currently used.
#' 
#' @export
#' 
import_entries <- function(entries_path, 
                           site = "draftkings", 
                           csv = TRUE,
                           cleansed = TRUE) {
  
  entry_import <- read.csv(entries_path,
                           header = TRUE,
                           as.is = TRUE,
                           stringsAsFactors = FALSE,
                           check.names = FALSE)
  
      
  return(entry_import)
  
}