#' Importing Entry data
#' 
#' @param entries_path Path to Entry data
#' @param site Betting site. Default is "draftkings"
#' 
#' @export
#' 
import_entries <- function(entries_path, 
                           site = "draftkings") {
  
  entry_import <- suppressWarnings(
    read.csv(entries_path,
             header = TRUE,
             as.is = TRUE,
             stringsAsFactors = FALSE,
             check.names = FALSE)
    )

  return(entry_import)
  
}