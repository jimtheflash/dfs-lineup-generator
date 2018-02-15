#' Importing Salary data
#' 
#' @param sal_path Path to Entry data
#' @param site Betting site. Default is "draftkings"
#' @param csv Logical. Not currently used.
#' @param cleansed Logical. Also not currently used.
#' 
#' @export
#' 
import_salaries <- function(sal_path, 
                            site = "draftkings", 
                            csv = TRUE,
                            from_entry = FALSE) {

  salary_import <- read.csv(sal_path,
                            header = from_entry,
                            stringsAsFactors = FALSE,
                            na.strings = "")
  
  return(salary_import)
  
}
