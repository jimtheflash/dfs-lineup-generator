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
