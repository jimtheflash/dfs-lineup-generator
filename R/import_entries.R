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