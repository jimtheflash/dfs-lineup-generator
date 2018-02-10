export_lineups <- function(lineups, 
                           entries, 
                           site = "draftkings", 
                           sport = "nba",
                           slate = NULL,
                           max_lineups = 500,
                           randomize_entries = FALSE,
                           file_name = paste0(site, "_", 
                                              sport, "_", 
                                              gsub("[^[:alnum:]]", "", Sys.Date()), "_",
                                              slate, ".csv")) {
  
  library(dplyr)

  cols_to_import <- lineups %>%
    select(ends_with("salary_id"))
  
  num_import_rows <- nrow(cols_to_import)
  entry_rows <- nrow(entries)
  
  if (num_import_rows < entry_rows) {
    nrows_to_add <- entry_rows - num_import_rows
    temp <- rbind(cols_to_import, cols_to_import[1:nrows_to_add, ])
    # TODO: update to include up to 500 lineups

    cols_to_import <- temp
  }
  
  if (num_import_rows > entry_rows) {
    cols_to_import <- cols_to_import[c(1:entry_rows), ]
  }
  
  
  fixed_names <- gsub("_salary_id", "", names(cols_to_import)) %>%
    toupper()
  
  names(cols_to_import) <- fixed_names
  
  entries_filled <- entries
  
  for (i in fixed_names) {
    entries_filled[[i]] <- cols_to_import[[i]]
  }
  
  if (randomize_entries == TRUE) {
    entries_filled <- entries_filled[order(rnorm(nrow(entries_filled))), ]
  }

  write.csv(entries_filled, file = file_name, 
            row.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8")
  
}