#' Function to save outputs
#' 
#' @param lineups Lineups to save
#' @param entries Entry data used to convert lineups to proper format
#' @param site The site where lineups will be used. Used to inform formatting
#' @param sport The sport of choice 
#' @param slate the Slate being played (character)
#' @param max_lineups number of lineups to export
#' @param randomize_entries Logical. Indicates whether to randomize order entry.
#' @param file_name The name of the file (If not a full path, it is saved to the current directory)
#' 
#' @return Logical indicating success
#' 
#' @export
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

  cols_to_import <- lineups %>%
    dplyr::select(dplyr::ends_with("salary_id"))
  
  lu_rows <- nrow(cols_to_import)
  entry_rows <- nrow(entries)
  
  if (lu_rows < entry_rows) {
    nrows_to_add <- entry_rows - lu_rows
    temp <- rbind(cols_to_import, cols_to_import[1:nrows_to_add, ])
    cols_to_import <- temp
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
  
  return(TRUE)
}