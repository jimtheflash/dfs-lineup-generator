#' Function to save outputs
#' 
#' @param lineups Lineups to save
#' @param entries Entry data used to convert lineups to proper format
#' @param lookup Dataframe for linking salary_id to 
#' @param site The site where lineups will be used. Used to inform formatting
#' @param sport The sport of choice
#' @param slate the slate (e.g., night, late, etc.)
#' @param max_lineups The maximum number of lineups
#' @param randomize_entries Logical. Indicates whether to randomize order entry.
#' @param file_name The name of the file (If not a full path, it is saved to the current directory)
#' 
#' @return Logical indicating success
#' 
#' @export
export_lineups_new <- function(lineups, 
                           entries,
                           lookup,
                           site = "draftkings", 
                           sport = "nba",
                           slate = NULL,
                           max_lineups = 500,
                           randomize_entries = FALSE,
                           file_name = paste0(site, "_", 
                                              sport, "_", 
                                              gsub("[^[:alnum:]]", "", Sys.Date()), "_",
                                              slate, ".csv")) {

  new_names <- gsub("\\.[1-9]", "",  names(lineups))
  names(lineups) <- new_names

  position_list_raw <- names(entries) %>% tolower()
  position_list <- position_list_raw[!grepl("fee|name|id$", position_list_raw)]

  cols_to_import <- lineups[, names(lineups) %in% position_list]
  
  num_import_rows <- nrow(cols_to_import)
  entry_rows <- nrow(entries)
  
  if (num_import_rows < entry_rows) {
    nrows_to_add <- entry_rows - num_import_rows
    temp <- rbind(cols_to_import, cols_to_import[1:nrows_to_add, ])
    #TODO: update to include up to 500 lineups
    
    cols_to_import <- temp
  }
  
  if (num_import_rows > entry_rows) {
    cols_to_import <- cols_to_import[c(1:entry_rows), ]
  }
  
  replaced_salaries <- matrix(ncol = ncol(cols_to_import),
                              nrow = nrow(cols_to_import)) %>%
    as.data.frame()
  names(replaced_salaries) <- names(cols_to_import)
  
  for (i in names(cols_to_import)) {
    join_df <- data.frame(pos = cols_to_import[[i]])
    joined <- join_df %>%
      dplyr::left_join(dplyr::select(lookup, "uid", "salary_id"), by = c("pos" = "uid"))
    replaced_salaries[[i]] <- joined$salary_id
  }
  
  fixed_names <- gsub("\\.[1-9]", "", replaced_salaries)
  
  names(replaced_salaries) <- fixed_names
  
  entries_filled <- entries
  
  for (i in fixed_names) {
    entries_filled[[i]] <- replaced_salaries[[i]]
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