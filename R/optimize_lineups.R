optimize_lineups <- function(unique_lineup_object,
                             position_list = c("pg", "sg", "pf", "sf", "c", "g", "f", "util"),
                             n_lineups = 20, 
                             max_exposure = .65,
                             limit_search = 5000,
                             return_freq_table = TRUE,
                             verbose = FALSE) {
  
  library(dplyr)

  final_lineups <- unique_lineup_object %>%
    arrange(-total_outcome) %>%
    filter(row_number() <= n_lineups)
  
  unique_lineups_remaining <- unique_lineup_object %>%
    arrange(-total_outcome) %>%
    filter(!(lineup_id %in% unique(final_lineups$lineup_id))) %>%
    filter(row_number() <= limit_search)
  
  freq_table <- final_lineups[, position_list] %>% 
    as.matrix() %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  names(freq_table) <- c("uid", "Freq")
  
  freq_table <- freq_table %>%
    arrange(-Freq)
  
  while (max(freq_table$Freq) > nrow(final_lineups) * max_exposure) {
    
    max_uid <- freq_table$uid[[1]] %>% 
      as.numeric()
    
    remaining_lineups_without_overexposed_player <- unique_lineups_remaining %>%
      filter(apply(unique_lineups_remaining[, position_list], 1, function(x) !max_uid %in% as.numeric(x)))
    
    lu_to_insert <- remaining_lineups_without_overexposed_player %>%
      arrange(-total_outcome) %>%
      filter(row_number() == 1)
    
    if (nrow(lu_to_insert) == 0) {
      break
    }
    
    lu_to_remove <- final_lineups %>%
      filter(apply(final_lineups[, position_list], 1, function(x) max_uid %in% as.numeric(x))) %>%
      arrange(-total_outcome) %>%
      filter(row_number() == max(row_number()))
      
      final_lineups <- final_lineups %>%
        filter(lineup_id != lu_to_remove$lineup_id) %>%
        rbind(lu_to_insert)
      
      unique_lineups_remaining <- unique_lineups_remaining %>%
        filter(lineup_id != lu_to_remove$lineup_id) %>%
        filter(lineup_id != lu_to_insert$lineup_id)
      
      freq_table <- final_lineups[, position_list] %>% 
        as.matrix() %>% 
        table() %>% 
        as.data.frame(stringsAsFactors = FALSE)
      names(freq_table) <- c("uid", "Freq")
      
      freq_table <- freq_table %>%
        arrange(-Freq)
      

  }

  output_list <- list(lineups = final_lineups, 
                      exposure_table = freq_table)

  return(output_list)
}
