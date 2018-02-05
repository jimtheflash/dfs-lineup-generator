optimize_lineups <- function(unique_lineup_object,
                             position_list = c("pg", "sg", "pf", "sf", "c", "g", "f", "util"),
                             n_lineups = 20, 
                             max_exposure = .65,
                             limit_search = 5000,
                             return_freq_table = TRUE) {
  
  library(dplyr)
  
  final_lineups <- unique_lineup_object %>%
    arrange(-total_outcome) %>%
    filter(row_number() <= n_lineups)
  
  unique_lineups_remaining <- unique_lineup_object %>%
    arrange(-total_outcome) %>%
    filter(!(lineup_id %in% unique(final_lineups$lineup_id))) %>%
    filter(row_number() <= limit_search)
  
  freq_table <- final_lineups[, names(final_lineups) %in% position_list] %>% 
    as.matrix() %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  names(freq_table) <- c("uid", "Freq")
  
  freq_table <- freq_table %>%
    arrange(-Freq)
  
  while (max(freq_table$Freq) > (nrow(final_lineups) * max_exposure)) {
    
    max_uid <- freq_table$uid[[1]] %>% 
      as.numeric()
    
    remaining_lineups_without_overexposed_player <- unique_lineups_remaining %>%
      filter(pg != max_uid) %>%
      filter(sg != max_uid) %>%
      filter(pf != max_uid) %>%
      filter(sf != max_uid) %>%
      filter(`c` != max_uid) %>%
      filter(`g` != max_uid) %>%
      filter(`f` != max_uid) %>%
      filter(util != max_uid)
    
    # TODO: GENERALIZE TO POSITION LIST TO AVOID HARD-CODING POSITIONS!
    # for (i in position_list) {
    #   remaining_lineups_without_overexposed_player <- 
    #     remaining_lineups_without_overexposed_player[remaining_lineups_without_overexposed_player[[i]] != max_uid, ]
    # }
    
    lu_to_insert <- remaining_lineups_without_overexposed_player %>%
      arrange(-total_outcome) %>%
      filter(row_number() == 1)
    
    if (nrow(lu_to_insert) == 0) {
      break
    }
    
    lu_to_remove <- final_lineups %>%
      filter(pg == max_uid |
               sg == max_uid |
               pf== max_uid |
               sf == max_uid |
               `c` == max_uid |
               `g` == max_uid |
               `f` == max_uid |
               util == max_uid) %>%
      arrange(-total_outcome) %>%
      filter(row_number() == max(row_number()))
      
      # TODO: UPDATE TO REMOVE HARD-CODED POSITIONS 
      # lu_to_remove_filt <- matrix()
      # for (i in position_list) {
      #   lu_to_remove_filt[, ncol(lu_to_remove_filt) + 1] <- final_lineups[[i]] == max_uid
      # }
      # 
      # filt <- rowSums(lu_to_remove_filt)
      #   arrange(-total_outcome) %>%
      #   filter(row_number() == max(row_number()))

      final_lineups <- final_lineups %>%
        filter(lineup_id != lu_to_remove$lineup_id) %>%
        rbind(lu_to_insert)
      
      unique_lineups_remaining <- unique_lineups_remaining %>%
        filter(lineup_id != lu_to_remove$lineup_id) %>%
        filter(lineup_id != lu_to_insert$lineup_id)
      
      freq_table <- final_lineups[, names(final_lineups) %in% position_list] %>% 
        as.matrix() %>% 
        table() %>% 
        as.data.frame(stringsAsFactors = FALSE)
      names(freq_table) <- c("uid", "Freq")
      
      freq_table <- freq_table %>%
        arrange(-Freq)
      
      print(paste0(nrow(unique_lineups_remaining), " rows remaining"))
    
  }

  output_list <- list(lineups = final_lineups, 
                      exposure_table = freq_table)
  return(output_list)
}