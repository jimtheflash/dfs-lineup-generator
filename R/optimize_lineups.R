#' Optimize the lineups
#' 
#' @param unique_lineup_object List of unique objects from \code{make_unique_lineups}
#' @param n_lineups The number of lineups to save
#' @param max_exposure The max percentage of lineups in which a single player can be included
#' @param min_games A value indicating the minimum number of games to be represented on a lineups
#' @param max_teamrep A value indicating the maximum number of players that can be on a single team
#' @param limit_search A value used to reduce search space
#' 
#' @return List of optimal lineups
#' 
#' @export
#' 
optimize_lineups <- function(unique_lineup_object,
                             n_lineups, 
                             max_exposure,
                             min_games = 2,
                             max_teamrep = 4,
                             limit_search = nrow(unique_lineup_object)) {

  lineup_id <- 1:nrow(unique_lineup_object)
  unique_lineup_object$lineupid <- lineup_id
  
  final_lineups <- unique_lineup_object %>%
    dplyr::filter(games >= min_games) %>%
    dplyr::filter(max_team_rep <= max_teamrep) %>%
    dplyr::arrange(-outcome) %>%
    dplyr::filter(row_number() <= n_lineups)
  
  remaining_lineups <- unique_lineup_object %>%
    dplyr::filter(games >= min_games) %>%
    dplyr::filter(max_team_rep <= max_teamrep) %>%
    dplyr::filter(lineupid <= limit_search) %>%
    dplyr::filter(!(lineupid %in% unique(final_lineups$lineupid))) %>%
    dplyr::arrange(-outcome)
  
  freq_table <- final_lineups %>%
    dplyr::select(-outcome, -lineupid, -games) %>%
    unlist() %>%
    table() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  names(freq_table) <- c("uid", "Freq")
  
  freq_table <- freq_table %>%
    dplyr::arrange(-Freq)
  
  threshold <- nrow(final_lineups) * max_exposure
  
  while (max(freq_table$Freq) > threshold) {
    
    max_uid <- as.numeric(freq_table$uid[[1]])
    max_uid_filter <- apply(final_lineups, 1, function(x) max_uid %in% unlist(x))
    max_uid_tbl <- final_lineups[max_uid_filter, ]
    lu_to_remove_id <- max_uid_tbl$lineupid[which.min(max_uid_tbl$outcome)]
    
    
    remaining_lineup_filter <- apply(remaining_lineups, 1, function(x) max_uid %in% unlist(x))
    
    if (sum(remaining_lineup_filter) == length(remaining_lineup_filter)) {
      output_list <- list(lineups = final_lineups, 
                          exposure_table = freq_table)
      
      return(output_list)
    }
    
    filtered_lineups <- remaining_lineups[!remaining_lineup_filter, ]

    highest_score <- suppressWarnings(max(filtered_lineups$outcome))
    lu_to_insert <- filtered_lineups[filtered_lineups$outcome == highest_score, ]
    
    if (nrow(lu_to_insert) != 1) {
      break
    }
    
    final_lineups <- final_lineups %>%
      dplyr::filter(lineupid != lu_to_remove_id) %>%
      rbind(lu_to_insert)
    
    remaining_lineups <- filtered_lineups %>%
      dplyr::filter(lineupid != lu_to_insert$lineupid) %>%
      dplyr::filter(lineupid != lu_to_remove_id)
    
    freq_table <- final_lineups %>%
      dplyr::select(-outcome, -lineupid, -games) %>%
      unlist() %>%
      table() %>% 
      as.data.frame(stringsAsFactors = FALSE)
    names(freq_table) <- c("uid", "Freq")
    
    freq_table <- freq_table %>%
      dplyr::arrange(-Freq)
    
  }
  
  output_list <- list(lineups = final_lineups, 
                      exposure_table = freq_table)

  return(output_list)
  
}
