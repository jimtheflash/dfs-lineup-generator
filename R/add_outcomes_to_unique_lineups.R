#' Function to add outcomes to the unique lineups
#' 
#' @param unique_lineup_object An object containing unique outcomes
#' @param outcome_object the non-unique outcome objects?
#' @param salary_object object containing salary information
#' 
#' @return Data.frame containing augmented lineups
#' 
#' @export
augment_unique_lineups <- function(unique_lineup_object, outcome_object, salary_object) {
  
  outcome_positions <- names(outcome_object)
  output_df <- unique_lineup_object
  salary_join <- select(salary_object, lower_clean_name, salary_id)
  
    
  for (i in outcome_positions) {
    pos_df <- data.frame(uid = unique_lineup_object[[i]])
    lu <- select((outcome_object[[i]] %>% ungroup()), uid, lower_clean_name, outcome)
    lu_plus_scores <- left_join(pos_df, lu, by = "uid")
    new_name <- paste0(i, "_lower_clean_name")
    new_outcome <- paste0(i, "_outcome")
    new_salary_id <- paste0(i, "_salary_id")
    
    output_df[[new_name]] <- lu_plus_scores$lower_clean_name
    output_df[[new_outcome]] <- lu_plus_scores$outcome
    
    position_salary_ids <- salary_join[salary_join$lower_clean_name %in% unique(output_df[[new_name]]), ]
    output_df$lower_clean_name <- output_df[[new_name]]
    
    output_df <- output_df %>%
      left_join(position_salary_ids, by = "lower_clean_name") %>%
      select(-lower_clean_name)
    
    
    output_df[[new_salary_id]] <- output_df$salary_id
    output_df$salary_id <- NULL
  }

  output_df$total_outcome <- select(output_df, ends_with("outcome")) %>%
    rowSums() %>%
    as.numeric()
  
  return(output_df)
  
}