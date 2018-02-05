augment_unique_lineups <- function(unique_lineup_object, outcome_object, salary_object) {
  
  outcome_positions <- names(outcome_object)
  output_df <- unique_lineup_object
  
  for (i in outcome_positions) {
    pos_df <- data.frame(uid = unique_lineup_object[[i]])
    lu <- select((outcome_object[[i]] %>% ungroup()), uid, lower_clean_name, outcome)
    lu_plus_scores <- left_join(pos_df, lu, by = "uid")
    new_name <- paste0(i, "_lower_clean_name")
    new_outcome <- paste0(i, "_outcome")
    
    output_df[[new_name]] <- lu_plus_scores$lower_clean_name
    output_df[[new_outcome]] <- lu_plus_scores$outcome
  }
  
  output_df$total_outcome <- select(output_df, ends_with("outcome")) %>%
    rowSums() %>%
    as.numeric()
  
  salary_join <- select(salary_object, lower_clean_name, salary_id)
  output_plus_salaryid <- output_df %>%
    left_join(salary_join)
  
  return(output_df)
  
}