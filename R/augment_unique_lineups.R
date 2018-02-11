#' Function to augment unique lineups
#' 
#' @param unique_lineup_object Object containing unique lineups
#' @param outcome_object the outcome data
#' @param salary_object Object containing salary information
#' 
#' @description Adds salary information to lineup objects
#' 
#' @return data frame containing augmented outputs
#' 
#' @export
augment_unique_lineups <- function(unique_lineup_object, 
                                   outcome_object, 
                                   salary_object) {
  
  
  output_df <- unique_lineup_object
  outcome_positions <- names(outcome_object)
  salary_join <- select(salary_object, lower_clean_name, salary_id)
  
  for (i in outcome_positions) {
    pos_df <- data.frame(uid = unique_lineup_object[[i]])
    lu <- select((outcome_object[[i]] %>% ungroup()), uid, lower_clean_name, outcome)
    lu_joined <- left_join(pos_df, lu, by = "uid") %>%
      left_join(salary_join, by = "lower_clean_name")

    new_name <- paste0(i, "_lower_clean_name")
    new_outcome <- paste0(i, "_outcome")
    new_salary_id <- paste0(i, "_salary_id")
    output_df[[new_name]] <- lu_joined$lower_clean_name
    output_df[[new_outcome]] <- lu_joined$outcome
    output_df[[new_salary_id]] <- lu_joined$salary_id
  }
  
  output_df$total_outcome <- select(output_df, ends_with("outcome")) %>%
    rowSums() %>%
    as.numeric()
  
  
  
  return(output_df)
  
}