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

  outcome_positions <- names(outcome_object)
  lu_outcome <- do.call(rbind, outcome_object) %>%
    dplyr::group_by(uid) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(uid, outcome)
  projection_mat <- matrix(nrow = nrow(unique_lineup_object), ncol = ncol(unique_lineup_object))
  for (i in 1:ncol(unique_lineup_object)) {
    uid_column <- data.frame(uid = unique_lineup_object[, i])
    uid_outcome <- dplyr::inner_join(uid_column, lu_outcome, by = "uid")
    projection_mat[, i] <- uid_outcome$outcome
  }
  projections <- rowSums(projection_mat)
  
  
  
  
  return(output_df)
  
}