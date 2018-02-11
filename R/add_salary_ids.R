#' Function to add salary identification
#' 
#' @param projections The unique player projections
#' @param salaries player salaries to be appended
#' 
#' @return tbl containing players and salaries appended
#' 
#' @export
add_salary_ids <- function(projections, salaries) {

  players_plus_salary_id <- projections %>%
    dplyr::left_join(select(salaries, "salary_id", "lower_clean_name"), 
              by = "lower_clean_name")
  
  return(players_plus_salary_id)
  
}