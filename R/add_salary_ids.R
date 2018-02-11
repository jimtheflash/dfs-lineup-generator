add_salary_ids <- function(projections, salaries) {
  

  players_plus_salary_id <- projections %>%
    dplyr::left_join(select(salaries, "salary_id", "lower_clean_name"), 
              by = "lower_clean_name")
  
  return(players_plus_salary_id)
  
}