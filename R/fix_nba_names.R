fix_nba_names <- function(salary_lookup, projection_list) {
  fixed_salary_lookup <- salary_lookup
  fixed_salary_lookup$lower_clean_name <- gsub("jr$", "", salary_lookup$lower_clean_name)
  
  fixed_projection_list <- projection_list
  fixed_projection_list$lower_clean_name <- gsub("jr$", "", projection_list$lower_clean_name)
  
  return_list <- list(fixed_salaries = fixed_salary_lookup, fixed_projections = fixed_projection_list)
}