#' Scoping projections
#' 
#' @param projections projection data
#' 
#' @description Identifies projections that do not have salary information, and labels them out of scope.
#' 
#' @export
scope_projections <- function(projections) {
  
  scoped_projections <- dplyr::filter(projections, !is.na(salary_id))
  excluded_projections <- dplyr::filter(projections, is.na(salary_id))
  
  return_list <- list(in_scope = scoped_projections,
                      out_of_scope = excluded_projections)
  
  return(return_list)
  
}