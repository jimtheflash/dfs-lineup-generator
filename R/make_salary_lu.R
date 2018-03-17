#' Create the salary lookup table
#' 
#' @param salary_import Salary data, generally obtained via \code{import_salaries}
#' @param site Betting site. Default is "draftkings"
#' @param game_style One of 'classic' or 'pickem'
#' 
#' @export
#' 
make_salary_lu <- function(salary_import,
                           site = "draftkings",
                           game_style = "classic") {
  
  salary_import_clean <- salary_import
  names(salary_import_clean) <- gsub("[^[:alnum:]]", "", names(salary_import_clean)) %>% 
    tolower()
  
  if (site == "draftkings") {
    
    game_lu <- data.frame(gameinfo = unique(salary_import_clean$gameinfo),
                          game_id = 1:length(unique(salary_import_clean$gameinfo)),
                          stringsAsFactors = FALSE)
    
    if (game_style == "classic") {
      salary_lu <- suppressWarnings(
        data.frame(player_name = salary_import_clean$name,
                   salary_id = as.numeric(salary_import_clean$id),
                   salary = as.numeric(salary_import_clean$salary),
                   gameinfo = salary_import_clean$gameinfo,
                   stringsAsFactors = FALSE) %>%
          dplyr::filter(player_name != "Name" & !is.na(player_name) & !is.na(salary_id))) 
    }
    
    if (game_style == "pickem") {
      salary_lu <- suppressWarnings(
        data.frame(player_name = salary_import_clean$name,
                   salary_id = as.numeric(salary_import_clean$id),
                   tier = salary_import_clean$rosterposition,
                   stringsAsFactors = FALSE) %>%
          dplyr::filter(player_name != "Name" & !is.na(player_name) & !is.na(salary_id)))
    }
  }
  
  salary_lu <- salary_lu %>%
   dplyr::mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name)),
                 uid = 1:nrow(salary_lu)) %>%
    dplyr::left_join(game_lu, by = "gameinfo") %>%
    dplyr::select(-gameinfo)
  
  return(salary_lu)
  
}