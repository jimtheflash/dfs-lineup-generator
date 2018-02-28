#' Function to augment projections
#' 
#' @param projection_data Data containing the projections
#' @param projection_to_use Name of column containing projection data to use in lineup creation?
#' @param value_projection Name of column used to project... value...?
#' @param game_style Classic or PickEm (Default "classic")
#' 
#' @return list of augmented projections
#' 
#' @export 
augment_projections <- function(projection_data, 
                                projection_to_use,
                                value_projection,
                                game_style = "classic") {

  player_augmented <- projection_data %>%
    dplyr::mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name)),
           outcome = projection_data[[projection_to_use]]) %>%
    dplyr::mutate(pts_rank = rank(-outcome))
  
  if (game_style == "classic") {
    player_augmented <- player_augmented %>%
      dplyr::mutate(sal_over_onethousand = salary / 1000) %>%
      dplyr::mutate(ppk = outcome / sal_over_onethousand) %>%
      dplyr::mutate(ppk_rank = rank(-ppk))
    
    pos_split <- strsplit(player_augmented$pos, split = "/", fixed = TRUE)
    
    player_augmented$pos1 <- lapply(pos_split, "[", 1) %>% 
      unlist() %>% 
      tolower()
    
    player_augmented$pos2 <- lapply(pos_split, "[", 2) %>% 
      unlist() %>% 
      tolower()
  }
  
  player_augmented_no_na <- dplyr::filter(player_augmented, !is.na(outcome))

  return_list <- list(augmented_projections = player_augmented_no_na,
                      omitted_players = setdiff(player_augmented$player_name,
                                                player_augmented_no_na$player_name))
  
  return(return_list)
 
}