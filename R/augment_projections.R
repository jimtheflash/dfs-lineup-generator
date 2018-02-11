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
  
  player_augmented_no_na <- filter(player_augmented, !is.na(outcome))
  player_augmented_no_na$uid <- 1:nrow(player_augmented_no_na)
  
  return_list <- list(augmented_projections = player_augmented_no_na,
                      omitted_players = setdiff(player_augmented$player_name,
                                                player_augmented_no_na$player_name))
  
  return(return_list)
 
}