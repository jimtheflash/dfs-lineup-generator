#' Create the possible lineups
#' 
#' @param player_position_list The list of players and their positions
#' @param salary_cap Upper limit for spend on a given lineup. Default is 50000
#' @param salary_min Lower limit for spend on a given lineup. Default is 49000
#' 
#' @return TBL of possible lineups
#' 
#' @details TODO: remove hard coding of positions
#' 
#' @export
make_possible_lineups <- function(player_position_list, 
                                  salary_cap = 50000, 
                                  salary_min = 49000) {
  
  # what do we want to do here? i think it's find all possible lineups for a set of players
  browser()
  
  position_list <- names(player_position_list)
  
  output_mat <- matrix(rep(0, length(position_list)), ncol = length(position_list))
  output_df <- as.data.frame(output_mat)
  names(output_df) <- position_list
  
  exhausted_positions <- c()
  
  for (p in position_list) {
    
    pos_df <- player_position_list[[p]]
    
    if (nrow(output_df) == 1) {
      
      output_df_expanded <- 
        output_df[rep(seq_len(nrow(output_df)), 
                      each = length(unique(pos_df$uid))), ]
      output_df_expanded[[p]] <- unique(pos_df$uid)
      
      output_df <- output_df_expanded
      
      exhausted_positions[length(exhausted_positions) + 1] <- p
      
      next
      
    }

    exhausted_cols <- as.data.frame(output_df[, names(output_df) %in% exhausted_positions])
    names(exhausted_cols) <- exhausted_positions
    
    unique_combo_vectors <- as.character(apply(exhausted_cols, 1, function(x) paste(x, collapse = ";")))
    
    unique_new_uid <- unique(pos_df$uid)
    
    combined_raw <- expand.grid(static = unique_combo_vectors, dynamic = unique_new_uid,
                                stringsAsFactors = FALSE)
    
    if (ncol(combined_raw) == 2) {
      static_num <- as.numeric(combined_raw$static)
      combined_mat <- matrix(nrow = length(static_num), ncol = 2)
      combined_mat[, 1] <- static_num
      combined_mat[, 2] <- combined_raw$dynamic
      sort_vec <- apply(combined_mat, 1, sort)
    }
    
    split_static <- strsplit(combined_raw$static, split = ";")
    
    split_static_mat <- do.call(rbind, split_static)
    split_static_mat <- apply(split_static_mat, 2, as.numeric)
    
    combined_num_mat <- cbind(split_static_mat, as.numeric(combined_raw$dynamic))
    
    split_static_df <- as.data.frame(split_static_mat)
    names(split_static_df) <- exhausted_positions
    
    combined_

    exhausted_positions[length(exhausted_positions) + 1] <- p
    
  }
  



    
    
    
  
  }
  