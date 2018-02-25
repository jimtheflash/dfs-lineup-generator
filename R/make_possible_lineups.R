#' Create the possible lineups
#' 
#' @param player_position_list The list of players and their positions
#' @param salary_cap Upper limit for spend on a given lineup. Default is 50000
#' @param salary_min Lower limit for spend on a given lineup. Default is 49000
#' 
#' @return TBL of possible lineups
#' 
#' @details TODO: ADD SALARY FILTERING PARAMETERS
#' 
#' @export
make_possible_lineups <- function(player_position_list, 
                                  salary_cap = 50000, 
                                  salary_min = 49000) {
  
  
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
       
    split_static <- strsplit(combined_raw$static, split = ";")
    split_static_mat <- do.call(rbind, split_static)
    split_static_mat <- apply(split_static_mat, 2, as.numeric)
    combined_num_mat <- cbind(split_static_mat, as.numeric(combined_raw$dynamic))
    combined_num_mat_filter <- apply(combined_num_mat, 1, function(x) length(unique(x)) == length(x))
    filtered_num_mat <- combined_num_mat[combined_num_mat_filter, ]
    filtered_sort_mat <- as.matrix(t(apply(filtered_num_mat, 1, sort)))
    filtered_sort_vec <- apply(filtered_sort_mat, 1, function(x) paste(x, collapse = ";"))
    deduped_filtered_tbl <- filtered_sort_mat %>%
      as.data.frame() %>%
      dplyr::mutate(combos = as.character(filtered_sort_vec)) %>%
      dplyr::group_by(combos) %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-combos)
    output_mat <- 
      matrix(rep(rep(0, nrow(deduped_filtered_tbl)), length(position_list)), ncol = length(position_list))
    output_mat[, c(1:ncol(deduped_filtered_tbl))] <- as.matrix(deduped_filtered_tbl)
    output_df <- as.data.frame(output_mat)
    names(output_df) <- position_list
    exhausted_positions[length(exhausted_positions) + 1] <- p
  }
  
  return(output_df)
  
  
  }
  