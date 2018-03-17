#' Create the possible lineups
#' 
#' @param player_position_list The list of players and their positions
#' @param final_positions List of final positions that need to be filled
#' @param salary_cap Upper limit for spend on a given lineup. Default is 50000
#' @param salary_min Lower limit for spend on a given lineup. Default is 49000
#' 
#' @details Should there be a points filter here?
#' 
#' @export
make_possible_lineups <- function(player_position_list,
                                  final_positions,
                                  salary_cap = 50000, 
                                  salary_min = 49000) {
  
  
  browser()
  
  output_mat <- matrix(rep(0, length(final_positions)), ncol = length(final_positions))
  output_df <- as.data.frame(output_mat)
  names(output_df) <- final_positions
  
  # exhausted_positions <- c()
  
  for (i in 1:length(final_positions)) {
    
    posname <- final_positions[[i]]
    pos_df <- player_position_list[[posname]]
    
    if (nrow(output_df) == 1) {
      output_df_expanded <- 
        output_df[rep(seq_len(nrow(output_df)), 
                      each = length(unique(pos_df$uid))), ]
      output_df_expanded[, 1] <- unique(pos_df$uid)
      output_df <- output_df_expanded
      # exhausted_positions[length(exhausted_positions) + 1] <- p
      gc()
      next
      
    }

    # exhausted_cols <- as.data.frame(output_df[, names(output_df) %in% exhausted_positions])
    # names(exhausted_cols) <- exhausted_positions
    
    cols_for_combos <- output_df[, !apply(output_df, 2, function(x) 0 %in% unique(x))] %>% 
      as.data.frame()
    names(cols_for_combos) <- final_positions[1:ncol(output_df)]
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
    
    deduped_filtered_tbl <- filtered_num_mat %>%
      as.data.frame() %>%
      dplyr::mutate(combos = as.character(filtered_sort_vec)) %>%
      dplyr::group_by(combos) %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-combos)
    
    salary_join_raw <- player_position_list[names(player_position_list) %in% c(p, exhausted_positions)]
    salary_join_df <- do.call(rbind, salary_join_raw) %>%
      dplyr::select(uid, salary) %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::ungroup()
    
    deduped_filtered_sal_mat <- matrix(nrow = nrow(deduped_filtered_tbl), ncol = ncol(deduped_filtered_tbl))
    for (i in 1:ncol(deduped_filtered_tbl)) {
      uid_vec <- as.numeric(as.matrix(deduped_filtered_tbl[, i]))
      uids <- data.frame(uid = uid_vec)
      sals <- uids %>%
        dplyr::inner_join(salary_join_df, by = "uid")
      deduped_filtered_sal_mat[, i] <- sals$salary
    }
    
    salary_filter <- rowSums(deduped_filtered_sal_mat) <= salary_cap
    
    deduped_filtered_sal_tbl <- deduped_filtered_tbl[salary_filter, ]
    
    output_mat <- 
      matrix(rep(rep(0, nrow(deduped_filtered_sal_tbl)), length(final_positions)), ncol = length(final_positions))
    output_mat[, c(1:ncol(deduped_filtered_sal_tbl))] <- as.matrix(deduped_filtered_sal_tbl)
    
    output_df <- as.data.frame(output_mat)
    names(output_df) <- final_positions
    
    exhausted_positions[length(exhausted_positions) + 1] <- p
    gc()
  }

  salary_join_total_df <- do.call(rbind, player_position_list) %>%
    dplyr::select(uid, salary) %>%
    dplyr::group_by(uid) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup()
  
  sal_mat <- matrix(nrow = nrow(output_df), ncol = ncol(output_df))
  for (j in 1:ncol(output_df)) {
    uid_vec <- as.numeric(as.matrix(output_df[, j]))
    sal_join <- data.frame(uid = uid_vec)
    sal_vec <- sal_join %>%
      dplyr::inner_join(salary_join_total_df, by = "uid") %>%
      dplyr::select(salary) %>%
      unlist()
    sal_mat[, j] <- sal_vec
    gc()
  }
  
  sal_filt <- rowSums(sal_mat) >= salary_min
  
  output_above_min_df <- output_df[sal_filt, ]
  
  return(output_above_min_df)
  
  }
  