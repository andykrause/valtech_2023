
## Convert ZHVI to an index

zhviToIndex <- function(raw_df, 
                        base_period,
                        ...){
  
  # Get non-date names
  label_names <- which(is.na(as.numeric(substr(names(raw_df),1 , 1))))
  first_date_col <- min(which(as.Date(names(raw_df)[-label_names]) > base_period)) + 
    max(label_names)
  last_date_col <- ncol(raw_df)
  
  # Limit columns, remove any NAs
  raw_df <- raw_df[, c(label_names, first_date_col:last_date_col)] %>%
    tidyr::drop_na()
  
  index_df <- raw_df %>%
    tidyr::pivot_longer(cols = -all_of(label_names)) %>%
    dplyr::mutate(index_date = as.Date(name),
                  zhvi_value = value) %>%
    dplyr::select(-c('name', 'value')) %>%
    dplyr::arrange(RegionID, index_date)
  
  index_df <- index_df %>%
    dplyr::group_by(RegionID) %>%
    dplyr::mutate(index_value = 100 * zhvi_value/zhvi_value[1])
  
  id_l <- nrow(index_df)
  index_df$mom_value <- c(0, (index_df$index_value[2:id_l] / index_df$index_value[1:(id_l-1)]) - 1)
  index_df$mom_value <- ifelse(index_df$index_date == index_df$index_date[1], 0,
                                 index_df$mom_value)
  
  return(index_df)
  
}