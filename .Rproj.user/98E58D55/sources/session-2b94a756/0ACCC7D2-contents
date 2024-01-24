# called by main script; just very brief script to get the baseline characteristics table, can run twice so made this a separate script
total <- df %>% distinct(ReferenceKey) %>% nrow()

result_df <- rbind(data.frame(DrugName = "Total number", Full_Count = as.character(total), stringsAsFactors = FALSE), result_df)

for (i in sort(unique(df$DrugName_clean))) {
  count <- df %>%
    filter(DrugName_clean == i) %>%
    distinct(ReferenceKey) %>%
    nrow()
  
  # Create a new row with DrugName and Count
  new_row <- data.frame(DrugName = i, Full_Count = paste0(count, " (", round(count/total * 100, 1), ")"), stringsAsFactors = FALSE)
  
  # Append the new row to the result dataframe
  result_df <- bind_rows(result_df, new_row)
}



for (i in sort(unique(df$DrugName_clean_os))[grepl("_", sort(unique(df$DrugName_clean_os)))]) {
  
  count <- df %>%
    filter(DrugName_clean_os == i) %>%
    distinct(ReferenceKey) %>%
    nrow()
  
  # Create a new row with DrugName and Count
  new_row <- data.frame(DrugName = i, Full_Count = paste0(count, " (", round(count/total * 100, 2), ")"), stringsAsFactors = FALSE)
  
  # Append the new row to the result dataframe
  result_df <- bind_rows(result_df, new_row)
}

result_df