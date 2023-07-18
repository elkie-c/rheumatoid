# baseline_days is days_to_s or days_to_o for setting the new baseline
cal_timepoint_score_baselined <-
  function(baseline_days,
           day,
           ReferenceKey,
           last_record,
           earliest_start_date,
           diagnosis_sub,
           weight_df) {
    # so you only calculate for that referencekey; must do so because your baseline days is different for different referencekey
    # diagnosis_sub <- diagnosis_sub %>% filter(Reference.Key. == ReferenceKey)
    
    df <- diagnosis_sub %>%
      filter(Reference.Key. == ReferenceKey) %>%  # filter only relevant referencekey
      mutate(
        # preserve ICD code only if the ICD happened more than days (variable) after first_ra
        ICD_before_timepoint = case_when(
          Reference.Date. <= earliest_start_date + baseline_days + day ~ ICD,
          
          TRUE ~ "NA"
        )
      ) %>%
      summarise(ICD_before_timepoint = paste(unique(ICD_before_timepoint), collapse = ","))
    
    
    # if first_ra + baseline_days + days > last-record then skip to output because you just give NA score
    if (earliest_start_date + baseline_days + day > last_record) {
      
      scores_df <- data.frame(
        ReferenceKey = ReferenceKey,
        day = day,
        score = NA_real_)
      
      return(scores_df)
    } else {
      
      
      icds_regex <- df %>% pull() %>% gsub(pattern = ",",
                                           replacement = "|",
                                           fixed = TRUE) %>% gsub(pattern = ".",
                                                                  replacement = "\\.",
                                                                  fixed = TRUE)
      # don't have to fix the decimals as they have precise meaning in ICD codes
      matching_rows <- grepl(x = weight_df$ICD9_clean,
                             pattern = icds_regex)
      
      
      # weight_df$ICD9_clean
      # icds_regex
      # weight_df[matching_rows, ] # to look at the matching rows
      
      
      if (any(matching_rows)) {
        score_before_timepoint <-
          weight_df[matching_rows,] %>% summarise(score = sum(Assigned_weight)) %>% pull()
      } else {
        score_before_timepoint <-  0
      }
      
      # return the output in a df
      scores_df <- data.frame(
        ReferenceKey = ReferenceKey,
        day = day,
        score = score_before_timepoint
      )
      
      # is deliberately a numerical so later can pivot and plot line charts
      
      return(scores_df)
    }
  }


# for debugging
# baseline_days = s_df[i, "days_to_s"] %>% pull()
# days = 0
# ReferenceKey = pull(s_df[i, "ReferenceKey"])

days <- c(0, 91, 182, 365, 730, 1096)

# pre-allocate the list with a fixed length
output_list <- vector("list", length = length(days) * nrow(s_df))

# initialize a counter for the list index
list_index <- 1

# undebug(cal_timepoint_score_baselined)

for (day in days) {
  # also add the last_record make sure within reach
  for (i in seq(nrow(oors_df))) {
    
      scores_df <- cal_timepoint_score_baselined(
        baseline_days = pull(oors_df[i, "days_to_OS"]),
        day = day,
        
        # so you get ith row, days_to_s as a baseline
        ReferenceKey = pull(oors_df[i, "ReferenceKey"]),
        last_record = pull(oors_df[i, "last_record"]),
        earliest_start_date = pull(oors_df[i, "earliest_start_date"]), # obtain
        # so only filter the relevant reference key
        diagnosis_sub,
        weight_df
      )
      
      # assign the output to the appropriate element of the list
      output_list[[list_index]] <- scores_df
      
      # increment the counter
      list_index <- list_index + 1
      
  }
}

# combine all data frames in the list into a single data frame
output_df <- data.table::rbindlist(output_list, use.names = TRUE, fill = TRUE)

# output_df %>% dplyr::arrange(Reference.Key., day) %>% View()




# section to look at difference in scores of same referencekey---------------------------------
 # group the data by Reference.Key and count the number of distinct score values
df_counts <- output_df %>%
  group_by(ReferenceKey) %>%
  filter(!is.na(score)) %>%
  summarise(n_distinct_scores = n_distinct(score))

# filter the data to keep only the groups where n_distinct_scores == 2
df_filtered <- df_counts %>%
  filter(n_distinct_scores > 1)


df_filtered %>% left_join(output_df) %>% View()
df_counts %>% pull(n_distinct_scores)

colnames(merged_df)
merged_df %>% filter(score_before_0 != score_before_365)
output_df %>% arrange(ReferenceKey) %>% View()
