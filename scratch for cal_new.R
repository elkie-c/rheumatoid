# apply the cal new score
# but baseline is on the day at which the bio_o or bio_s is started
# then calculate the score after 1 month, 6 months, 1 year
# using that, can do statistical tests to look for significant differences as well as plots


# the difficult part is need to calculate from the diagnosis_sub, which means you attempt to calculate by filtering certain reference dates
# and you subset ReferenceKey.

# score before RA; join the new scores df
scores_df <- cal_timepoint_score(0, diagnosis_sub, weight_df)

# score inclusive of the day of dx of RA (in case pt was diagnosed for problems on top of RA on that consultation day)
scores_df <- left_join(cal_timepoint_score(1, diagnosis_sub, weight_df), scores_df, by = "Reference.Key.")


df <- merged_df %>% 
  filter(os_trajectory == "s") %>% 
  distinct(ReferenceKey, .keep_all = TRUE) # only need one row per ReferenceKey
  select(ReferenceKey, days_to_s, last_record)

df <- df[, c("ReferenceKey", "days_to_s", "last_record")]

# join back the earliest start date because our days of s and o are based on it
diagnosis_sub <- merged_df %>% distinct(ReferenceKey, earliest_start_date) %>% full_join(diagnosis_sub, by = c("ReferenceKey" = "Reference.Key."))


diagnosis_sub

for (i in nrow(df)) {
  
  cal_timepoint_score(days = df[i, "days_to_s"], # so you get ith row, days_to_s as a baseline
                      ReferenceKey = df[i, "ReferenceKey"], # so only filter the relevant reference key
                      diagnosis_sub,
                      weight_df,
                      prefix = "os_baseline_score_")
  
  # now the problem is you don't want the days to appear 
  
  
  i <- 1
  days <-  df[i, "days_to_s"] %>% pull()
  refkey <- df[i, "ReferenceKey"] %>% pull()
  

  matching_keys <- unique(diagnosis_sub$Reference.Key.[diagnosis_sub$Reference.Key. %in% df$ReferenceKey])
  matching_keys
}

# for days you feed the days_till_s or o
cal_timepoint_score <- function(days, ReferenceKey, diagnosis_sub, weight_df, prefix = "score_before_") {
  
  diagnosis_sub %>% pull(ICD) %>% unique()
  
  df2 <- diagnosis_sub %>% 
    filter(ReferenceKey == refkey) %>% 
    mutate(
      ICD_before_timepoint = case_when(
        Reference.Date. <= earliest_start_date + days ~ ICD, # preserve ICD code only if the ICD happened more than days (variable) after earliest_start_date, which is not always same as first_ra
        TRUE ~ "NA")
    ) %>% 
    View() %>% 
    summarise(ICD_before_timepoint = paste(unique(ICD_before_timepoint), collapse = ","))
  
  
  # create empty df to assign the values in the for loop in this function
  scores_df <- data.frame(Reference.Key. = character(),
                          score_before_timepoint = numeric(),
                          stringsAsFactors = FALSE)
  
  # same as last time so for explanation just see above
  for (i in seq(nrow(df))) {
    icds_regex <-
      df[i, "ICD_before_timepoint"] %>% pull() %>% gsub(pattern = ",",
                                                        replacement = "|",
                                                        fixed = TRUE) %>% gsub(pattern = ".",
                                                                               replacement = "\\.",
                                                                               fixed = TRUE) 
    matching_rows <- grepl(x = weight_df$ICD9_clean,
                           pattern = icds_regex)
    
    # icds_regex
    # weight_df[matching_rows, ] # to look at the matching rows
    
    if (any(matching_rows)) {
      score_before_timepoint <- weight_df[matching_rows, ] %>% summarise(score = sum(Assigned_weight)) %>% pull()
    } else {
      score_before_timepoint <-  0
    }
    
    # do the bind_rows rather than checking the reference key
    new_row <- data.frame(Reference.Key. = df[i, "Reference.Key."],
                          score_before_timepoint = score_before_timepoint)
    scores_df <- bind_rows(scores_df, new_row)
    
  }
  
  # customise the column name to reflect the days
  names(scores_df)[names(scores_df) == "score_before_timepoint"] <- paste0(prefix, days)
  
  return(scores_df)
  
}





# for the plot on ingredients which went to excel instead 
# experimentation
# ggplot(df_counts, aes(x = Year, y = Count, fill = ingredient)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
#   scale_fill_brewer(palette = "Set1") +
#   xlab("Year") +
#   ylab("Count") +
#   ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "Ingredient and Biooriginator/Biosimilar")) +
#   scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))
# 
# 
# ggplot(df_counts, aes(x = Year, y = Count, fill = ingredient)) +
#   
#   geom_bar(data = subset(df_counts, ingredient == "ADALIMUMAB"), aes(fill = bioo_or_bios), stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
#   geom_bar(data = subset(df_counts, ingredient == "INFLIXIMAB"), aes(fill = bioo_or_bios), stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
#   
#   scale_fill_brewer(palette = "Set1") +
#   xlab("Year") +
#   ylab("Count") +
#   ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "Ingredient and Biooriginator/Biosimilar")) +
#   scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))
# 
# 
# ggplot(df_counts, aes(x = Year, y = Count, fill = interaction(ingredient, bioo_or_bios))) +
#   geom_bar(data = subset(df_counts, ingredient == "ADALIMUMAB"), stat = "identity", position = position_fill(drop = FALSE), width = 0.6) +
#   geom_bar(data = subset(df_counts, ingredient == "INFLIXIMAB"), stat = "identity", position = position_fill(drop = FALSE), width = 0.6) +
#   scale_fill_brewer(palette = "Set1") +
#   xlab("Year") +
#   ylab("Count") +
#   ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(title = "Ingredient and Biooriginator/Biosimilar")) +
#   scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))
# 


# # create two new columns, variable and value. Variable says "bioo_or_bios" or "ingredient"
# df_counts_reshaped <- df_counts %>%
#   pivot_longer(cols = c("bioo_or_bios", "ingredient"), names_to = "variable", values_to = "value")
# 
# # if_else. So next two values would be the values if TRUE and FALSE respectively
# df_counts_reshaped <- df_counts_reshaped %>%
#   mutate(variable = if_else(variable == "bioo_or_bios", "Biooriginator/Biosimilar", "Ingredient"))
# # 


# # Define color palettes for each ingredient
# colors_adalimumab <- c("#E41A1C", "#FC8D62")
# colors_infliximab <- c("#377EB8", "#A6CEE3")
# colors_rituximab <- c("#4D4D4D", "#F0F0F0")
# 
# # Subset data for each ingredient
# df_adalimumab <- df_counts %>% filter(ingredient == "ADALIMUMAB")
# df_infliximab <- df_counts %>% filter(ingredient == "RITUXIMAB")
# df_rituximab <- df_counts %>% filter(ingredient == "INFLIXIMAB")
# 
# # Plot the data with manual nudging and color schemes
# 
# ggplot() +
#   geom_bar(data = df_adalimumab, aes(x = Year, y = Count, fill = bioo_or_bios), stat = "identity",
#            position = position_nudge(x = -0.2), width = 0.25) +
#   scale_fill_manual(values = colors_adalimumab, name = "ADALIMUMAB\nBiooriginator/Biosimilar") +
#   geom_bar(data = df_infliximab, aes(x = Year, y = Count, fill = bioo_or_bios), stat = "identity",
#            position = position_nudge(x = 0), width = 0.25) +
#   scale_fill_manual(values = colors_infliximab, name = "INFLIXIMAB\nBiooriginator/Biosimilar") +
#   geom_bar(data = df_rituximab, aes(x = Year, y = Count, fill = bioo_or_bios), stat = "identity", 
#            position = position_nudge(x = 0.2), width = 0.25) +
#   scale_fill_manual(values = colors_rituximab, name = "RITUXIMAB\nBiooriginator/Biosimilar") +
#   xlab("Year") +
#   ylab("Count") +
#   ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))
# 
# 
# 
# # Plot the data with manual nudging and color schemes
# ggplot() +
#   geom_bar(data = df_adalimumab, aes(x = Year, y = Count, fill = bioo_or_bios), stat = "identity", 
#            position = position_nudge(x = -0.2), width = 0.25) +
#   scale_fill_manual(values = colors_adalimumab, name = "ADALIMUMAB\nBiooriginator/Biosimilar", aesthetics = "fill") +
#   geom_bar(data = df_infliximab, aes(x = Year, y = Count, fill = as.factor(bioo_or_bios)), stat = "identity", 
#            position = position_nudge(x = 0), width = 0.25) +
#   scale_fill_manual(values = colors_infliximab, name = "INFLIXIMAB\nBiooriginator/Biosimilar", aesthetics = "fill1") +
#   geom_bar(data = df_rituximab, aes(x = Year, y = Count, fill = as.character(bioo_or_bios)), stat = "identity", 
#            position = position_nudge(x = 0.2), width = 0.25) +
#   scale_fill_manual(values = colors_rituximab, name = "RITUXIMAB\nBiooriginator/Biosimilar", aesthetics = "fill2") +
#   xlab("Year") +
#   ylab("Count") +
#   ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))
