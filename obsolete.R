# (OPTIONAL) baseline table ----------------------------------------------------------
prescription_traj %>%
  filter(DrugName_clean == "cdmard") %>%
  distinct(ReferenceKey) %>%
  nrow()

# first run using full count
df <- prescription_traj

result_df <- data.frame(DrugName = character(), Full_Count = character(), stringsAsFactors = FALSE)

source("/Users/elsiechan/Documents/GitHub/rheumatoid/05_reference_table_stats.R")

result_df1 <- result_df

# second run obtaining only those with biologics use

# obtain reference key of those with bmard
temp_ref <- prescription_traj %>% filter(DrugName_clean != "cdmard") %>% select(ReferenceKey) 

df <- prescription_traj[prescription_traj$ReferenceKey %in% temp_ref$ReferenceKey, ]

result_df <- data.frame(DrugName = character(), Full_Count = character(), stringsAsFactors = FALSE)

source("/Users/elsiechan/Documents/GitHub/rheumatoid/05_reference_table_stats.R")

result_df2 <- result_df %>%
  rename(sub_count = Full_Count)

result_df <- full_join(result_df1, result_df2)

result_df$DrugName <- str_to_title(result_df$DrugName)

result_df <- result_df %>%
  rename(`2009-2022 subcohort for biologics trajectory analysis` = sub_count,
         `2009-2022 cohort` = Full_Count)

result_df <- result_df[order(result_df$DrugName != "Total Number", c$DrugName != "Cdmard", result_df$DrugName), ]

write.csv(result_df, paste0(path, "/charts/", "baseline_df.csv"), row.names = FALSE)



# question 1: Uptake of b/tsDMARDs (stratified by mode of action and bio-originator / biosimilars) by year (2010-2022) among patients with rheumatoid arthritis (RA); ended up using EXCEL instead-------------------------------------

#look at the counts 
df <- prescription_traj %>% 
  select(ReferenceKey, bioo_or_bios, DrugName_clean) %>% 
  distinct(ReferenceKey, DrugName_clean, bioo_or_bios) %>% 
  group_by(DrugName_clean, bioo_or_bios) %>% 
  summarise(count = n())

print(df, n = 27)

# barplot which shows uptake by bioo_or_bios ------------------------------
# Create a subset of the data frame with only the columns we need
df_subset <- prescription_traj %>% select(DispensingDate = "DispensingDate(yyyy-mm-dd)", ReferenceKey, bioo_or_bios = "bioo_or_bios", ingredient = "ingredient")

# Convert DispensingDate to Date format
df_subset$DispensingDate <- ymd(df_subset$DispensingDate)

# Extract year from DispensingDate
df_subset$Year <- year(df_subset$DispensingDate)

# Count the number of unique patients per year, bioo_or_bios, and ingredient
df_counts <- df_subset %>%
  filter(!is.na(bioo_or_bios) & !is.na(ingredient)) %>%
  distinct(Year, ReferenceKey, bioo_or_bios, ingredient) %>%
  group_by(Year, bioo_or_bios, ingredient) %>%
  summarise(Count = n())

print(df_counts, n = 32)

ggplot(df_counts, aes(x = Year, y = Count, fill = bioo_or_bios)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Ingredient and Biooriginator/Biosimilar")) +
  scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))

# interaction
ggplot(df_counts, aes(x = Year, y = Count, fill = interaction(bioo_or_bios, ingredient))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Ingredient and Biooriginator/Biosimilar")) +
  scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))


ggplot(df_counts, aes(x = Year, y = Count, fill = bioo_or_bios, ingredient)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of Biooriginator or Biosimilar Uptake by Year and Ingredient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Ingredient and Biooriginator/Biosimilar")) +
  scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))

# create a new column combining 'ingredient' and 'bioo_or_bios'
df_counts$ing_bio = paste(df_counts$ingredient, df_counts$bioo_or_bios, sep="_")

# reshape data
df_reshaped <- df_counts %>%
  select(Year, Count, ing_bio) %>%
  pivot_wider(names_from = ing_bio, values_from = Count, values_fill = 0)

write.csv(df_counts, file = paste0(path, "/df_counts.csv"), row.names = FALSE)
write.csv(df_reshaped, file = paste0(path, "/df_reshaped.csv"), row.names = FALSE)

# barplot which shows by MoA ----------------------------------------------
# this is to show all the moa has been labelled
prescription_traj %>% filter(is.na(moa)) 

moa_counts <- prescription_traj %>% select(DispensingDate = "DispensingDate(yyyy-mm-dd)", ReferenceKey, moa = "moa")

# Convert DispensingDate to Date format
moa_counts$DispensingDate <- ymd(moa_counts$DispensingDate)

# Extract year from DispensingDate
moa_counts$Year <- year(moa_counts$DispensingDate)

# Count the number of unique patients per year, bioo_or_bios, and ingredient

moa_counts <- moa_counts %>%
  distinct(Year, ReferenceKey, moa) %>%
  group_by(Year, moa) %>%
  summarise(Count = n())

print(moa_counts, n = 32)

colnames(moa_counts)

# ignoring cdmard because those
ggplot(moa_counts %>% filter(moa != "cdmard"), aes(x = Year, y = Count, fill = moa)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of biologic drug use \n by MoA from 2009 to 2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "MoA")) +
  scale_x_continuous(breaks = unique(moa_counts$Year), labels = unique(moa_counts$Year))

moa_counts <- moa_counts %>% filter(moa != "cdmard")

# reshape data
moa_reshaped <- moa_counts %>%
  select(Year, Count, moa) %>%
  pivot_wider(names_from = moa, values_from = Count, values_fill = 0)


write.csv(moa_counts, file = paste0(path, "/moa_counts.csv"), row.names = FALSE)
write.csv(moa_reshaped, file = paste0(path, "/moa_reshaped.csv"), row.names = FALSE)
