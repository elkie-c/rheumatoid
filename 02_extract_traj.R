# script sourced by wrangle.R; reused for extracting the treatment trajectory
prescription_traj_list <- split(df, f = df$ReferenceKey)


# for debugging -----------------------------------------------------------
# extract_traj(prescription_traj_list[[170]])
# prescription_traj_list[[180]]
# extract_traj(prescription_traj_list[[180]])
# temp_list <- lapply(X = prescription_traj_list[160:170], FUN = extract_traj)
# extract_traj(prescription_traj_list[["10140118"]])

# debug(extract_traj)
# undebug(gap_merge_per_drug)
# extract_traj(prescription_traj_list[["2154976"]]) %>% View()
# extract_traj(prescription_traj_list[["1319963"]]) %>% View()

# df %>% filter(ReferenceKey == 2154976) %>% tail()
# prescription_traj %>% filter(ReferenceKey == 2154976) %>% View()
# prescription_traj %>% filter(ReferenceKey == 2154976) %>% pull(DrugName_clean)
# df %>% filter(ReferenceKey == 2154976) %>% pull(DrugName_clean)


# need to have loaded the functions from 02_functions.R to run this
source("/Users/elsiechan/Documents/GitHub/rheumatoid/02_functions.R")

# takes 7 minutes to run on 16707 pts
temp_list <- lapply(X = prescription_traj_list, FUN = extract_traj)
# temp_list[["1007790"]]


# Define a function to add the ReferenceKey column which is now the name of the list
add_reference_key <- function(df, key) {
  df$ReferenceKey <- key
  return(df)
}

# Use pmap to apply the function to each data frame in the list
temp_list <- pmap(list(temp_list, names(temp_list)), add_reference_key)

# Merge all the data frames together
merged_df <- bind_rows(temp_list)

# don't want the seconds
# Extract the date component from the PrescriptionStartDate and PrescriptionEndDate columns
merged_df$PrescriptionStartDate <- as.Date(merged_df$PrescriptionStartDate)
merged_df$PrescriptionEndDate <- as.Date(merged_df$PrescriptionEndDate)

# Reorder the columns
merged_df <- merged_df %>%
  select(ReferenceKey, everything())
