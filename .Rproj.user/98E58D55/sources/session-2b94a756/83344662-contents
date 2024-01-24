df1 <- df1 %>%
  mutate(PrescriptionInterval_1 = interval(PrescriptionStartDate, PrescriptionEndDate))

# Create a new column in merged_df_drugnamed with the prescription date range as an interval
df2 <- df2 %>%
  mutate(PrescriptionInterval_2 = interval(PrescriptionStartDate, PrescriptionEndDate))

merged_df <- df1 %>% left_join(
  df2,
  by = c(
    "ReferenceKey",
    "PrescriptionStartDate",
    "PrescriptionEndDate"
  ),
  suffix = suffix
)

# suffix[2] is the one you would expect NA, because suffix[1] contains more information
na_rows <- merged_df %>% filter(is.na(DrugName_clean_2))


# for each ReferenceKey in that filtered merged_df, also grab the rows from moanamed_df

#' fill_na_with_moa
#'
#' @param a take the same Referencekey pt with NA entries, from the na_rows
#' @param b from the same ReferenceKey, the entire moa 
#'
#' @return a but instead of NA, those are filled with where the interval matches in b, the moa
#' @export

fill_na_with_moa <- function(a, b) {
  # get the drug intervals where moa is NA; we don't use gap_output because that would be NA for all pt at the end anyways
  interval_a <- a %>% pull(PrescriptionInterval_1)
  
  df1 %>% filter(ReferenceKey == ref) %>% print(n = 100)
  df2 %>% filter(ReferenceKey == ref) %>% print(n = 100)
  
  
  # check interval_a against every interval in b$PrescriptionInterval_moa, to get the index which matches b
  overlaps <- sapply(interval_a, function(x) {
    which(x %within% b$PrescriptionInterval_2)
  })
  
  # in certain situations, same of interval_a hit 

  
  if (is.matrix(overlaps)) { # in the case where sometimes breaking rituximab into o and s causes it to produce gaps which result in interval of a hitting more than one b, we take the drug combination with more drugs
    # albeit information loss but very briefly because if the other single drug continued for long it would have been bridged by the threshold anyways (need thinking)
    
    # the way it works is loop through different rows of the same column and find the one with the longest ddrug name. Then use that after the loop. 
    # so first round is 0, so temp would be set as longest_drug_row. Thereafter you would actually have a row
    longest_drug_row <- 0
    reduced_overlap <- numeric()
    
    # for (i in seq(ncol(overlaps))) {
    #   for (j in seq(nrow(overlaps))) {
    #     print(paste0("j is" , j, ",     i is", i))
    #   }
    # }
    for (i in seq(ncol(overlaps))) {
      for (j in seq(nrow(overlaps))) {
      temp_row <- b[overlaps[j, i], ]
      
      if (
        nchar(temp_row$DrugName_clean) > 
          ifelse(longest_drug_row == 0, 0, nchar(b[longest_drug_row, ]$DrugName_clean))
          ) { # DrugName_clean? or will it have other colnames?
        # ifelse such that return 0 rather than integer(0) the first time
        longest_drug_row <- overlaps[j, i] # you take the number so we can feed it into the next step
      }
      
      }
      # after looping through, for the same ncol, the diff rows
      # row refers to that in b
      reduced_overlap <- c(reduced_overlap, longest_drug_row)
    }
    overlaps <- reduced_overlap
  }
  
  
  # for the same drug class there could have been diff drugs taken, so would expect duplicate rows in b_cols
  b_cols <- b[overlaps,] %>% rename(
    DrugName_clean_2 = DrugName_clean,
    duration_2 = duration,
    gap_output_2 = gap_output
  ) %>% select(DrugName_clean_2, duration_2, gap_output_2, PrescriptionInterval_2)
  
  # drop those columns (of _2, so of the less informative df) from df1, and take those columns from df2; filled as in the previous NA are now filled with values
  filled_rows <- a %>% 
    select(-DrugName_clean_2, -duration_2, -gap_output_2, -PrescriptionInterval_2) %>% 
    cbind(b_cols)
  
  return(filled_rows)
}



# for debugging
# ref <- 2154976
# ref <- 2209539
# debug(fill_na_with_moa)
# fill_na_with_moa(a, b)
# merged_df %>% filter(ReferenceKey == 2154976) %>% View()
# ref <- "11328056"
# ref <- "11976795"

filled_rows_list <- list()

for (ref in unique(na_rows$ReferenceKey)) {
  # create the two rows with refkey, only for that pt
  # taken from na_rows since that has already been filtered
  
  a <- na_rows %>% filter(ReferenceKey == ref) 
  b <- df2 %>% filter(ReferenceKey == ref)
  
  # append the output to the list
  filled_rows_list <- c(filled_rows_list, list(fill_na_with_moa(a, b)))
}

# combine all the elements in the list into a single data frame
filled_rows_df <- bind_rows(filled_rows_list)

# combine with the rows that are did not have NA in the larger df
merged_df <- rbind(merged_df %>% filter(!is.na(DrugName_clean_2)), filled_rows_df) %>% arrange(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate)


#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # merged_df %>% distinct(ReferenceKey)
#' 
#' moanamed_df <- moanamed_df %>% 
#'   mutate(PrescriptionInterval_moa = interval(PrescriptionStartDate, PrescriptionEndDate))
#' 
#' # first step use left join so drugnames are all present, but have NA columns in moanamed_df
#' merged_df <- merged_df_drugnamed %>% left_join(
#'   moanamed_df,
#'   by = c(
#'     "ReferenceKey",
#'     "PrescriptionStartDate",
#'     "PrescriptionEndDate"
#'   ),
#'   suffix = c("_drugnamed", "_moa")
#' )
#' 
#' # merged_df_drugnamed %>% filter(ReferenceKey == 1319963) %>% print(n = 100)
#' # moanamed_df %>% filter(ReferenceKey == 1319963) %>% print(n = 100)
#' # merged_df %>% filter(ReferenceKey == 1319963) %>% print(n = 100)
#' 
#' # take the ones with NA (i.e. the entries that did not match) from merged_df, filter them out first
#' na_rows <- merged_df %>% filter(is.na(DrugName_clean_moa))
#' 
#' # for each ReferenceKey in that filtered merged_df, also grab the rows from moanamed_df
#' 
#' #' fill_na_with_moa
#' #'
#' #' @param a take the same Referencekey pt with NA entries, from the na_rows
#' #' @param b from the same ReferenceKey, the entire moa 
#' #'
#' #' @return a but instead of NA, those are filled with where the interval matches in b, the moa
#' #' @export
#' #'
#' #' @examples
#' fill_na_with_moa <- function(a, b) {
#'   # get the drug intervals where moa is NA; we don't use gap_output because that would be NA for all pt at the end anyways
#'   interval_a <- a %>% pull(PrescriptionInterval_drugnamed)
#'   
#'   # check interval_a against every interval in b$PrescriptionInterval_moa, to get the index which matches b
#'   overlaps <- sapply(interval_a, function(x) {
#'     which(x %within% b$PrescriptionInterval_moa)
#'   })
#'   
#'   # for the same drug class there could have been diff drugs taken, so would expect duplicate rows in b_cols
#'   b_cols <- b[overlaps,] %>% rename(
#'     DrugName_clean_moa = DrugName_clean,
#'     duration_moa = duration,
#'     gap_output_moa = gap_output
#'   ) %>% select(DrugName_clean_moa, duration_moa, gap_output_moa, PrescriptionInterval_moa)
#'   
#'   # drop those columns from a, and take those columns from b; filled as in the previous NA are now filled with values
#'   filled_rows <- a %>% 
#'     select(-DrugName_clean_moa, -duration_moa, -gap_output_moa, -PrescriptionInterval_moa) %>% 
#'     cbind(b_cols)
#'   
#'   return(filled_rows)
#' }
#' 
#' 
#' filled_rows_list <- list()
#' 
#' # for debugging
#' # ref <- 2154976
#' # ref <- 2209539
#' # debug(fill_na_with_moa)
#' # fill_na_with_moa(a, b)
#' # merged_df %>% filter(ReferenceKey == 2154976) %>% View()
#' 
#' for (ref in unique(na_rows$ReferenceKey)) {
#'   # create the two rows with refkey, only for that pt
#'   # taken from na_rows since that has already been filtered
#'   
#'   a <- na_rows %>% filter(ReferenceKey == ref) 
#'   b <- moanamed_df %>% filter(ReferenceKey == ref)
#'   
#'   # append the output to the list
#'   filled_rows_list <- c(filled_rows_list, list(fill_na_with_moa(a, b)))
#' }
#' 
#' # combine all the elements in the list into a single data frame
#' filled_rows_df <- bind_rows(filled_rows_list)
#' 
#' # combine with the rows that are did not have NA
#' merged_df <- rbind(merged_df %>% filter(!is.na(DrugName_clean_moa)), filled_rows_df) %>% arrange(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate)
#' 
#' # rename drug, moa, for ease of reference and plotting later
#' merged_df <- merged_df %>% 
#'   rename(drug = DrugName_clean_drugnamed,
#'          moa = DrugName_clean_moa)