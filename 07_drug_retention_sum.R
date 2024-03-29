# script called by 06_plots and stats

# takes the single_drug_rows and follow_up (only two possible values); output_row is output cumulative sum with drug name
single_drug_list <- split(single_drug_rows, single_drug_rows$ReferenceKey)


#' prescription_overlap_boolean
#'
#' @param j 
#' j is the single_drug df
#' @return boolean vector if met the minimal 2 years requirement of follow-up; NA if does not meet
#' @export
#'
#' @examples # sample with more than one rows
# which(names(single_drug_list) == 2071268)
# j <- single_drug_list[[1]]
# prescription_overlap_boolean(j)
# debug(prescription_overlap_boolean)

# debugging only
# for (i in seq(length(single_drug_list))) {
#   print(prescription_overlap_boolean(single_drug_list[[i]]))
#   # print(i)
# }


prescription_overlap_boolean <- function(j, follow_up) {
  # prescription start date of the first entry
  start <- j[1, "PrescriptionStartDate"] %>% pull()
  # prescription end date of the last entry
  end <- j[nrow(j), "PrescriptionEndDate"] %>% pull()
  int <- interval(start, end)
  
  # this is for the drug; NOT follow up, but whether switched
  int_length <- as.numeric(int, "days")
  
  # last date record would take into account of death
  # period between start and last_date as start reflects the start of the btsDMARD in question, so this is whether the period is 2yrs for that specific drug
  last_record <- j[1, "last_record"] %>% pull()
  
  # drug specific follow up, so from first date of drug to last record
  drug_specific_fu <- as.numeric(interval(start, last_record), "days")
  
  if (follow_up == "two_years") { # depending on follow_up variable
    # if the drug_specific follow up is < 730
    if (drug_specific_fu < 730) {
      return(c(NA, NA, NA, NA, NA)) # if just NA, theoretically can recycle. But if more NA than drug entries e.g. adalimumab_s then have problem rbinding
    } else {
      boolean_vector <-
        c(int_length > 0,
          int_length > 182,
          int_length > 365,
          int_length > 547,
          int_length > 730)
      
      return(boolean_vector)
    }
  } else if (follow_up == "one_year") {
    if (drug_specific_fu < 365) {
      return(c(NA, NA, NA, NA, NA)) # one less NA
    } else {
      boolean_vector <-
        c(int_length > 0,
          int_length > 91,
          int_length > 182,
          int_length > 273,
          int_length > 365)
      
      return(boolean_vector)
    }
  } else {
    print("Follow up needs to be one_year or two_years. Please retry.")
  }
  
}

# your row would be only the same drug depending on what was fed into the function so can safely reduce the rows
# no need a very careful function for merging since no gaps expected
boolean_matrix <- Reduce(
  x = lapply(X = single_drug_list, 
             follow_up, 
             FUN = prescription_overlap_boolean),
  f = "rbind",
  accumulate = FALSE
)

boolean_df <- as.data.frame(boolean_matrix)

# example to make sure column sum works
# df <- data.frame(`0_month` = c(TRUE, TRUE, TRUE, TRUE, NA, TRUE),
#                  `6_month` = c(TRUE, TRUE, TRUE, TRUE, NA, TRUE),
#                  `12_month` = c(TRUE, TRUE, TRUE, TRUE, NA, FALSE),
#                  `18_month` = c(TRUE, TRUE, TRUE, TRUE, TRUE, NA),
#                  `24_month` = c(TRUE, TRUE, TRUE, FALSE, NA, FALSE))
# 
# df %>% summarize(across(everything(), ~sum(., na.rm = TRUE)))

# row sum
output_row <- boolean_df %>%
  summarize(across(everything(), ~sum(., na.rm = TRUE)))

# add back drug name via i as the first column
output_row <- cbind(i, output_row)

# Remove row names
rownames(output_row) <- NULL