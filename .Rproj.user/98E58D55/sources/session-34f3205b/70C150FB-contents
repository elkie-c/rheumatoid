
# minimal reproducible example --------------------------------------------

# df <- df %>% 
#   select (PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) %>% 
#   unique()
# 
# df %>% select(DrugName_clean) %>% pull()
# 
# df <- data.frame(
#   PrescriptionStartDate = c("2019-03-27", "2019-03-27", "2019-03-27", "2019-08-14", "2019-08-14", "2019-08-14", "2018-02-14", "2018-06-20", "2018-12-19", "2018-12-19", "2018-12-19", "2017-01-18", "2017-08-30", "2016-02-17", "2016-08-03", "2013-07-25", "2013-11-20", "2014-04-23", "2014-06-18", "2014-11-13", "2015-04-15", "2015-08-12", "2020-03-02", "2020-03-02", "2020-03-02", "2020-09-30", "2020-09-30", "2020-09-30", "2021-05-05", "2021-05-05", "2021-12-08", "2021-12-08"),
#   PrescriptionEndDate = c("2019-08-13", "2019-08-13", "2019-08-13", "2020-02-25", "2020-02-25", "2020-02-25", "2018-06-19", "2018-12-18", "2019-04-09", "2019-04-09", "2019-04-09", "2017-08-15", "2018-02-13", "2016-08-02", "2017-01-17", "2013-11-13", "2014-04-22", "2014-06-17", "2014-11-11", "2015-04-15", "2015-08-11", "2016-02-16", "2020-10-04", "2020-10-04", "2020-10-04", "2021-05-04", "2021-05-04", "2021-05-04", "2021-12-14", "2021-12-14", "2022-05-24", "2022-05-24"),
#   DrugName_clean = c("HYDROXYCHLOROQUINE", "LEFLUNOMIDE", "METHOTREXATE", "HYDROXYCHLOROQUINE", "LEFLUNOMIDE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "HYDROXYCHLOROQUINE", "LEFLUNOMIDE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "METHOTREXATE", "HYDROXYCHLOROQUINE", "LEFLUNOMIDE", "METHOTREXATE", "HYDROXYCHLOROQUINE", "LEFLUNOMIDE", "METHOTREXATE", "LEFLUNOMIDE", "METHOTREXATE", "LEFLUNOMIDE", "METHOTREXATE"))     

# gap_output <- vector("numeric", nrow(df)-1) -----------------------------------
#' calculate gaps 
#'
#' @param df 
#' @input for now takes the same drug, df under prescription, but the data should be arranged by prescription date
#' @return gap_output which contains the number of days of gap between entry to the next (if not 0)
#' @export
#'
#' @examples df %>% filter(DrugName_clean == "METHOTREXATE")
#' gap(df)

gap <- function(df) {
  df <- df[order(df$PrescriptionStartDate), ] # may be redundant but must make sure ordered)
  
  gap_output <- c(rep(5, nrow(df)-1), NA) # just to init to a different value to see if the code is really working
  # you need the NA later for combining the gap_output with the df, or else would just be missing value if we don't put the NA there
  
  for (i in seq(nrow(df))[-nrow(df)]) { # remove the last item, i.e. the nrow, because i+1 would not exist
    
    if (df$PrescriptionEndDate[i] > df$PrescriptionStartDate[i+1]) {# so if the current end date larger than next start date, already means overlap
      # of if less than a threshold
      gap_output[i] <- 0
    } else {
      gap_output[i] <-  df$PrescriptionStartDate[i+1] - df$PrescriptionEndDate[i] # the difference would be the gap; so gap on row n, would mean the break between row n + 1 startdate and row n enddate
    }
  }
  df$gap_output <- gap_output
  return(df)
}

# so you have an output df from the above. Next step is MUTATE to add the boolean (in 01_wrangle)


# merge dates -------------------------------------------------------------
#' merge dates
#' so based on the output of gap_output, you have the dates and gaps. Then this function will merge based on the boolean. 
#' @param df 
#'
#' @return merged dates, a simplified df with three columns of PrescriptionStartDate, PrescriptionEndDate, and DrugName_clean
#' @export
#' what it does is gets the period. Uses careful initialisation so the first period starts with the PrescriptionStartDate of i=1, until there is a break in the period which is given by the FALSE boolean of the gap_boolean column. e.g. if there is a false at i=4, the first period would be from i=1 start date to i=3 end date. The initial_i will then become startdate of i=4. This repeats until we get the next FALSE or reach the end, the last row, where the gap_boolean will be = NA. That is the case of elseif.
#' @examples merge_dates(df)
merge_dates <- function(df) {
  
  # as you do NOT know the length of output since it depends on the number of gaps, we'll have to use rbind which is more clumsy
  output <- data.frame(
    PrescriptionStartDate = double(),
    PrescriptionEndDate = double(),
    DrugName_clean = character()
  )
  
  initial_i <- 1 # so for the first period of time, you assume starting with start date of i = 1 since df is already arranged in ascending order
  for (i in seq(nrow(df))) {
    
    if (!is.na(df$gap_boolean[i]) && df$gap_boolean[i] == TRUE) { # if it reaches where there is a gap
      # so let's say i = 4 you hit a TRUE, the first time you want the period from initial_i i.e. i = 1 StartDate to i = 3 EndDate (because the break is between EndDate of i=3 and startDate of i=4
      # PrescriptionEndDate[i] checked
      output <- rbind(output, 
                      setNames(data.frame(df$PrescriptionStartDate[initial_i], max(df$PrescriptionEndDate[1:i]), df$DrugName_clean[i]), nm = colnames(output))
      ) # use setNames to prevent the data.frame rigging the actual names we wanted
      
      # whenever you created a new period for the drug, you need to reset the initial_i so it starts at i+1 rather than at initial_i = 1, because at i is where the new beginning of the next period is; if you start at i instead of i+1, recall we already had the period up to endDate of i, so there would be overlap there
      initial_i <- i + 1
      
      
    } else if (is.na(df$gap_boolean[i])) { # or when is.na() exists, signals we reached the end
      
      # unlike if, we take df$PrescriptionEndDate[i] to be the end rather than i-1, because NA does not signify any gap
      output <- rbind(output, 
                      setNames(data.frame(df$PrescriptionStartDate[initial_i], max(df$PrescriptionEndDate[1:i]), df$DrugName_clean[i]), nm = colnames(output))
      )  # see explanation for max below
    }
  }
  return(output)
}

# Explanation for taking max(df$PrescriptionEndDate[1:i])
# so from RefKey 2154976, where the penultimate entry has a prescriptionenddate later than that of the last row
# then when you take initial_i from n-1, and i from end date of n, where n is the number of rows, you will miss the last end date which is actually n-1

# solution is to alter taking the prescriptionenddate such that you take the maximum prescription end date inclusively above row i
# because if there is a prescription end date LATER than the current row somewhere above, call that row x. You are at row n. Then prescription start date of row n must be later than row x. And then for this awkward situation, prescriptionenddate of row n is before row x. Which means n is located inside x. And since we already ranked by prescription startdate, we have the earliest start date by the initial_i counter, we could safely take the last enddate. 


# gap_merge_per_drug ------------------------------------------------------
#' gap_merge_per_drug
#'
#' @param df 
#' @param threshold threshold <- 30 # arbitrary threshold for which discontinuity more than threshold is considered a break from the drug use
#' this function will take the df of EACH drug e.g. all the different periods of methotrexate for the same ReferenceKey and then call two other functions, which are found in 02_functions.R, to yield output. First it identifies gap in the drug prescription, then it merges them based on whether they meet a threshold
#' this is a recursive function to merge gaps until there are no gaps in case of ranges that are included in each other; quite tricky
#' @return
#' @export
#'
#' @examples if you want MRE, go to the MRE in functions.R, and then filter to get ONE drug's df
gap_merge_per_drug <- function(df, threshold = 14) {
  # function is in 02_functions script, so apply the gap function there
  df <- gap(df)
  # create a boolean column to check if more or less than threshold, decide if the gap is significant
  df <- df %>% 
    mutate(
      gap_boolean = case_when(
        gap_output <= threshold ~ FALSE,
        gap_output > threshold ~ TRUE,
        TRUE ~ NA
      )
    )
  
  # merge_dates is from 02_functions.R
  output <- merge_dates(df) # output of a df of just a few rows of the merged dates
  
  # previously noted a problem with adalimumab as there are many prescriptions overlapping each other, some completely nested by another, so one round of applying gap_merge_per_drug was insufficient; therefore, here I have modified the code to repeat the merging action of gap still produces a gap smaller than the threshold
  
  # create gap(output)
  temp <- gap(output)
  temp <- temp %>% 
    mutate(
      gap_boolean = case_when(
        gap_output <= threshold ~ FALSE,
        gap_output > threshold ~ TRUE,
        TRUE ~ NA
      )
    )
  
  # only take the non-na values; only return output if ALL of them are TRUE or else indicates presence of gaps
  
  
  if (!all(temp$gap_boolean[ !is.na(temp$gap_boolean) ])) { # if any of those gaps, this whole expression (with ! negating at the start) would return a true
    
    # recursive, call back this gap_merge_per_drug, using output (where we did not add the columns)
    gap_merge_per_drug(output)
    
  } else{ 
  return(output)
  }
}
# 
# undebug(gap_merge_per_drug)
# debug(merge_dates)


# decompose dates ---------------------------------------------------------
#' decompose dates
#'
#' @param drug_period 
#' @param dates 
#' this function decompose dates. So give it a row drug_period (PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) and a list of dates, then it can decompose the PrescriptionStart and PrescriptionEndDate into the interval periods based on the list of dates. e.g. if Start is Day 10, End is Day 100, list of dates is Day 20, Day 50, you will get a df of Day 10 - 20, Day 20 - 50, Day 20 - 100. 
#' @return
#' @export
#'
#' @examples
# drug_period <- data.frame(
#   PrescriptionStartDate = as.POSIXct(c("2013-07-25", "2018-12-19", "2018-12-19")),
#   PrescriptionEndDate = as.POSIXct(c("2022-05-24", "2021-05-04", "2022-05-24")),
#   DrugName_clean = c("METHOTREXATE", "HYDROXYCHLOROQUINE", "LEFLUNOMIDE")
# )
# dates <- c("2013-07-25", "2018-12-19", "2022-05-24", "2021-05-04")
# dates <- as.Date(unique(dates))
# 
# decompose_dates(row = drug_period[1,], dates)
# decompose_dates(row = drug_period[2,], dates)
# decompose_dates(row = drug_period[3,], dates)

decompose_dates <- function(row, dates) {
  # obtain the dates which are within the wanted period, using boolean; returns error for atomic start date?
  filtered_dates <- dates[dates %within% lubridate::interval(row$PrescriptionStartDate, row$PrescriptionEndDate)]
  
  # filtered_dates <- dates[dates %within% lubridate::interval(row["PrescriptionStartDate"], row["PrescriptionEndDate"])]
  
  # rank in ascending order
  ordered_dates <- filtered_dates[order(filtered_dates)]
  
  if (length(filtered_dates) == 1) {
    return(row) # because if you went into the else, would encounter problem with -1 since we only have one row. Since if the filtered dates only one, e.g. if the drug was given for one day, or very short period and does not overlap any other dates, it just returns the same row
  } else {
    # Create a data frame with the drug periods that emerge from this ranked date
    df <-
      data.frame(
        PrescriptionStartDate = ordered_dates[1:length(ordered_dates) - 1],
        PrescriptionEndDate = ordered_dates[2:length(ordered_dates)],
        DrugName_clean = rep(row$DrugName_clean, length(ordered_dates) -
                               1)
      )
    return(df)
  }
}


# df <- data.frame(PrescriptionStartDate = c("2013-07-25", "2018-12-19", "2018-12-19"),
#                  PrescriptionEndDate = c("2022-05-24", "2021-05-04", "2022-05-24"),
#                  DrugName_clean = c("METHOTREXATE", "HYDROXYCHLOROQUINE", "LEFLUNOMIDE"))



# extract_traj ------------------------------------------------------------
# load the functions inside of 02_functions.R since extract_traj calls them
extract_traj <- function(df) {
  df$PrescriptionStartDate <-
    as.POSIXct(df$PrescriptionStartDate, format = "%Y-%m-%d")
  df$PrescriptionEndDate <-
    as.POSIXct(df$PrescriptionEndDate, format = "%Y-%m-%d")
  
  # sort the data by PrescriptionStartDate
  # df <- df[order(df$PrescriptionStartDate),]
  
  df <- df %>%
    arrange(PrescriptionStartDate) %>% 
    select (PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) %>%
    unique()
  
  
  
  # EG ONLY make sure to convert df into single df
  # df <- df %>% filter(DrugName_clean == "METHOTREXATE")
  
  split_df <- split(df, f = df$DrugName_clean)
  
  # Reduce after lapply so gets you the drugs, and the clean period for which it was used!
  drug_period <-
    Reduce(
      x = lapply(X = split_df, FUN = gap_merge_per_drug),
      f = "rbind",
      accumulate = FALSE
    )
  
  
  # theoretically, if you had monotherapy (which may have more than one periods), you could just output here, and give a duration. But I decided to let it run the rest of the code if it does not take up too much time, and outputs data in the desirable format
  
  # because rbind so we lost the order; rank again
  drug_period <- drug_period %>% arrange(PrescriptionStartDate)
  
  # pull out all the dates
  dates <-
    drug_period %>% select(PrescriptionStartDate, PrescriptionEndDate) %>% unlist()
  dates <- as.POSIXct(dates) # or else not recognizable format
  dates <-
    as.Date(unique(unlist(dates))) # just in dates format, removing the time format; so no longer a df, not just vector of dates
  
  
  
  # did not use apply as that leads to issues with atomic vector, not the same as for loop easier
  # create the empty output for our next step
  output <- data.frame(
    PrescriptionStartDate = as.Date(character()),
    PrescriptionEndDate = as.Date(character()),
    DrugName_clean = character(),
    stringsAsFactors = FALSE
  )
  
  # call decompose_dates function from 02_functions.R
  # loop to repeat this on all rows
  for (i in seq(nrow(drug_period))) {
    temp_df <- decompose_dates(drug_period[i,], dates)
    output <- rbind(temp_df, output)
  }
  
  # arrange also by drugname_clean so we get cd28+cdmard and not repeat with cdmard+cd28
  output <- output %>%
    group_by(PrescriptionStartDate, PrescriptionEndDate) %>%
    arrange(PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) %>%  # by alphabetical order of DrugName_clean
    distinct(PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) %>% 
    summarise(DrugName_clean = paste(DrugName_clean, collapse = "+"), .groups = 'drop') %>% # .groups just to silence the message, probably redundant here tho the code
    ungroup() %>%
    arrange(PrescriptionStartDate)
  
  output$duration <-
    output$PrescriptionEndDate - output$PrescriptionStartDate
  
  # so e.g. if allegedly, two drug regimen for 2 days, before switching to 3 drug regimen → this is quite insignificant, then can abandon the shorter duration regimen (more downstream rather than edit the dates earlier); not done yet, depends
  
  # sometimes you get a situation with allegedly a drug was prescribed for one day. Which is not actually a switch in drug regimen, but a drug prescribed for an additional day. e.g. ReferenceKey = 100031. So we figure out a way to clean that by filter using duration BUT some doctor can give many ONE-DAY infliximab, so we would have unintentionally removed that
  # output <- output %>% filter(duration > 7)
  
  # apply again this gap function, just to figure out the gap between the changes in regimen; maybe useful later if we consider some gaps too short to be a so-called "switch" in regimen
  output <- gap(output) 
  return(output)
}



