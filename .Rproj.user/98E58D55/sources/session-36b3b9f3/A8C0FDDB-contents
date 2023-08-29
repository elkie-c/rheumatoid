# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
                 zoo, # adding back non-NA values
                 ggpubr,
                 mgsub, #for multiple substitutions
                 readxl, # read the excel drug list
                 hrbrthemes, 
                 RColorBrewer,
                 # ggalluvial, # treatment trajectory sankey)
                 plotly, # heatmaps
                 svglite, # save svgs
                 # MASS, # linear regression stepwise # select disrupts dplyr
                 # ggforce # for sankey
                 knitr, # for tables
                 kableExtra
)

# setwd("/Users/elsiechan/Documents/GitHub/rheumatoid")

# replace with your path to kuan's folder of rds
path <- "/Users/elsiechan/Desktop/kuan_folder"

setwd(path)
death <- readRDS("Death.RDS")
diagnosis <- readRDS("Diagnosis.RDS")
inpatient <- readRDS("Inpatient.RDS")
prescription <- readRDS("Prescription.RDS")



# read excel sheet of drug list -------------------------------------------
# Set the file path of the Excel file
excel_file <- "Drug list.xlsx"
# sheet_names <- excel_sheets(excel_file)

# Read each sheet as a separate data frame
bio_df <- read_excel(excel_file, sheet = 1)
bioo_df <- read_excel(excel_file, sheet = 2)
# cdmard_df <- read_excel(excel_file, sheet = 3, col_names = FALSE, trim_ws = TRUE)
# cdmard <- toString(cdmard_df)

# fill with previous non-missing value to give back the value to NA
bio_df <- tidyr::fill(bio_df, Ingredient)

bioo <- bioo_df$Agent
# manually extracted because the formating of the sheet is a little crude
cdmard <- c("Sulfasalazine", "Mycophenolate", "Methotrexate", "Leflunomide", "Hydroxychloroquine", "Cyclophosphamide", "Ciclosporin", "Azathioprine", "Apremilast", "Cyclosporin")
cdmard <- toupper(cdmard)


# read excel sheet for disease cp -----------------------------------------
weight_df <- read_excel('MMI.xlsx')


# find the row index where DiseaseEntity_260292 is "Dyspepsia"
row_index <- which(weight_df$DiseaseEntity_26029213 == "Dyspepsia")

# set the value of the ICD9_unref column in the relevant row, because problematic cell
weight_df[row_index, "ICD9_unref"] <- "536.8"


weight_df$Assigned_weight <- as.numeric(weight_df$Assigned_weight)


# (no need) clean weight_df (clean ICD codes by expanding hyphen and x)---------------------------------------------------------
# remove empty space
weight_df$ICD9_unref <- str_replace_all(weight_df$ICD9_unref, "\\s+", "")
weight_df$DiseaseEntity_26029213 <- str_replace_all(weight_df$DiseaseEntity_26029213, "\\s+", "")

# these two functions written, first to expand out the hyphen, then to expand out the x

# if there is hyphen, expand the label to create multiple copies of the inclusive numbers, with .x
expand_hyphen <- function(icd) {
  # Check if the input contains a hyphen
  if (grepl("-", icd)) {
    # Split the input by the hyphen
    range <- strsplit(icd, "-")[[1]]
    
    # Extract the numeric portion of the values
    start_val <- as.numeric(gsub("[^0-9.]", "", range[1]))
    end_val <- as.numeric(gsub("[^0-9.]", "", range[2]))
    
    # Create a sequence of values from start_val to end_val
    vals <- seq(start_val, end_val, by = 1)
    
    # add back the .x so later the steps can create the multipels of it
    vals <- paste0(vals, ".x")
    
    return(vals)
  } else {
    return(icd) # so if hyphen is not grepped, just give the original icd
  }
}

expand_x <- function(icd) {
  # Check if the input contains a hyphen
  if (grepl("x", icd)) {
    # Extract the numeric portion of the input
    val <- gsub("[^0-9.]", "", icd)
    
    # Create a sequence of values from val to val+0.9
    # val <- "428.1."
    
    # paste to get all the values from 0 to 9
    vals <- paste(val, seq(0, 9), sep = "")
    
    return(vals)
  } else {
    return(icd)
  }
}

weight_df$ICD9_clean <- NA

for (i in seq(nrow(weight_df))) {
  icd_char <- weight_df[i, "ICD9_unref"] %>% pull()
  
  # split by comma to get a character vector for each icd_char
  icd_char <- strsplit(icd_char, ",")[[1]]
  
  expand_hyphen_output <- character(0)
  
  for (icd in icd_char) {
    # icd_char[icd_char == icd] <- paste0(expand_hyphen(icd)) # update each index
    expand_hyphen_output <- c(expand_hyphen_output, expand_hyphen(icd))
  }
  
  expand_x_output <- character(0)
  for (icd in expand_hyphen_output) {
    # icd_char[icd_char == icd] <- paste0(expand_hyphen(icd)) # update each index
    expand_x_output <- c(expand_x_output, expand_x(icd))
  }
  
  weight_df[i, "ICD9_clean"] <- paste(expand_x_output, collapse = ",") # so set the NA, ICD9_clean to this clean value; collapse turns it into one string
}

# weight_df
# add these two numbers just to simulate it, MRE
# icd_char <- paste0(weight_df[6, 2] %>% pull(), ",277.x,", "800")
# icd_char <- strsplit(x, ",")[[1]]





# clean diagnosis ------------------------------------------------------
# firstly, only consider pt if diagnosed with RA at least at some point in life
# the following line shows that other than rheumatoid arthritis, you also have rheumatoid nodule arthritis

# rename to Reference.date, since second time diagnosis.rds the column names are renamed
diagnosis <- diagnosis %>% rename(Reference.Date. = date)

ra_vector <- unique(grep(
  pattern =
    "714\\.[0128]",
  x = diagnosis$description,
  value = TRUE
))

# exclude this strange item called Screen-rheumatoid
# ra_vector <- grep(pattern = "^(?!.*Screen)", x = ra_vector, value = TRUE, perl = TRUE)

# select out the seronegative, glomerulonephritis
ra_vector <- grep(pattern = "glomerulonephritis|seronegative", x = ra_vector, perl = TRUE, value = TRUE, invert = TRUE)

# there are some strange diagnosis with rheum, and for the benefit of doubt (and discussion), they are as follows: now the decision is whether you would like to keep these? Could modify the expression above to do that
# Find the strings that contain "rheum" but not in in ra_vector (which we will use for analysis)
not_ra_vector <- unique(grep(
  pattern =
    "rheum|RA",
  x = diagnosis$description,
  value = TRUE
))

not_ra_vector[!not_ra_vector %in% ra_vector] # so these are the strings

# for each pt diagnosed with RA, get the earliest reference date for the RA diagnosis
# Group by patient and get earliest reference date
earliest_ra <- diagnosis[diagnosis$description %in% ra_vector, ] %>%
  group_by(ReferenceKey) %>%
  summarize(first_ra = min(Reference.Date.))


# double check only
# earliest_ra[earliest_ra$Reference.Key. == 5872, ]
# diagnosis[diagnosis$Reference.Key. == 5872,]

# update the table with a column on the earliest RA diagnosis if any
diagnosis_sub <- dplyr::left_join(x = diagnosis,
                                  y = earliest_ra,
                                  by = "ReferenceKey") %>%  # before next line will see some NA dates for those not diagnosed with our definition of RA
  filter(!is.na(first_ra)) # to remove those who do not have our definition of RA
  # filter(Reference.Date. <= first_ra) # lesser or equal to so we keep the RA columns and do not exclude them


# obtain a list of all biologics indications
# when cleaning diagnosis df, we need to remove pt with diagnosis of ANY biologics indications prior to their first date of RA (e.g. cancer or autoimmune disease), in case they may have received biologics for a purpose other than RA. I need a string vector for those indications

# taken from https://ard.bmj.com/content/annrheumdis/suppl/2021/10/22/annrheumdis-2021-221571.DC1/annrheumdis-2021-221571supp001_data_supplement.pdf  they are as follows:
# reactive arthritis
#   099\.3|
#   711\.[13]|
#   716\.[4569]|
#   719\.4|
#   # SLE
#   710\.0|
#   # SA
#   720\.0|
#   # psoriasis
#   696|
#   # multiple sclerosis
#   340|
#   55[56]|
#   # neoplasm
#   14[0-9]|15[0-9]|
#   16[0-5]|
#   17[0-2]|
#   17[4-6]|
#   179|
#   18[0-9]|
#   19[0-5]|
#   20[0-9]

other_biologic <- grepl(
  x = diagnosis_sub$description,
pattern = "099\\.3|
711\\.[13]|
716\\.[4569]|
719\\.4|
710\\.0|
720\\.0|
696|
340|
55[56]|
14[0-9]|15[0-9]|
16[0-5]|
17[0-2]|
17[4-6]|
179|
18[0-9]|
19[0-5]|
20[0-9]") # you need double backslashes in R, as \ is also escape character in R itself

# obtain the reference key of those with ANY match with the other_biologic indications
unwanted_ra <- diagnosis_sub[other_biologic, "ReferenceKey"] %>% pull()

# how many pts did we exclude this way?
print(paste0("At first, we have ", length(unique(diagnosis_sub$ReferenceKey)), " number of patients who fulfil our definition of RA"))

print(paste0("Then, we removed patients who were diagnosed with conditions that indicate the use of biologics at ANY point in their life. Now, there are ", length(unique(unwanted_ra)), " patients who meet this condition. Example of their ID include the following:"))

print(paste0(unique(unwanted_ra)[1:5], collapse = ", "))

print(paste0(
  "So the number of patients who remain in the analysis is ",
  length(unique(diagnosis_sub$ReferenceKey)) - length(unique(unwanted_ra))
))




# diagnosis_sub <- diagnosis_sub %>% 
#   filter(!ReferenceKey %in% unwanted_ra)
# 
# diagnosis_sub$Patient.Type..IP.OP.A.E.. <- trimws(diagnosis_sub$Patient.Type..IP.OP.A.E..)

# get weighting score -----------------------------------------------------
# weight_df
# diagnosis_sub
diagnosis_sub %>% dplyr::select(All.Diagnosis.Description..HAMDCT..)


# this code is PROBLEMATIC and only selects first regex match; instead take the regex expression which is already there by default

diagnosis_sub$ICD <- regmatches(
  x = pull(diagnosis_sub[, "description"]),
  m = regexpr(
    pattern = "(?<=\\()[\\d\\.EV]+(?=:)",
    # after ( but not including, \ the \ character, not including the colon; [^\\(]*$ to take the last occurrence of the open bracket, don't want anymore brackets like the case of (lymph node) instead of (ICD)
    # EV because some ICD begins with that followed by the bracket
    text = pull(diagnosis_sub[, "description"]),
    perl = TRUE
  )
)
  
# diagnosis_sub[899, ] # example with the bracket lymph node
# for debugging to see where regex failed to match
# diagnosis_sub[, "All.Diagnosis.Description..HAMDCT.."][grep(pattern = "(?<=\\()[\\d\\.EV]+(?=:)", x = diagnosis_sub[, "All.Diagnosis.Description..HAMDCT.."], perl = TRUE, invert = TRUE)]

# extra steps to check after removing the 0 is indeed the intended effect; later turned out is useless as 250.00 is not the same as 250.0
# diagnosis_sub$ICD_no_0 <- gsub(pattern = ".00", x = diagnosis_sub$ICD, replacement = ".0", fixed = TRUE)
# diagnosis_sub %>% filter(ICD != ICD_no_0) %>% select(ICD, ICD_no_0, All.Diagnosis.Description..HAMDCT..)



# you can't rely on the All.Diagnosis.Code..ICD9.. because at some point after they all turn into english text !?
# diagnosis_sub <- diagnosis_sub %>% rename(ICD = All.Diagnosis.Code..ICD9..)

# which(nchar(diagnosis_sub %>% pull(ICD)) > 8)
# pull(diagnosis_sub, ICD)[which(nchar(diagnosis_sub %>% pull(ICD)) > 8)]
# diagnosis_sub[which(nchar(diagnosis_sub %>% pull(ICD)) > 8), ]
# options(max.print = 100000)


# add before_ra column, boolean for entry whether it was before the first_ra diagnosis
diagnosis_sub <- diagnosis_sub %>% mutate(before_ra = if_else(Reference.Date. < first_ra, T, F))



#' cal_timepoint_score
#' give it a timepoint. e.g. 5 days. Then look for ICDs that happened before, inclusively, 5 days after the diagnosis of first_ra. Takes those ICD score. Uses the weight_df matrix to assign weight, and outputs a df with score for each Referencekey
#' @param days variable for the number of days. If set to 0, then would be the score BEFORE RA (not including the day of RA). If set to arbitrarily large number, like 100000000, would be the current i.e. most updated ICDs of the pt, all included.
#' @param diagnosis_sub 
#' @param weight_df previous weight_df. The weights come from publications
#'
#' @return a df called scores_df which has reference key and weighting score for each reference key
#' @export
#'
#' @examples
cal_timepoint_score <- function(days, diagnosis_sub, weight_df, prefix = "score_before_") {
  df <- diagnosis_sub %>%
    mutate(
      ICD_before_timepoint = case_when(
        Reference.Date. <= first_ra + days ~ ICD, # preserve ICD code only if the ICD happened more than days (variable) after first_ra
        TRUE ~ "NA")
    ) %>% 
    group_by(Reference.Key.) %>%
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


# score before RA; join the new scores df
scores_df <- cal_timepoint_score(0, diagnosis_sub, weight_df)

# score inclusive of the day of dx of RA (in case pt was diagnosed for problems on top of RA on that consultation day)
scores_df <- left_join(cal_timepoint_score(1, diagnosis_sub, weight_df), scores_df, by = "Reference.Key.")

# score 1 month after RA
scores_df <- left_join(cal_timepoint_score(31, diagnosis_sub, weight_df), scores_df, by = "Reference.Key.")

# score 6 months after RA
scores_df <- left_join(cal_timepoint_score(182, diagnosis_sub, weight_df), scores_df, by = "Reference.Key.")

# score 1 year after RA
scores_df <- left_join(cal_timepoint_score(365, diagnosis_sub, weight_df), scores_df, by = "Reference.Key.")

# the score after infinitely long i.e. up to current day (273 years)
scores_df <- left_join(cal_timepoint_score(100000, diagnosis_sub, weight_df), scores_df, by = "Reference.Key.")

# saveRDS(object = scores_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/scores_df.rds")
# scores_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/scores_df.rds")


# one potential problem is if pt was diagnosed e.g. with RA at 2022 December. A year from then would be 2023 Dec when we do not have the data yet, but our data would make it appear as tho the pt has not had any worsening. But we do not know the end-point. Unless first_ra we use that to ignore the consideration of data 1 year from first_ra for the score score_before_365 days. So we could do that downstream using first_ra. 


# clean prescription df--------------------------------------------------------------
# from read excel sheet, we have obtained the drug list already. The useful variables would be biosimilar_df, bioo, and cdmard

# filter the prescription to obtain only rows of those three biologics (for DEMONSTRATION only because we are not using this table)
# prescription_sub <- prescription %>% 
#   filter(grepl(pattern = paste(unique(bio_df$Ingredient), collapse = "|"),
#                x = DrugName))

# this table shows there are many labels of just the biologic, but not the brandname. Which means we do not know if those are actually bios or bioo
# table(prescription_sub$DrugName)

# standardize the drug name into three new columns: ingredient, brand, bioo_or_bios (bios or bioo?)
# firstly, obtain the character vector for each of the three biologics
infliximab <- bio_df %>% filter(Ingredient == "INFLIXIMAB") %>% select(`Brand name`) %>% pull()
infliximab <- unique(c("INFLIXIMAB", infliximab))
infliximab <- gsub(pattern = "[[:space:]\u00A0]", replacement = "", x = infliximab)

adalimumab <- bio_df %>% filter(Ingredient == "ADALIMUMAB") %>% select(`Brand name`) %>% pull()
adalimumab <- unique(c("ADALIMUMAB", adalimumab))
adalimumab <- gsub(pattern = "[[:space:]\u00A0]", replacement = "", x = adalimumab)

rituximab <- bio_df %>% filter(Ingredient == "RITUXIMAB") %>% select(`Brand name`) %>% pull()
rituximab <- unique(c("RITUXIMAB", rituximab))
rituximab <- gsub(pattern = "[[:space:]\u00A0]", replacement = "", x = rituximab)

# now, create the ingredient column
# table(prescription_sub$DrugName)
prescription_sub <- prescription %>% 
  mutate(
    ingredient = case_when(
      stringr::str_detect(DrugName, paste(infliximab, collapse = "|")) ~ "INFLIXIMAB", # using string infliximab, because sometimes only brand name, but not ingredient name given. So you would miss that if you hadn't use this string
      stringr::str_detect(DrugName, paste(adalimumab, collapse = "|")) ~ "ADALIMUMAB",
      stringr::str_detect(DrugName, paste(rituximab, collapse = "|")) ~ "RITUXIMAB",
      TRUE ~ NA_character_)
  )

# secondly, get the character strings for bios or bioo
bioo_string <- bio_df %>% filter(Type == "Bio-originator") %>% select(`Brand name`) %>% pull()

# as per Kuan's advice, if the drug name is just the drug name (i.e. not the brand name of bioo or bios), we can safely assume it is biooriginator
# and therefore I could add the ddrug name under the bioo_string
bioo_string <- c(bioo_string, c("INFLIXIMAB", "ADALIMUMAB", "RITUXIMAB"))

bios_string <- bio_df %>% filter(Type == "Biosimilar") %>% select(`Brand name`) %>% pull()

# remove some gaps
bioo_string <- gsub(pattern = "[[:space:]\u00A0]", replacement = "", x = bioo_string)
bios_string <- gsub(pattern = "[[:space:]\u00A0]", replacement = "", x = bios_string)

# now, create the bio column (which says if it is biosimilar or biooriginator)
# put the bios BEFORE bioO in case the drug name contains both the ingredient and the bios brand name, in which case the bios brand name should take precedence
prescription_sub <- prescription_sub %>% 
  mutate(
    bioo_or_bios = case_when(
      stringr::str_detect(DrugName, paste(bios_string, collapse = "|")) ~ "s",
      stringr::str_detect(DrugName, paste(bioo_string, collapse = "|")) ~ "o", # using string infliximab, because sometimes only brand name, but not ingredient name given. So you would miss that if you hadn't use this string
      TRUE ~ NA_character_)
  )


# double check here just to check if names extracted properly
# s <- prescription_sub %>% filter(!is.na(bioo_or_bios)) %>% select(DrugName, ingredient, bioo_or_bios)
# View(unique(s[c("DrugName", "ingredient", "bioo_or_bios")]))



# Thirdly, now is just brute force to extract the brand names as there is no clever meaningful way of doing so
prescription_sub <- prescription_sub %>% 
  mutate(
    brand = case_when(
      str_detect(DrugName, "REMICADE") ~ "REMICADE",
      str_detect(DrugName, "REMSIMA") ~ "REMSIMA",
      str_detect(DrugName, "HUMIRA") ~ "HUMIRA",
      str_detect(DrugName, "HULIO") ~ "HULIO",
      str_detect(DrugName, "ADALLOCE") ~ "ADALLOCE",
      str_detect(DrugName, "AMGEVITA") ~ "AMGEVITA",
      str_detect(DrugName, "IDACIO") ~ "IDACIO",
      str_detect(DrugName, "HYRIMOZ") ~ "HYRIMOZ",
      str_detect(DrugName, "MABTHERA") ~ "MABTHERA",
      str_detect(DrugName, "RIXATHON") ~ "RIXATHON",
      str_detect(DrugName, "TRUXIMA") ~ "TRUXIMA",
      TRUE ~ NA_character_)
  ) # note even though a lot of brands, just for sake of completeness; our data has most ly HUMIRA, REMICADE, and REMSIMA; then a few more like MABTHERA, RIXATHON, TRUXIMA, AMGEVITA, HYRIMOZ




# to prove the point our data only has a few of them
# pattern <- paste(bio_df %>% select(`Brand name`) %>% pull(), collapse = "|")
# pattern <- gsub(pattern = "[[:space:]\u00A0]", replacement = "", x = pattern)
# grep(pattern, x = unique(prescription$DrugName), value = TRUE)

# create prescription_traj to label all the RELEVANT drugs ------------------------------------

# now we have a prescription table with cleaned information. Now we need to merge the diagnosis table to ONLY get the pts that meet our criteria, and look at the descriptive statistics of their uptake
prescription_sub <- prescription_sub %>% filter(ReferenceKey %in% diagnosis_sub$Reference.Key.) # essential step

# remove white space and non-breaking space; convert to capital letters
drugs <- gsub("[[:space:]\u00A0]", "", 
              toupper(c(cdmard, bioo, bio_df$`Brand name`, unique(bio_df$Ingredient))))

# these are drugs for treating RA, may be switched around; but what about brand names?
# prescription <- prescription[1:300000, ]

# Clean the drug names using case_when() and str_detect()
prescription_traj <- prescription_sub %>%
  mutate(
    DrugName_clean = case_when(
      str_detect(DrugName, str_c(drugs, collapse = "|")) ~ str_extract(DrugName, str_c(drugs, collapse = "|")),
      TRUE ~ NA_character_
    )
  )

# where it is not NA; so we only take the non-NA entries
prescription_traj <- prescription_traj %>% filter(!is.na(DrugName_clean))


tnfi <- bioo_df %>% filter(`Mode of action` == "Tumor Necrosis Factor Inhibitor") %>%  pull(Agent) %>% toupper()
cd28 <- bioo_df %>% filter(`Mode of action` == "CD28") %>%  pull(Agent) %>% toupper()
cd20 <- bioo_df %>% filter(`Mode of action` == "CD20") %>%  pull(Agent) %>% toupper()
il6 <- bioo_df %>% filter(`Mode of action` == "IL-6") %>%  pull(Agent) %>% toupper()
jaki <- bioo_df %>% filter(`Mode of action` == "Janus kinase inhibitor") %>%  pull(Agent) %>% toupper()


# some drugs are tnfi but not actually in bioo_df i.e. the brand names humira; so here we add them back with the drug names regardless bios or bioo brand names
tnfi <- unique(c(tnfi, infliximab, adalimumab))
# also add that back to cd20
cd20 <- unique(c(cd20, rituximab))


# add column of drug mechanism of action (cDMARD, TNF inhibitor etc) for ease of stratification
prescription_traj <- prescription_traj %>% 
  mutate(
    moa = case_when(
      str_detect(DrugName_clean, paste(cdmard, collapse = "|")) ~ "cdmard",
      str_detect(DrugName_clean, paste(tnfi, collapse = "|")) ~ "tnfi",
      str_detect(DrugName_clean, paste(cd28, collapse = "|")) ~ "cd28",
      str_detect(DrugName_clean, paste(cd20, collapse = "|")) ~ "cd20",
      str_detect(DrugName_clean, paste(il6, collapse = "|")) ~ "il6",
      str_detect(DrugName_clean, paste(jaki, collapse = "|")) ~ "jaki",
      TRUE ~ NA_character_)
    ) 



# question 1: Uptake of b/tsDMARDs (stratified by mode of action and bio-originator / biosimilars) by year (2010-2022) among patients with rheumatoid arthritis (RA); ended up using EXCEL instead-------------------------------------

# number of pts in our subsetted data is 16727
length(unique(prescription_traj$ReferenceKey))


#look at the counts 
df <- prescription_traj %>% 
select(ReferenceKey, bioo_or_bios, DrugName_clean) %>% 
  distinct(ReferenceKey, DrugName_clean, bioo_or_bios) %>% 
  group_by(DrugName_clean, bioo_or_bios) %>% 
  summarise(count = n())

print(df, n = 27)

saveRDS(object = prescription_traj, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/prescription_traj.rds")
# readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/prescription_traj.rds")

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


write.csv(df_counts, file = "/Users/elsiechan/Desktop/kuan_folder/df_counts.csv", row.names = FALSE)
write.csv(df_reshaped, file = "/Users/elsiechan/Desktop/kuan_folder/df_reshaped.csv", row.names = FALSE)

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

ggplot(moa_counts, aes(x = Year, y = Count, fill = moa)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of RA drug use by MoA from 2009 to 2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "MoA")) +
  scale_x_continuous(breaks = unique(moa_counts$Year), labels = unique(moa_counts$Year))


# question 2 treatment trajectory (would allow us to label pt) -----------------------------------------
# prescription_traj_list <- prescription_traj_list[1:50]

# as per Kuan's suggestions to simplify the analysis and since no economic significance just umbrella the cdmard under cdmard, makes it easier
prescription_traj <- prescription_traj %>% 
  mutate(
    DrugName_clean = case_when(
      DrugName_clean %in% cdmard ~ "cdmard",
      TRUE ~ DrugName_clean
    )
  )



# prescription_traj %>% filter(ReferenceKey == 2154976) %>% arrange(PrescriptionStartDate) %>% tail()

# this line to give extra info if O or S being used
prescription_traj <- prescription_traj %>%
  mutate(DrugName_clean_os = case_when(
    bioo_or_bios == "o" ~ paste0(DrugName_clean, "_o"),
    bioo_or_bios == "s" ~ paste0(DrugName_clean, "_s"),
    TRUE ~ DrugName_clean
  ))


df <- prescription_traj



Ref <- 2209539
df <- df %>% filter(ReferenceKey == Ref)
df <- df %>% arrange(ReferenceKey, PrescriptionStartDate) %>% head(10000)

# testing the function after moa changes
merged_df_drugnamed %>% filter(ReferenceKey == Ref) %>% print(n = 100)
moanamed_df %>% filter(ReferenceKey == Ref) %>% print(n = 100)
debug(extract_traj)
debug(gap_merge_per_drug)
debug(decompose_dates)
undebug(extract_traj)
undebug(gap_merge_per_drug)
undebug(decompose_dates)
df %>% filter(ReferenceKey == Ref) %>% select(PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) %>% arrange(PrescriptionStartDate) %>% print(n = 100)
df %>% filter(ReferenceKey == Ref) %>% extract_traj()
df %>% filter(ReferenceKey == Ref) %>% mutate(DrugName_clean = moa) %>% extract_traj()

df %>% filter(ReferenceKey == Ref)



# UNCOMMENT THIS LINE TO GET BY MOA DF----------------------------
# this line commented out but would have changed the analysis by MoA instead of DrugName_clean thereafter
# df$DrugName_clean <- df$moa

# UNCOMMENT THIS LINE TO GET BY O or S DF----------------------------
# so have to rerun from this line on to generate different df; the custom functions just use DrugName_clean so it would be too troublesome if we put in this as a new column instead
# df$DrugName_clean <- df$DrugName_clean_os


source("/Users/elsiechan/Documents/GitHub/rheumatoid/02_extract_traj.R")

unique(merged_df$DrugName_clean)
table(merged_df$DrugName_clean)

# merged_df_drugnamed %>% filter(ReferenceKey == 1007790)


# so the drugnamed would be when I didn't run this line prescription_traj$DrugName_clean <- prescription_traj$moa
# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_drugnamed.rds")
# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_moanamed.rds")
# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_drugnamed_os.rds")


merged_df_drugnamed <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_drugnamed.rds")
moanamed_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_moanamed.rds")
merged_df_drugnamed_os <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_drugnamed_os.rds")

# moanamed has fewer rows than merged_df, is the reduced version; so we can join, and let it contain redundant information for now

# now to merge them would lead to duplicate rows in moanamed; so later if need to convert back to moa just use distinct


# merge the drug name with moa, so redundant rows for moa (REPEATED already to merge all three dfs together, merged_df_drugnamed_os, merged_df_drugnamed, then moanamed_df)------------

df1 <- merged_df_drugnamed_os
df2 <- merged_df_drugnamed

suffix <- c("_1", "_2")

source("/Users/elsiechan/Documents/GitHub/rheumatoid/03_merge_os_drugname_moa.R")
# col1 <- "DrugName_clean_1"
# col2 <- "DrugName_clean_2"

# rename drug, moa, for ease of reference and plotting later
merged_df <- merged_df %>% 
  rename(drug_os = DrugName_clean_1,
         # drug = DrugName_clean2,
         DrugName_clean = DrugName_clean_2) # see explanation for this second line of renaming

df_renamed <- merged_df %>%
  rename_with(~case_when(
    stringr::str_ends(.x, "_1") ~ str_remove(.x, "_1") %>% paste0("_os"),
    # stringr::str_ends(.x, "_2") ~ str_remove(.x, "_2") %>% paste0("_drug"),
    stringr::str_ends(.x, "_2") ~ str_remove(.x, "_2"), # for explanation is we want this to behave as df1 and moa (later) as df2
    TRUE ~ .x
  ))

# so you have drugname_clean_os, and drugname_clean_drug
# the function merge_os_drugname_moa, what it does is for the df2, it will take out the rows with NA. And loop to fill in the information
# so with os > drugname > moa. I can use the drugname column to loop expand the moa. 
# hence REMOVE the _drug naming for df_renamed; this way that one becomes df1 column, and moa becomes df2 column


# round 2 combine this time with moanamed_df
df1 <- df_renamed
df2 <- moanamed_df
suffix <- c("_1", "_2")

source("/Users/elsiechan/Documents/GitHub/rheumatoid/03_merge_os_drugname_moa.R")


merged_df <- merged_df %>% 
  rename(drug = DrugName_clean_1,
         moa = DrugName_clean_2) 

merged_df <- merged_df %>%
  rename_with(~case_when(
    stringr::str_ends(.x, "_1") ~ str_remove(.x, "_1") %>% paste0("_drug"),
    stringr::str_ends(.x, "_2") ~ str_remove(.x, "_2") %>% paste0("_moa"),
    TRUE ~ .x
  ))


# rearrange the order
merged_df <- merged_df %>% select(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate, drug_os, drug, moa, duration_os, duration_drug, duration_moa, PrescriptionInterval_os, PrescriptionInterval_drug, PrescriptionInterval_moa, gap_output_os, gap_output_drug, gap_output_moa, everything())

# merged_df %>% filter(drug_os != drug) %>% View()
# merged_df %>% filter(ReferenceKey == 1028527) %>% View()
# merged_df %>% filter(ReferenceKey == 10174025) %>% View()

# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df.rds")
merged_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df.rds")



# calculate days before the use of b/tsDMARD (we don't worry about csDMARD most of the time)--------------------------------
# so these are either b or tsdmard, calculate number of days till the use of it
btsdmard <- c(tnfi, cd28, cd20, il6, jaki)

# diagnosis_sub is from clean_diagnosis see earlier subtitle of this script
# join the table to obtain the day of first_ra diagnosis
merged_df <- left_join(merged_df,
                       unique(diagnosis_sub[, c("Reference.Key.", 
                                                "first_ra")]),
                       by = c("ReferenceKey" = "Reference.Key."))


merged_df$first_ra <- as.Date(merged_df$first_ra)

# prescription after vs prescription before ra ----------------------------
# setting earliest_start_date, so we can separate pt which we want to include in b/tsDMARD analysis
# row_number returns a column of integers indicating the row number for each row in a group
# row_number() to identify the first row within each group, and for that row, we set first_ra to the earliest_start_date. For all other rows in each group, we use the same logic as before to set first_ra to the earliest PrescriptionStartDate only if it is later than the current first_ra.

# you would expect that first_ra is smaller or equal to prescriptionstartdate since we have already excluded, earlier, those who have conditions other than RA which indicate the use of biologics. 
# sensibility check: why some pts receive prescription before their date of first dx with RA?
# if FALSE, the cohort which we potentially want to exclude from our analysis
merged_df <- merged_df %>%
  arrange(ReferenceKey, PrescriptionStartDate) %>%
  group_by(ReferenceKey) %>%
  mutate(
    earliest_start_date = min(PrescriptionStartDate),
    prescription_after_ra = earliest_start_date >= first_ra
  ) %>%
  ungroup()

print(paste0("Of the ", length(unique(merged_df$ReferenceKey)), 
             " patients, we would expect that the earliest date of diagnosis of rheumatoid arthritis must have occured before the prescription of any rheumatoid arthritis drugs. This is indeed the case for ", 
             merged_df %>% filter(prescription_after_ra == TRUE) %>% distinct(ReferenceKey) %>% count(), 
             " patients. However, we found that there were ", 
             merged_df %>% filter(prescription_after_ra == FALSE) %>% distinct(ReferenceKey) %>% count(), " exceptions in which the patient received RA-related prescription before the earliest date of rheumatoid arthritis diagnosis. These patients will be excluded from our analysis as they affect the reliability of the number of days relapsed since the use of b/tsDMARD."))

# random <- 5
# diagnosis_sub %>% filter(Reference.Key. == prescription_before_ra[random]) %>% arrange(Reference.Date.)
#  
# prescription_sub %>% filter(ReferenceKey == prescription_before_ra[random]) %>% select(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate, DrugName) %>% arrange(PrescriptionStartDate)
# 
# View(diagnosis_sub %>% filter(Reference.Key. == prescription_before_ra[random]) %>% arrange(Reference.Date.))
# View(prescription_sub %>% filter(ReferenceKey == prescription_before_ra[random]) %>% select(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate, DrugName) %>% arrange(PrescriptionStartDate))


# code if we decided to just modify and go with the pseudo-dx date instead
# merged_df %>%
#   group_by(ReferenceKey) %>%
#   mutate(
#     earliest_start_date = min(PrescriptionStartDate),
#     first_ra = if_else(row_number() == 1 | first_ra > earliest_start_date, earliest_start_date, first_ra)
#   ) %>%
#   ungroup()


# merged_df %>% filter(prescription_after_ra == TRUE) %>% pull(ReferenceKey) %>% unique()


# number of days from dx to prescription of first_ra drug; normally should be positive as prescription comes after dx ------------------------------------------------------------------
merged_df$dx_to_prescription <- merged_df$earliest_start_date - merged_df$first_ra

# if negative, it means RA ICD diagnosis came after the first prescription of RA-related drug
num_vec <- merged_df %>% filter(dx_to_prescription < 0) %>% distinct(ReferenceKey, dx_to_prescription) %>% pull(dx_to_prescription)
  

range_vec <- cut(as.numeric(num_vec), breaks = c(-Inf, -365, -100, -31, -14, -7, -1), 
                 labels = c("-365 to -101", "-100 to -31", "-30 to -15", "-14 to -8", "-7 to -2", "-1"))

# Create a frequency table of the counts within each category
table(range_vec)


# days till first cdmard
# negative would be FALSE for prescription_after_ra
merged_df <- merged_df %>%
  filter(drug == "cdmard") %>%
  group_by(ReferenceKey) %>% # so would give us earliest startdate by reference key
  mutate(earliest_cdmard_date = min(PrescriptionStartDate)) %>%
  mutate(days_to_cdmard = earliest_cdmard_date - first_ra) %>%
  distinct(ReferenceKey, days_to_cdmard) %>%  # only need unique to prevent many-to-many relationship
  select(ReferenceKey, days_to_cdmard) %>% 
  full_join(merged_df) # so merged_df gets a new column, which is days_to_cdmard; rather than left_join


# you should get some NA values because there are patients who did not take cdmard
# any(is.na(merged_df %>% pull(days_to_cdmard)))
# any(is.na(temp %>% pull(days_to_cdmard)))


# date till first bsdmard if any (NA otherwise)
# btsdmard is made of (tnfi, cd28, cd20, il6, jaki)
# merged_df %>% pull(DrugName_clean) %>% unique()

btsdmard_class <- c("jaki", "tnfi", "cd20", "cd28", "il6")

merged_df <- merged_df %>% 
  filter(grepl(pattern = paste(btsdmard_class, collapse = "|"),
               x = moa)) %>%
  group_by(ReferenceKey) %>% 
  mutate(earliest_btsdmard_date = min(PrescriptionStartDate)) %>% 
  mutate(days_to_btsdmard = earliest_btsdmard_date - first_ra) %>%
  distinct(ReferenceKey, days_to_btsdmard) %>%
  select(ReferenceKey, days_to_btsdmard) %>% 
  full_join(merged_df)

merged_df$days_to_btsdmard <- as.numeric(merged_df$days_to_btsdmard)
merged_df$days_to_cdmard <- as.numeric(merged_df$days_to_cdmard)

# print(temp %>% filter(is.na(days_to_btsdmard)), n = 200)

# combine scores_df with merged_df ----------------------------------------
# if you used full_join, will have a lot of NA rows from scores_df
merged_df <- dplyr::left_join(merged_df, scores_df, by = c("ReferenceKey" = "Reference.Key."))

# there is one random entry with drug prescription in 1900
merged_df <- merged_df %>% filter(earliest_start_date > 1950)



# combine death with merged_df --------------------------------------------


# death <- readRDS("Death.RDS")
death <- death %>%
  rename(main_death = `DeathCause(MainCause)`) %>%
  rename(death_date = DateofRegisteredDeath)

# had to use this stupid method because death contains some list in some columns, is a complex object
death_df <- data.frame(ReferenceKey = death$ReferenceKey, 
                       death_date = death$death_date, 
                       main_death = death$main_death)

merged_df <- merged_df %>%
  left_join(death_df, by = "ReferenceKey")


# create column for last record (whichever earlier: death date or last PrescriptionStartDate in data (rather than PrescriptionEndDate)  --------
# pt is still alive then take our max_date
# max_date is actually july 11th which is way after the data was downloaded - but this is not expected to have a effect on our data
max_date <- max(merged_df$PrescriptionStartDate, na.rm = TRUE)

merged_df <- merged_df %>%
  group_by(ReferenceKey) %>%
  mutate(last_record = ifelse(!is.na(death_date), 
                              as.Date(death_date), # ensure correct formatting 
                              max_date)) %>% 
  ungroup() %>% 
  mutate(last_record = as.Date(last_record))

# merged_df %>% select(main_death)
# typeof(merged_df)
# str(merged_df)
# sapply(merged_df, typeof)
# merged_df %>% select(ReferenceKey)


# days from RA diagnosis to death -----------------------------------------
merged_df <- merged_df %>%
  mutate(dx_to_death = ifelse(!is.na(death_date),
                                difftime(death_date, earliest_start_date, units = "days"),
                                NA))

# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df2.rds")
merged_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df2.rds")


# 1a: How long patients can use b/ts DMARDs in Hong Kong, potentially stratified by years to illustrate the improved uptake rate (My personal guesswork is new patients such as diagnosis in 2019 should access to b/ts DMARDs faster than old patients diagnosis in 2009)----------------

# Extract the year of diagnosis of the RA
merged_df$first_ra_year <- format(merged_df$first_ra, "%Y")

merged_df$years_to_btsdmard <- as.numeric(merged_df$days_to_btsdmard / 365.25)
merged_df$years_to_cdmard <- as.numeric(merged_df$days_to_cdmard / 365.25)

# summarize solves the issue where for the same first_ra value the value seems to pile up and give a erroneous num for days to btsdmard
merged_df_days <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  distinct(ReferenceKey, .keep_all = TRUE) %>% 
  group_by(first_ra) %>%
  summarize(mean_days_to_btsdmard = mean(days_to_btsdmard),
            mean_days_to_cdmard = mean(days_to_cdmard))

# 
# # Create a bar chart of days_to_btsdmard, stratified by year of diagnosis of the RA
# # tunable parameters include y = years_to_btsdmard, x = first_ra, first_ra_year
# ggplot(
#   data = merged_df_days %>% filter(!is.na(mean_days_to_btsdmard)),
#   aes(x = first_ra, y = mean_days_to_btsdmard)
# ) +
#   geom_col() +
#   labs(title = "Days to btsdmard by Date of Diagnosis of RA",
#        x = "Date of Diagnosis of RA", y = "Days to btsdmard")

# merged_df_days %>% pull(days_to_btsdmard)
# merged_df_days %>% pull(first_ra) %>% table()

# change the variables for ease of plotting
# x_var <- "first_ra_year"
# x_label <- "Year of Diagnosis of RA"
x_var <- "first_ra"
x_label <- "Date of Diagnosis of RA"

y_var <- "mean_days_to_cdmard"
y_label <- "Mean Days to cDMARD"
y_var <- "mean_days_to_btsdmard"
y_label <- "Mean Days to btsDMARD"


df <- merged_df_days %>% filter(!is.na(mean_days_to_cdmard))
df <- merged_df_days %>% filter(!is.na(mean_days_to_btsdmard))


# Plot the y_var by x_var with gridlines, modified colors, and minor gridlines
my_plot <- ggplot(
  data = df,
  aes(x = !!sym(x_var), y = !!sym(y_var))
) +
  # ylim(min(merged_df_days$days_to_btsdmard, na.rm = TRUE),
  #      max(merged_df_days$days_to_btsdmard, na.rm = TRUE)) +
  geom_col(fill = "#0072B2", color = "#333333") +
  labs(title = paste0(y_label, " by ", x_label),
       x = x_label, y = y_label) +
  scale_fill_manual(values = c("#0072B2")) +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "#DDDDDD"),
        panel.grid.minor = element_line(color = "#EEEEEE", linetype = "dashed"),
        panel.background = element_rect(fill = "#F5F5F5"),
        axis.line = element_line(color = "#333333"),
        axis.text = element_text(color = "#333333", size = 12),
        axis.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "none")

my_plot

ggsave(paste0(path, "/charts/", "bar_", y_var, ".svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "bar_", y_var, ".png"), my_plot, device = "png", dpi = 300)


# 1b # plot the scatter chart to find out any relationship between days to cdmard and days to bdmard----------------------------------------------------------------------

# getting some descriptive statistics
temp1 <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  # filter(!is.na(days_to_cdmard)) %>% 
  # filter(!is.na(days_to_btsdmard)) %>% 
  distinct(ReferenceKey) %>% 
  nrow()

temp2 <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  filter(!is.na(days_to_cdmard)) %>%
  # filter(!is.na(days_to_btsdmard)) %>% 
  distinct(ReferenceKey) %>% 
  nrow()

temp3 <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  # filter(!is.na(days_to_cdmard)) %>% 
  filter(!is.na(days_to_btsdmard)) %>%
  distinct(ReferenceKey) %>% 
  nrow()

temp4 <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  filter(!is.na(days_to_cdmard)) %>%
  filter(!is.na(days_to_btsdmard)) %>%
  distinct(ReferenceKey) %>% 
  nrow()

print(paste0("For the following analysis, on whether days from diagnosis to cDMARD and bDMARD can predict mortality, we only included patients who received prescription after rheumatoid arthritis. By including patients who received prescription before the diagnosis, our baseline multimorbidity index would have failed to reflect the baseline condition of the patient, and may have been influenced by the effect of the medication. Among the ", temp1, " number of patients were diagnosed before prescription of rheumatoid arthritis-related drug, there were, at some point of their treatment trajectory, ", temp2, " patients who received cDMARD and ", temp3, " who received btsDMARD, and ", temp4, " who received both, not necessarily concurrently."))

df <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  filter(!is.na(days_to_cdmard)) %>% 
  filter(!is.na(days_to_btsdmard)) %>% 
  distinct(ReferenceKey, .keep_all = TRUE)

# Find the maximum value of either column
max_years <- max(pmax(df$years_to_cdmard, df$years_to_btsdmard), na.rm = TRUE)
max_years <- max_years + 1 # for more clarity

my_plot <- ggplot(df, aes(x = years_to_cdmard, y = years_to_btsdmard, color = score_before_0)) +
  geom_point(na.rm = TRUE, size = 0.9, alpha = 0.9) +
  labs(x = "Years to cdmard", y = "Years to btsdmard",
       title = "Relationship between years to btsdmard and cdmard") +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  scale_x_continuous(limits = c(0, max_years), 
                     breaks = seq(0, max_years, by = 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, max_years), 
                     breaks = seq(0, max_years, by = 5), expand = c(0, 0)) +
  scale_color_gradient(low = "#F5B7B0", high = "#00008B", 
                       na.value = "gray80") +
  guides(color = guide_colorbar(title = "Multimorbidity Index at \n time of diagnosis"))

my_plot

ggsave(paste0(path, "/charts/", "scatter_", "yrs_to_drug", ".svg"), my_plot, device = "svg")
# Save the ggplot object in PNG format
ggsave(paste0(path, "/charts/", "scatter_", "yrs_to_drug", ".png"), my_plot, device = "png", dpi = 300)



# 1c: using years_to_cdmard and years_to_bts_dmard correcting for score_before_0 to predict dx_to_death

# continue to use the df from 1b so we omit the na values for either
# Fit linear regression model with years_to_cdmard as predictor and score_before_0 as a covariate
model_cdmard <- lm(dx_to_death ~ score_before_0 + years_to_cdmard, data = df)
summary(model_cdmard)

# Fit linear regression model with years_to_btsdmard as predictor and score_before_0 as a covariate
model_btsdmard <- lm(dx_to_death ~ score_before_0 + years_to_btsdmard, data = df)
summary(model_btsdmard)

# Extract model coefficients and p-values
coef_btsdmard <- summary(model_btsdmard)$coefficients
pval_btsdmard <- coef_btsdmard["years_to_btsdmard", "Pr(>|t|)"]
coef_estimate_btsdmard <- coef_btsdmard["years_to_btsdmard", "Estimate"]
adj_r2_btsdmard <- summary(model_btsdmard)$adj.r.squared

# Construct description sentence; significant here
paste0("The coefficient estimate for years to btsdmard was ", round(coef_estimate_btsdmard, 2), " and the associated p-value was ", format.pval(pval_btsdmard), ". This indicates that there is a significant positive relationship between the number of years between diagnosis and first use of btsDMARD and time to death. The adjusted R-squared value for the model was ", round(adj_r2_btsdmard, 4), ", suggesting that a small amount of the variance in time to death was explained by the model.")


# Extract model coefficients and p-values
coef_cdmard <- summary(model_cdmard)$coefficients
pval_cdmard <- coef_cdmard["years_to_cdmard", "Pr(>|t|)"]
coef_estimate_cdmard <- coef_cdmard["years_to_cdmard", "Estimate"]
adj_r2_cdmard <- summary(model_cdmard)$adj.r.squared

# Construct description sentence; insignificant here
paste0("The coefficient estimate for years to cdmard was ", round(coef_estimate_cdmard, 2), " and the associated p-value was ", format.pval(pval_cdmard), ". This indicates that there is no significant relationship between the number of years between diagnosis and first use of cDMARD and time to death. The adjusted R-squared value for the model was ", round(adj_r2_cdmard, 4), ", suggesting that only a very small amount of the variance in time to death was explained by the model.")


# improved version
paste0("Linear regression models were used to predict the time to death based on the number of years between diagnosis and first use of cDMARD or btsDMARD, while controlling for baseline multimorbidity. The results show that baseline comorbidity score did not have a significant confounding effect on the relationship between time to death and the number of years between diagnosis and first use of either drug. However, only years to btsDMARD was found to be a significant positive predictor of time to death (p = ", format.pval(pval_btsdmard), "), with an adjusted R-squared value of ", round(adj_r2_btsdmard, 4), " and coefficient estimate ", round(coef_estimate_btsdmard, 2), ". In contrast, years to cdmard was not a significant predictor of time to death in this sample (p = ", format.pval(pval_cdmard), ") adjusted R-squared = ", round(adj_r2_cdmard, 4), ".")


# 2: Among b/ts DMARDs, which one is preferred by local clinicians as first/second line option? Could also stratified by years to demonstrate the change of market share as time goes by (For example, i know the use of infliximab is decreasing year by year)----------------
# obtain first btsdmard
first_btsdmard_df <- merged_df %>%
  # filter(ReferenceKey == 10024714 | ReferenceKey == 100096) %>% 
  group_by(ReferenceKey) %>%
  filter(grepl(pattern = paste(btsdmard, collapse = "|"), 
                    x = drug)) %>%
  slice_min(PrescriptionStartDate) %>%
  ungroup() 

# first_btsdmard_df$drug[1] <- "ETANERCEPT+ABATACEPT+INFLIXIMAB+cdmard"

first_btsdmard_df <- first_btsdmard_df %>%
  mutate(first_btsdmard_list = stringr::str_extract_all(drug, paste(btsdmard, collapse = "|"))) %>% 
  unnest(first_btsdmard_list) %>%
  group_by(ReferenceKey) %>%
  summarize(first_btsdmard = paste(first_btsdmard_list, collapse = "+")) 


# Join the first_btsdmard_df with merged_df
merged_df <- merged_df %>%
  left_join(first_btsdmard_df, by = "ReferenceKey")


merged_df_proportions <- merged_df %>%
  filter(!is.na(first_btsdmard)) %>% 
  group_by(first_ra_year, first_btsdmard) %>%
  summarize(n = n()) %>%
  group_by(first_ra_year) %>%
  mutate(prop = n / sum(n))

print(merged_df_proportions, n = 200)

ggplot(
  data = merged_df_proportions,
  aes(x = first_ra_year, y = prop, color = first_btsdmard, group = first_btsdmard)
) +
  geom_smooth(method = "loess", se = FALSE, span = 1) +
  labs(
    title = "Proportion of First btsDMARD by Drug and Year",
    x = "Year of Diagnosis of RA",
    y = "Proportion of First btsDMARD",
    color = "First btsDMARD Drug"
  ) +
  scale_color_discrete(name = "First btsDMARD Drug") +
  scale_x_discrete(
    breaks = seq(min(merged_df_proportions$first_ra_year), max(merged_df_proportions$first_ra_year), by = 5)
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df3.rds")
merged_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df3.rds")

# 3: The access time and uptake rate of biosimilars in Hong Kong

# there is one random entry with drug prescription in 1900
merged_df <- merged_df %>% filter(earliest_start_date > 1950)

# split the drug column by the "+" character; get the total number of moa for each pt (alone or in combination, number of unique drugs taken)
total_moa_df <- merged_df %>%
  separate_rows(moa, sep = "\\+") %>%
  group_by(ReferenceKey) %>%
  summarize(n_moa = n_distinct(moa))

merged_df <- merged_df %>% full_join(total_moa_df)

# new column called moa swaps (swap, addition or drop)
# arbitrary indicator of complexity, just for aesthetic of Gantt's chart
merged_df <- merged_df %>%
  group_by(ReferenceKey) %>%
  mutate(n_swaps = sum(drug != lag(moa, default = first(moa)))) %>%
  ungroup()

# new column called duration just max(end) - min(start)
merged_df <- merged_df %>%
  group_by(ReferenceKey) %>%
  mutate(duration = as.numeric(difftime(max(PrescriptionEndDate), min(PrescriptionStartDate), units = "days"))) %>%
  ungroup()

# colnames(merged_df)
# merged_df %>% pull(drug_os) %>% unique()
# merged_df %>% pull(DrugName_clean) %>% unique()


# filter to only include when tx duration > 30 days (for continuous period) to be considered significant

# 4: just the switch between bios and bioo, on average how many days? ---------------
# this info is needed. e.g. multimorbidity index would not make sense if bios was taken many days after, then you don't have the baseline 
# should theoretically take the change in the multimorbidity index score compare with one year after the switch if there ever was one

merged_df %>% 
  group_by(ReferenceKey) %>% 
  mutate(first_use_o = ifelse(str_detect(drug_os, "_o"), PrescriptionStartDate, NA_real_)) %>% 
  View()

colnames(merged_df)

# adding days to o and earliest o date
merged_df <- merged_df %>% 
  filter(str_detect(drug_os, "_o")) %>% 
  group_by(ReferenceKey) %>% 
  mutate(earliest_o_date = min(PrescriptionStartDate)) %>% 
  mutate(days_to_o = earliest_o_date - earliest_start_date) %>% # rather than first_ra unlike cdmard and bdmard
  distinct(ReferenceKey, earliest_o_date, days_to_o) %>%
  full_join(merged_df)

merged_df$days_to_o <- as.numeric(merged_df$days_to_o)

# adding days to s and earliest s date
merged_df <- merged_df %>% 
  filter(str_detect(drug_os, "_s")) %>% 
  group_by(ReferenceKey) %>% 
  mutate(earliest_s_date = min(PrescriptionStartDate)) %>% 
  mutate(days_to_s = earliest_s_date - earliest_start_date) %>%
  distinct(ReferenceKey, earliest_s_date, days_to_s) %>%
  full_join(merged_df)

merged_df$days_to_s <- as.numeric(merged_df$days_to_s)

# creating os trajectory
merged_df <- merged_df %>% 
  mutate(os_trajectory = case_when(
    days_to_o < days_to_s ~ "o_s",
    days_to_o > days_to_s ~ "s_o",
    !is.na(days_to_o) & is.na(days_to_s) ~ "o", # have o but not s
    !is.na(days_to_s) & is.na(days_to_o) ~ "s", # have s but not o
    days_to_o == days_to_s ~ "b",
    TRUE ~ NA_character_
  ))

merged_df %>% View()


temp <- merged_df %>% 
  distinct(ReferenceKey, os_trajectory) %>% 
  group_by(os_trajectory) %>%
  pull(os_trajectory) %>% 
  table()


paste0("Based on the earliest date of use of bio-originator or bio-similar, we classified patients into bio-originator only (", temp["o"], "), biosimilar only (", temp["s"], "), switch from bio-originator to biosimilar (", temp["o_s"], "), and switch from biosimilar to bio-originator (", temp["s_o"], "). There were no patients who commenced both simultaneously.")

# it makes sense the above don't add up to the !is.na(days_to_btsdmard) because that is btsdmard; whereas we are just looking at the bdmard with bios and bioo 
# merged_df %>% 
#   distinct(ReferenceKey, days_to_btsdmard) %>% pull(days_to_btsdmard) %>% is.na() %>% table()

# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df4.rds")
merged_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df4.rds")


# on average how many days until switch? 
merged_df <- merged_df %>% 
  filter(os_trajectory == "s_o" | os_trajectory == "o_s") %>% # where there is a switch
  mutate(days_till_os_switch = abs(days_to_s - days_to_o)) %>% 
  distinct(ReferenceKey, days_till_os_switch) %>% 
  full_join(merged_df)

# get the mean, min and max
temp <- merged_df %>% filter(os_trajectory == "o_s") %>% 
  distinct(ReferenceKey, days_till_os_switch) %>% 
  summary(days_till_os_switch)

paste0("For the switch from biooriginator to biosimilar, the mean number of days from the earliest date, be it date of diagnosis of rheumatoid arthritis or the prescription of a rhematoid arthritis drug, is ", temp[4,2], "(min = ", temp[1,2], ", max = ", temp[6,2], ".")

# failed plot attempt at alluvial (but pretty useless so forget it------------------
# now I need a plot to demonstrate o, s, o_s, s_o, and number of days till the switch occurs for the latter two

df <- merged_df %>% 
  distinct(ReferenceKey, .keep_all = TRUE)

# just to remove the values i called NA
df <- df %>% filter(os_trajectory != "NA")


ggplot(data = df_transition,
       aes(axis1 = o, axis2 = s, y = freq)) +
  geom_alluvium(aes(fill = s)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  theme_void()

vaccinations
ggplot(data = vaccinations,
       aes(axis1 = survey, axis2 = response, y = freq)) +
  geom_alluvium(aes(fill = response)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  theme_void()


# create a data frame with the transition counts
df_transitions <- table(df$os_trajectory)

# convert the table to a data frame
df_transitions <- as.data.frame(df_transitions)

# rename the columns
colnames(df_transitions) <- c("from", "n")

# create an alluvial diagram
ggplot(df_transitions, aes(y = n, axis1 = from)) +
  geom_alluvium(aes(fill = from), width = 1/12) +
  geom_stratum(width = 1/12, aes(fill = from)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF", "#FFB85F")) +
  ggtitle("Transitions between OS Trajectories") +
  theme(plot.title = element_text(hjust = 0.5))




# create a data frame with the transition counts
df_transitions <- data.frame(from = sub("_.*", "", df$os_trajectory),
                             to = sub(".*_", "", df$os_trajectory))

# tabulate the counts of transitions
df_transitions <- table(df_transitions$from, df_transitions$to)

# convert the table to a data frame
df_transitions <- as.data.frame(df_transitions)

# rename the columns
colnames(df_transitions) <- c("from", "to", "n")

# create an alluvial diagram
ggplot(df_transitions, aes(y = n, axis1 = from, axis2 = to)) +
  geom_flow(aes(fill = from), width = 0.5) +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF", "#FFB85F")) +
  ggtitle("Transitions between OS Trajectories") +
  theme(plot.title = element_text(hjust = 0.5))





# definitely needed to calculate the multimorbidity score if of interests from the point days_to_o, and repeated for days_to_s-----------------------------------

merged_df












# 5: predilection to cause of death if different drugs used (?) ------------------------

merged_df %>% View()

merged_df %>% 
  filter(!is.na(main_death)) %>% 
  count(main_death) %>% 
  filter(n > 10) %>% 
  arrange(desc(n))

# 6: change in score over time stratified by use of bios vs bioo -------------

# labelled pt by bioo, bios, those who changed from bioo → bios, bios → bioo
oors_df <- merged_df %>% 
  filter(os_trajectory == "s") %>% 
  distinct(ReferenceKey, .keep_all = TRUE) %>% # only need one row per ReferenceKey
  select(ReferenceKey, days_to_s, last_record, earliest_start_date) %>% 
  rename(days_to_OS = days_to_s) # hence reproducible


source("/Users/elsiechan/Documents/GitHub/rheumatoid/04_cal_timepoint_score_baselined.R")

# output is output_df
s_df <- output_df

oors_df <- merged_df %>% 
  filter(os_trajectory == "o") %>% 
  distinct(ReferenceKey, .keep_all = TRUE) %>% # only need one row per ReferenceKey
  select(ReferenceKey, days_to_o, last_record, earliest_start_date) %>% 
  rename(days_to_OS = days_to_o)

source("/Users/elsiechan/Documents/GitHub/rheumatoid/04_cal_timepoint_score_baselined.R")

o_df <- output_df
o_df %>% arrange(ReferenceKey)


o_df$os <- "o"
s_df$os <- "s"

df <- rbind(o_df, s_df)

df <- df %>% arrange(ReferenceKey, day)

df <- df %>% mutate(month = round(as.numeric(day)/30.44))

# calculate mean and standard error of score, and number of patients, for each month and os group
df_summary <- df %>%
  filter(day != 1096) %>%  # because very few datapoints for biosimilars
  group_by(month, os) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            se_score = sd(score, na.rm = TRUE) / sqrt(sum(!is.na(score))),
            n_patients = sum(!is.na(score)))


my_plot <- ggplot(df_summary, aes(x = month, y = mean_score, fill = os)) +
  geom_ribbon(aes(ymin = mean_score - se_score, ymax = mean_score + se_score), alpha = 0.2) +
  geom_line(aes(color = os)) +
  geom_point(aes(color = os), size = 1, fill = "white") +
  labs(x = "Months after first use of drug", y = "Multimorbidity score") +
  scale_x_continuous(limits = c(0, max(df_summary$month)), breaks = seq(0, max(df_summary$month), 3)) +
  scale_fill_manual(values = c("o" = "blue", "s" = "red"), name = "Drug type", labels = c("Bio-originators", "Biosimilars")) +
  scale_color_manual(values = c("o" = "blue", "s" = "red"), name = "Drug type", labels = c("Bio-originators", "Biosimilars")) +
  theme(axis.text = element_text(size = 10, family = "Arial"), 
        legend.position = "right", 
        legend.box.background = element_rect(color = "gray75", linetype = "solid", fill = "white", size = 0.5), 
        legend.title.align = 0.5, 
        legend.margin = margin(0, 5, 0, 5), 
        legend.text = element_text(size = 10, family = "Arial"), 
        legend.key.size = unit(0.75, "cm")) +
  geom_label(aes(label = paste0("n = ", n_patients)), 
             position = position_nudge(y = 0.25), 
             fill = "gray80", 
             color = c("blue", "red")[as.factor(df_summary$os)], 
             size = 2, 
             # check_overlap = TRUE, 
             na.rm = TRUE, 
             label.padding = unit(0.1, "cm"), 
             label.r = unit(0.1, "cm")) +
  ggtitle("Multimorbidity score from First use\n of Bio-originator and Biosimilar") +
  theme(plot.title = element_text(size = 16, face = "bold", family = "Arial"))

my_plot

# my_plot + annotate("text", x = 3, y = 1.6, label = paste0("Wilcoxon rank-sum test:\n p = ", 
#                                                           round(test_result_1year$p.value, 3), " (1-year) / ", round(test_result_2year$p.value, 3), 
#                                                           " (2-year),\n not statistically significant"), size = 3, color = "black", fill = "white",
#                    alpha = 0.8, box.color = "black", fontface = "bold", hjust = 0, vjust = 1)

ggsave(paste0(path, "/charts/", "line_os_comparison_multimorbidity", ".svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "line_os_comparison_multimorbidity", ".png"), my_plot, device = "png", dpi = 300)

# generate a table for n_patients; not so useful actually
# n_table <- knitr::kable(df_summary, caption = "Number of patients") %>%
#   kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)


# welch's t test------------------------------------------------------------
# instead of paired t test due to different variance of data
# Subset data for day 0 and day 12 for treatment s and treatment o
df_s_m0 <- df %>% filter(month == 0 & os == "s")
df_s_m12 <- df %>% filter(month == 12 & os == "s")
df_o_m0 <- df %>% filter(month == 0 & os == "o")
df_o_m12 <- df %>% filter(month == 12 & os == "o")

# Calculate the increase in score for treatment s and treatment o
df_s <- data.frame(ID = df_s_m0$ReferenceKey, increase = df_s_m12$score - df_s_m0$score)
df_o <- data.frame(ID = df_o_m0$ReferenceKey, increase = df_o_m12$score - df_o_m0$score)


# Perform Wilcoxon rank-sum test
test_result_1year <- wilcox.test(df_s$increase, df_o$increase)


# repeat for month 24, two years
df_s_m24 <- df %>% filter(month == 24 & os == "s")
df_o_m24 <- df %>% filter(month == 24 & os == "o")

# Calculate the increase in score for treatment s and treatment o
df_s <- data.frame(ID = df_s_m0$ReferenceKey, increase = df_s_m24$score - df_s_m0$score)
df_o <- data.frame(ID = df_o_m0$ReferenceKey, increase = df_o_m24$score - df_o_m0$score)


# Perform Wilcoxon rank-sum test
test_result_2year <- wilcox.test(df_s$increase, df_o$increase)



# cannot perform welch's t-test because not normally distributed
# Create histograms and normal probability plots of the data; check normal distribution
# par(mfrow = c(2,2))
# hist(df_s$increase, main = "Treatment S")
# qqnorm(df_s$increase); qqline(df_s$increase)
# hist(df_o$increase, main = "Treatment O")
# qqnorm(df_o$increase); qqline(df_o$increase)

# Perform normality tests
shapiro.test(df_s$increase)
shapiro.test(df_o$increase)

shapiro.test(rbind(df_s, df_o)$increase)
# 
# 
# # Perform Welch's t-test
# t.test(df_s$increase, df_o$increase, var.equal = FALSE)


# linear mixed effect model (failed)------------------------------------
# create a new column for the change in score
df_summary$change_score <- df_summary$mean_score - df_summary$mean_score[df_summary$os == "o" & df_summary$month == 0]

# fit a linear mixed-effects model
# Then, we fit a linear mixed-effects model using the lmer() function from the lme4 package. The predictor variable is os, which represents the drug type (Bio-originator or Biosimilar), and the covariate is mean_score, which represents the baseline score. We also include month as a fixed effect to control for any potential time trends in the outcome variable. Finally, we include a random intercept for each patient using the (1 | patient_id) syntax.

# The Estimate column for the os variable represents the difference in the change in score between the two drug types (Biosimilars vs. Bio-originators), after controlling for the baseline score and time trends. If the estimate is statistically significant (i.e., the p-value is less than your chosen alpha level), then you can conclude that there is evidence of a difference in the change in score between the two drug types.
model <- lme4::lmer(change_score ~ os + month + mean_score + (1 | patient_id), data = df_summary)

# print the model summary
summary(model)



# Calculate mean score for each patient and drug type
df_summary_patient <- df %>%
  group_by(os, ReferenceKey) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Calculate mean score for each drug type and month
df_summary_drug <- df %>%
  group_by(os, month) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Calculate change score relative to baseline for each drug type and month
df_summary_drug <- df_summary_drug %>%
  mutate(change_score = mean_score - mean_score[os == "o" & month == 0])

# Merge df and df_summary_drug to obtain patient-level data
df_merged <- merge(df, df_summary_drug, by = c("os", "month"))

# Merge df_merged and df_summary_patient to obtain patient-level data with mean score
df_merged <- merge(df_merged, df_summary_patient, by = c("os", "ReferenceKey"))

cor(df_merged[, c("os", "month", "mean_score.y")])

# Fit mixed-effects model with random intercepts for each patient
model <- lme4::lmer(change_score ~ os + month + mean_score.y + (1 | ReferenceKey), data = df_merged)







# 7: gantt's chart for treatment trajectory ----------------------------------
start_dates <- merged_df %>% 
  distinct(ReferenceKey, earliest_start_date) %>% 
  pull(earliest_start_date)

# Create year groups using cut()
year_groups <- cut(start_dates, breaks = seq(min(start_dates), max(start_dates), by = "1 year"))

# Create a frequency table using table()
freq_table <- table(year_groups)


# gantt's chart need to be grouped to have some meaning. So group by drug start_date_group so those within 1 year are grouped first followed by duration_group, then drug numbers (monotherapy first, and then those with dual therapy)

# downsample the data for speed and clarity
# create groups based on prescription start dates and duration
merged_df_downsampled <- merged_df %>%
  distinct(ReferenceKey) %>%
  sample_n(size = 300) %>%
  inner_join(merged_df, by = "ReferenceKey") %>%
  group_by(ReferenceKey) %>% # group_by referencekey before cutting PrescriptionStartDate)
  mutate(start_date_group = 
           cut(earliest_start_date, 
               breaks = seq.Date(as.Date(min(merged_df$PrescriptionStartDate)), 
                                 as.Date(max(merged_df$PrescriptionEndDate)), 
                                 by = "6 months"))) %>% 
  mutate(duration_group = cut(duration, breaks = c(0, 365, 730, Inf))) %>%
  arrange(start_date_group, duration_group, n_moa)

# merged_df_downsampled %>% 
#   distinct(ReferenceKey, .keep_all = TRUE) %>% View()

# summary(merged_df$PrescriptionStartDate)
# summary(merged_df$PrescriptionEndDate)
# seq.Date(as.Date(min(merged_df$PrescriptionStartDate)), as.Date(max(merged_df$PrescriptionEndDate)), by = "6 months")



# Create a data frame with start, end, drug, and patient columns
df <- merged_df_downsampled %>%
  mutate(patient = as.factor(ReferenceKey)) %>% 
  select(ReferenceKey, 
         patient, 
         start = PrescriptionStartDate, 
         end = PrescriptionEndDate, 
         drug = moa)

# sample df just for showing the gantt's chart for one pt, illustrative purposes only
# extract_traj(prescription_traj_list[["10140118"]]) # eg to check validity of the plot

# df <- data.frame(
#   start = as.Date(c("2019-05-15", "2019-06-05", "2019-07-02", "2019-07-29", "2021-02-28", "2021-07-05", "2022-03-08", "2022-05-30")),
#   end = as.Date(c("2019-06-05", "2019-07-02", "2019-07-29", "2021-02-28", "2021-07-05", "2022-03-08", "2022-04-10", "2023-01-08")),
#   drug = c("cdmard", "cdmard+jaki", "cdmard", "cdmard+jaki", "cdmard", "cdmard+il6", "il6", "cdmard+il6"),
#   patient = as.factor(c("10140118", "10140118", "10140118", "10140118", "10140118", "10140118", "10140118", "10140118"))
# )
# print(df, n = 100)

# Define lighter colors for single drugs
single_colors <- c("cdmard" = "#6d8db8",
                   "cd28" = "#8fbf8b",
                   "jaki" = "#db7e84",
                   "il6" = "#d68a53",
                   "tnfi" = "#7e7a1c",
                   "cd20" = "#b8b87d")

# Define darker colors for drug combinations
combo_colors <- c("cdmard+jaki" = "#3c1a61",
                  "cdmard+tnfi" = "#5a1c03",
                  "cd28+cdmard" = "#145b8d",
                  "cdmard+il6" = "#525252",
                  "cdmard+il6+jaki" = "#1a5b1a",
                  "cd28+cdmard+il6" = "#5e4187",
                  "cdmard+jaki+tnfi" = "#3c1403",
                  "cd20+cdmard" = "#8b7a3d",
                  "cd28+il6" = "#5b3f35",
                  "cd28+jaki" = "#9c5e88",
                  "cdmard+il6+tnfi" = "#912525",
                  "il6+tnfi" = "#7e7a1c",
                  "jaki+tnfi" = "#9c5e88",
                  "cd28+cdmard+tnfi" = "#a64c00",
                  "cd20+cdmard+jaki" = "#8c7c68",
                  "cd20+cdmard+tnfi" = "#a64c00",
                  "cd20+jaki" = "#9c5e88",
                  "cd28+tnfi" = "#a64c00",
                  "il6+jaki" = "#9c5e88",
                  "cd28+cdmard+jaki" = "#5e4187",
                  "cd20+tnfi" = "#a64c00",
                  "cd20+cdmard+il6" = "#8c7c68",
                  "cd20+cd28+cdmard" = "#5e4187")

# Combine the two palettes
drug_colors <- c(single_colors, combo_colors)

# Create a new column in the dataframe that assigns the correct color to each drug
df$color <- drug_colors[df$drug]

# for setting order of gantt's chart
df$patient <- as.factor(df$patient)

# Reorder the levels based on their appearance in the data
df$patient <- forcats::fct_inorder(df$patient)

# Create the plot
fig <- df %>%
  plot_ly() %>%
  add_segments(x = ~start, xend = ~end, 
               y = ~as.character(patient), yend = ~as.character(patient),
               line = list(color = ~color, width = 2), # Use the new color column here
               color = ~color, 
               name = ~drug, 
               hoverinfo = "text",
               text = ~paste("Drug:", drug,
                             "<br>Start:", format(start, "%d %b, %Y"),
                             "<br>End:", format(end, "%d %b, %Y"),
                             "<br>Patient:", patient),
               showlegend = TRUE)

# Customize the layout
fig <- fig %>% layout(
  xaxis = list(title = "Year", tickfont = list(size = 6),
               tick0 = lubridate::year(min(df$start)),
               tickvals = seq(lubridate::year(min(df$start)), lubridate::year(max(df$start)), by = 1),
               dtick = 3,
               tickangle = 0,
               tickformat = "%Y",
               domain = c(0.1, 0.9)),
  yaxis = list(
    title = "",
    # tickmode = "linear",  # Use linear tick mode
    # dtick = 1,  # Adjust the tick interval as needed
    showticklabels = FALSE, # change to false later
    tickfont = list(size = 6),
    categoryorder = "array",
    categoryarray = rev(levels(df$patient))
  ),
  title = list(text = "<b>Treatment Trajectories of Patients</b>", 
               font = list(size = 15)),
  hovermode = 'closest'
)

fig

# number of days before first change in regimen

# number of days before ADDING one drug

# numbers who defaulted i.e. those whose last record is > 180 days of no FU in the absence of mortality

# numbers who died (?)

df <- prescription_traj %>% filter(ReferenceKey == 847624) # monotherapy methotrexate
df <- prescription_traj %>% filter(ReferenceKey == 10027297) # triple
df <- prescription_traj %>% filter(ReferenceKey == 1093041) # triple
df <- prescription_traj %>% filter(ReferenceKey == 10701858) # triple
df <- prescription_traj %>% filter(ReferenceKey == 10226382) # triple
df <- prescription_traj %>% filter(ReferenceKey == 10069729) # triple


# label pt with 1st, 2nd line tx ------------------------------------------

# use mutate and case_when a hierarchy starting with more 3rd line

# get a label per pt whether first, second, or third-line tx used (useful for all forms of plotting including stratifying the weighting score later; but cannot have reduced the cDMARD, need to have the drug names (so we know if 2 cDMARD are used → second-phase of treatment already)




# create the function to extract the treatment trajectory; function should be able to apply on class of drug, but also later on the moa


# first separate the df by reference key


# monotherapy


# dual therapy; arrange by time and see if given same time or not



# mini summary to look at the numbers we have for NA, bioo_or_bios
# prescription_traj %>% filter(!is.na(ingredient)) %>% count(ingredient, bioo_or_bios)






# rough -------------------------------------------------------------------
# Count the number of unique drugs for each patient and filter for patients with 3 or more unique drugs
patients_with_3_or_more_drugs <- prescription_traj %>%
  group_by(ReferenceKey) %>%
  summarize(n_unique_drugs = n_distinct(DrugName_clean)) %>%
  filter(n_unique_drugs >= 3) %>%
  pull(ReferenceKey)

prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[1]) # triple drugs given same time
prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[2]) # hydroxychloroquine + methotrexate in Jan 16 to March 12th, given in four rows i.e. visited doctor twice, then tofactinib + methotrexate
prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[3]) # triple drugs given same time (lefounomide, methotrexate, hydroxychloroquine)
prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[4]) #(same triple)
prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[6]) # last row called "Keep Record Only" ?
prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[10]) # seemingly hydroxychloroquine + methotrexate → etanercept + methotrexate

# grepping the brand name
# pt_with_labelled_brand <- prescription %>% 
#   filter(grepl(pattern = paste(unique(bio_df$`Brand name`), collapse = "|"),
#                x = DrugName)) %>% 
#   select(ReferenceKey) %>% 
#   pull()

# but after grepping the brandname, you STILL want other medications taken by the same pt, because this will tell us whether the pt switched drug, or took additional drugs
# prescription_sub <- prescription %>% filter(ReferenceKey %in% pt_with_labelled_brand)


# Filter the diagnosis table to only get pt who has, in their lifetime, been diagnosed with any in ra_vec
ra_vector_refkey <- diagnosis[diagnosis$All.Diagnosis.Description..HAMDCT.. %in% ra_vector, "Reference.Key."]
diagnosis_sub <- diagnosis[diagnosis$Reference.Key. %in% ra_vector_refkey, ]