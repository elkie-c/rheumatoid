# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
                 zoo, # adding back non-NA values
                 ggpubr,
                 mgsub, #for multiple substitutions
                 readxl, # read the excel drug list
                 hrbrthemes, 
                 ggalluvial, # treatment trajectory sankey)
                 plotly # heatmaps
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
weight_df$Assigned_weight <- as.numeric(weight_df$Assigned_weight)


# clean weight_df (clean ICD codes by expanding hyphen and x)---------------------------------------------------------
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
ra_vector <- unique(grep(
  pattern =
    "714\\.[0128]",
  x = diagnosis$All.Diagnosis.Description..HAMDCT..,
  value = TRUE
))

# old ra vector, includes those antepartum RA, maternal care etc
# ra_vector <- unique(grep(
#   pattern =
#     "rheumatoid|Rheumatoid|[Jj]uv.*rh|714",
#   x = diagnosis$All.Diagnosis.Description..HAMDCT..,
#   value = TRUE
# ))

# exclude this strange item called Screen-rheumatoid
# ra_vector <- grep(pattern = "^(?!.*Screen)", x = ra_vector, value = TRUE, perl = TRUE)

# select out the seronegative, glomerulonephritis
ra_vector <- grep(pattern = "glomerulonephritis|seronegative", x = ra_vector, perl = TRUE, value = TRUE, invert = TRUE)

# there are some strange diagnosis with rheum, and for the benefit of doubt (and discussion), they are as follows: now the decision is whether you would like to keep these? Could modify the expression above to do that
# Find the strings that contain "rheum" but not in in ra_vector (which we will use for analysis)
not_ra_vector <- unique(grep(
  pattern =
    "rheum|RA",
  x = diagnosis$All.Diagnosis.Description..HAMDCT..,
  value = TRUE
))

not_ra_vector[!not_ra_vector %in% ra_vector] # so these are the strings


# for pt diagnosed with cancer or autoimmune disease before RA, remove them
# for each pt diagnosed with RA, get the earliest reference date for the RA diagnosis
# Group by patient and get earliest reference date
earliest_ra <- diagnosis[diagnosis$All.Diagnosis.Description..HAMDCT.. %in% ra_vector, ] %>%
  group_by(Reference.Key.) %>%
  summarize(first_ra = min(Reference.Date.))

# double check only
# earliest_ra[earliest_ra$Reference.Key. == 5872, ]
# diagnosis[diagnosis$Reference.Key. == 5872,]

# update the table with a column on the earliest RA diagnosis if any
diagnosis_sub <- dplyr::left_join(x = diagnosis,
                                  y = earliest_ra,
                                  by = "Reference.Key.") %>%  # before next line will see some NA dates for those not diagnosed with our definition of RA
  filter(!is.na(first_ra)) # to remove those who do not have our definition of RA
  # filter(Reference.Date. <= first_ra) # lesser or equal to so we keep the RA columns and do not exclude them


# obtain a list of all biologics indications
# later, when cleaning diagnosis df, we need to remove pt with diagnosis of ANY biologics indications prior to their first date of RA, in case they may have received biologics for a purpose other than RA. I need a string vector for those indications

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
  x = diagnosis_sub$All.Diagnosis.Code..ICD9..,
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
unwanted_ra <- diagnosis_sub[other_biologic, "Reference.Key."]

# how many pts did we exclude this way?
print(paste0("At first, we have ", length(unique(diagnosis_sub$Reference.Key.)), " number of patients who fulfil our definition of RA"))

print(paste0("Then, we removed patients who were diagnosed with conditions that indicate the use of biologics at ANY point in their life. Now, there are ", length(unique(unwanted_ra)), " patients who meet this criteria. Example of their ID include the following:"))

print(paste0(unique(unwanted_ra)[1:5], collapse = ", "))

print(paste0(
  "So the number of patients who remain in the analysis are ",
  length(unique(diagnosis_sub$Reference.Key.)) - length(unique(unwanted_ra))
))

# then, unwanted_ra gives us 194 in the following line
# length(unique(unwanted_ra))
# so we have eliminated 194 of those RA pt with some kind of indications for biologic BEFORE diagnosis of RA. But later as I found out even though they do have these indications, none of those pt contributed to our analysis, likely because they were not given biologics, or they were given biologics or an unknown type i.e. biosimilar or biooriginator
diagnosis_sub <- diagnosis_sub %>% 
  filter(!Reference.Key. %in% unwanted_ra)


# get weighting score -----------------------------------------------------
# weight_df
# diagnosis_sub
diagnosis_sub %>% select(All.Diagnosis.Description..HAMDCT..)

diagnosis_sub$ICD <- regmatches(
  x = diagnosis_sub[, "All.Diagnosis.Description..HAMDCT.."],
  m = regexpr(
    pattern = "(?<=\\()[\\d\\.EV]+(?=:)",
    # after ( but not including, \ the \ character, not including the colon; [^\\(]*$ to take the last occurrence of the open bracket, don't want anymore brackets like the case of (lymph node) instead of (ICD)
    # EV because some ICD begins with that followed by the bracket
    # (?<=\\()\\d+.*(?=:)   # this is the older version which didn't work if ( followed by E or V alphabet in the ICD code
    text = diagnosis_sub[, "All.Diagnosis.Description..HAMDCT.."],
    perl = TRUE))

# diagnosis_sub[899, ] # example with the bracket lymph node
# for debugging to see where regex failed to match
# diagnosis_sub[, "All.Diagnosis.Description..HAMDCT.."][grep(pattern = "(?<=\\()[\\d\\.EV]+(?=:)", x = diagnosis_sub[, "All.Diagnosis.Description..HAMDCT.."], perl = TRUE, invert = TRUE)]


# extra steps to check after removing the 0 is indeed the intended effect; later turned out is useless as 250.00 is not the same as 250.0
# diagnosis_sub$ICD_no_0 <- gsub(pattern = ".00", x = diagnosis_sub$ICD, replacement = ".0", fixed = TRUE)
# diagnosis_sub %>% filter(ICD != ICD_no_0) %>% select(ICD, ICD_no_0, All.Diagnosis.Description..HAMDCT..)


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
cal_timepoint_score <- function(days, diagnosis_sub, weight_df) {
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
  names(scores_df)[names(scores_df) == "score_before_timepoint"] <- paste0("score_before_", days)
  
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



# question 1: Uptake of b/tsDMARDs (stratified by mode of action and bio-originator / biosimilars) by year (2010-2022) among patients with rheumatoid arthritis (RA);-------------------------------------

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



# Remove smaller intervals that are completely inclusive of another interval and of the same drug
prescription_traj %>%
  filter(ReferenceKey == 1319963) %>% 
  arrange(PrescriptionStartDate) %>% 
  group_by(ReferenceKey, DrugName_clean) %>% 
  mutate(next_start_date = lead(PrescriptionStartDate),
         next_end_date = lead(PrescriptionEndDate)) %>% 
  # select(PrescriptionStartDate, PrescriptionEndDate, DrugName_clean, next_start_date, next_end_date) %>% arrange(PrescriptionStartDate) %>% print(n = 100) %>%  # only for purpose of debugging this line
  ungroup() %>% 
  filter(next_start_date != PrescriptionEndDate | is.na(next_start_date)) %>% # so if next start date is not equal to prescription end date, or next start date is NOT empty (as in the last row) would remove; hence if next start date is empty
  select(-c(next_start_date, next_end_date)) %>% mutate(DrugName_clean = moa) %>% extract_traj()




# UNCOMMENT THIS LINE TO GET BY MOA DF----------------------------
# this line commented out but would have changed the analysis by MoA instead of DrugName_clean thereafter
# df$DrugName_clean <- df$moa

prescription_traj_list <- split(df, f = df$ReferenceKey)

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

merged_df$duration <-
  as.numeric(
    difftime(
      merged_df$PrescriptionEndDate,
      merged_df$PrescriptionStartDate,
      units = "secs"
    )
  ) / 86400

# Reorder the columns
merged_df <- merged_df %>%
  select(ReferenceKey, everything())



unique(merged_df$DrugName_clean)
table(merged_df$DrugName_clean)

# merged_df_drugnamed %>% filter(ReferenceKey == 1007790)



# so the drugnamed would be when I didn't run this line prescription_traj$DrugName_clean <- prescription_traj$moa
# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_drugnamed.rds")
# saveRDS(object = merged_df, file = "/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_moanamed.rds")



merged_df_drugnamed <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_drugnamed.rds")
moanamed_df <- readRDS("/Users/elsiechan/Desktop/kuan_folder/saved_rds/merged_df_moanamed.rds")

# moanamed has fewer rows than merged_df, is the reduced version; so we can join, and let it contain redundant information for now

# now to merge them would lead to duplicate rows in moanamed; so later if need to convert back to moa just use distinct

# Create a new column in merged_df_drugnamed with the prescription date range as an interval
merged_df_drugnamed <- merged_df_drugnamed %>%
  mutate(PrescriptionInterval_drugnamed = interval(PrescriptionStartDate, PrescriptionEndDate))

moanamed_df <- moanamed_df %>% 
  mutate(PrescriptionInterval_moa = interval(PrescriptionStartDate, PrescriptionEndDate))

# first step use left join so drugnames are all present, but have NA columns in moanamed_df
merged_df <- merged_df_drugnamed %>% left_join(
  moanamed_df,
  by = c(
    "ReferenceKey",
    "PrescriptionStartDate",
    "PrescriptionEndDate"
  ),
  suffix = c("_drugnamed", "_moa")
)


# merged_df_drugnamed %>% filter(ReferenceKey == 1319963) %>% print(n = 100)
# moanamed_df %>% filter(ReferenceKey == 1319963) %>% print(n = 100)
# merged_df %>% filter(ReferenceKey == 1319963) %>% print(n = 100)



# take the ones with NA (i.e. the entries that did not match) from merged_df, filter them out first
na_rows <- merged_df %>% filter(is.na(DrugName_clean_moa))



# for each ReferenceKey in that filtered merged_df, also grab the rows from moanamed_df

#' fill_na_with_moa
#'
#' @param a take the same Referencekey pt with NA entries, from the na_rows
#' @param b from the same ReferenceKey, the entire moa 
#'
#' @return a but instead of NA, those are filled with where the interval matches in b, the moa
#' @export
#'
#' @examples
fill_na_with_moa <- function(a, b) {
  # get the drug intervals where moa is NA; we don't use gap_output because that would be NA for all pt at the end anyways
  interval_a <- a %>% pull(PrescriptionInterval_drugnamed)
  
  # check interval_a against every interval in b$PrescriptionInterval_moa, to get the index which matches b
  overlaps <- sapply(interval_a, function(x) {
    which(x %within% b$PrescriptionInterval_moa)
  })
  
  # for the same drug class there could have been diff drugs taken, so would expect duplicate rows in b_cols
  b_cols <- b[overlaps,] %>% rename(
    DrugName_clean_moa = DrugName_clean,
    duration_moa = duration,
    gap_output_moa = gap_output
  ) %>% select(DrugName_clean_moa, duration_moa, gap_output_moa, PrescriptionInterval_moa)
  
  # drop those columns from a, and take those columns from b; filled as in the previous NA are now filled with values
  filled_rows <- a %>% 
    select(-DrugName_clean_moa, -duration_moa, -gap_output_moa, -PrescriptionInterval_moa) %>% 
    cbind(b_cols)
  
  return(filled_rows)
}


output_list <- list()


# 2154976 is the problematic ref
# ref <- 2154976
ref <- 1319963
ref <- 2209539
debug(fill_na_with_moa)
fill_na_with_moa(a, b)
View(a)

merged_df %>% filter(ReferenceKey == 2154976) %>% View()



for (ref in unique(na_rows$ReferenceKey)) {
  # create the two rows with refkey, only for that pt
  # taken from na_rows since that has already been filtered
  
  a <- na_rows %>% filter(ReferenceKey == ref) 
  b <- moanamed_df %>% filter(ReferenceKey == ref)
  
  # append the output to the list
  output_list <- c(output_list, list(fill_na_with_moa(a, b)))
}

# combine all the elements in the list into a single data frame
output_df <- bind_rows(output_list)






a %>% pull(ReferenceKey)
anti_join(merged_df_drugnamed, moanamed_df, by = "ReferenceKey") %>% View()

merged_df_drugnamed %>% pull(ReferenceKey) %>% unique()
moanamed_df %>% pull(ReferenceKey) %>% unique()









# calculate days before the use of b/tsDMARD (we don't worry about csDMARD most of the time)--------------------------------
# so these are either b or tsdmard, calculate number of days till the use of it
btsdmard <- c(tnfi, cd28, cd20, il6, jaki)

# diagnosis_sub is from clean_diagnosis see earlier subtitle of this script
# join the table to obtain the day of first_ra diagnosis
merged_df <- left_join(merged_df, 
          unique(diagnosis_sub[, c("Reference.Key.", "first_ra")]), 
          by = c("ReferenceKey" = "Reference.Key."))

merged_df$first_ra <- as.Date(merged_df$first_ra)

# sensibility check: why some pts receive prescription before their date of first dx with RA?
# return the FALSE ones because you would expect first_ra <= prescriptionstartdate

# you would expect that first_ra is smaller or equal to prescriptionstartdate since we have already excluded, earlier, those who have conditions other than RA which indicate the use of biologics. 
prescription_after_ra <- merged_df %>%
  group_by(ReferenceKey) %>%
  filter(PrescriptionStartDate == min(PrescriptionStartDate)) %>%
  ungroup() %>% 
  filter(first_ra <= PrescriptionStartDate) %>% 
  pull(ReferenceKey)

# the cohort which we potentially want to exclude from our analysis
prescription_before_ra <- merged_df %>%
  group_by(ReferenceKey) %>%
  filter(PrescriptionStartDate == min(PrescriptionStartDate)) %>%
  ungroup() %>% 
  filter(first_ra > PrescriptionStartDate) %>% 
  pull(ReferenceKey)


print(paste0("Of the ", length(unique(merged_df$ReferenceKey)), 
             " patients, we would expect that the earliest date of diagnosis of rheumatoid arthritis must have occured before the prescription of any rheumatoid arthritis drugs. This is indeed the case for ", 
             length(prescription_after_ra), 
             " patients. However, we found that there were ", 
             length(prescription_before_ra), " exceptions in which the patient received RA-related prescription before the earliest date of rheumatoid arthritis diagnosis. These patients will be excluded from our analysis as they affect the reliability of the number of days relapsed since the use of b/tsDMARD."))

# random <- 5
# diagnosis_sub %>% filter(Reference.Key. == prescription_before_ra[random]) %>% arrange(Reference.Date.)
#  
# prescription_sub %>% filter(ReferenceKey == prescription_before_ra[random]) %>% select(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate, DrugName) %>% arrange(PrescriptionStartDate)
# 
# View(diagnosis_sub %>% filter(Reference.Key. == prescription_before_ra[random]) %>% arrange(Reference.Date.))
# View(prescription_sub %>% filter(ReferenceKey == prescription_before_ra[random]) %>% select(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate, DrugName) %>% arrange(PrescriptionStartDate))




# days till first prescription
# setting earliest_start_date, so we can separate pt which we want to include in b/tsDMARD analysis
# row_number returns a column of integers indicating the row number for each row in a group
# row_number() to identify the first row within each group, and for that row, we set first_ra to the earliest_start_date. For all other rows in each group, we use the same logic as before to set first_ra to the earliest PrescriptionStartDate only if it is later than the current first_ra.
merged_df <- merged_df %>%
  arrange(ReferenceKey, PrescriptionStartDate) %>%
  group_by(ReferenceKey) %>%
  mutate(
    earliest_start_date = min(PrescriptionStartDate),
    prescription_after_ra = earliest_start_date >= first_ra
  ) %>%
  ungroup()

# code if we decided to just modify and go with the pseudo-dx date instead
# merged_df %>%
#   group_by(ReferenceKey) %>%
#   mutate(
#     earliest_start_date = min(PrescriptionStartDate),
#     first_ra = if_else(row_number() == 1 | first_ra > earliest_start_date, earliest_start_date, first_ra)
#   ) %>%
#   ungroup()


# merged_df %>% filter(prescription_after_ra == TRUE) %>% pull(ReferenceKey) %>% unique()

# number of days from dx to prescription of first_ra drug; normally should be positive as prescription comes after dx
merged_df$dx_to_prescription <- merged_df$earliest_start_date - merged_df$first_ra

# if negative, it means RA ICD diagnosis came after the first prescription of RA-related drug
num_vec <- merged_df %>% filter(dx_to_prescription < 0) %>% distinct(ReferenceKey, dx_to_prescription) %>% pull(dx_to_prescription)
  

range_vec <- cut(as.numeric(num_vec), breaks = c(-Inf, -365, -100, -31, -14, -7, -1), 
                 labels = c("-365 to -101", "-100 to -31", "-30 to -15", "-14 to -8", "-7 to -2", "-1"))

# Create a frequency table of the counts within each category
table(range_vec)


# is just the dx_to_prescription column; already there, no need below line
# merged_df %>% mutate(days_to_first_drug = difftime(first_ra, earliest_start_date, units = "days"))
#

options(max.print = 50)
# just to hv an idea of the distribution of values
merged_df %>% pull(DrugName_clean) %>% table()

# days till first cdmard
# negative would be FALSE for prescription_after_ra
merged_df <- merged_df %>%
  filter(DrugName_clean == "cdmard") %>%
  group_by(ReferenceKey) %>% # so would give us earliest startdate by reference key
  mutate(earliest_cdmard_date = min(PrescriptionStartDate)) %>%
  mutate(days_to_cdmard = earliest_cdmard_date - first_ra) %>%
  distinct(ReferenceKey, days_to_cdmard) %>%  # only need unique to prevent many-to-many relationship
  select(ReferenceKey, days_to_cdmard) %>% 
  full_join(merged_df) # so merged_df gets a new column, which is days_to_cdmard; rather than left_join

# merged_df %>% filter(is.na(days_to_cdmard))

# you should get some NA values because there are patients who did not take cdmard
# any(is.na(merged_df %>% pull(days_to_cdmard)))
# any(is.na(temp %>% pull(days_to_cdmard)))


# date till first bsdmard if any (NA otherwise)
# btsdmard is made of (tnfi, cd28, cd20, il6, jaki)
# merged_df %>% pull(DrugName_clean) %>% unique()

merged_df <- merged_df %>% 
  filter(grepl(pattern = paste(c("jaki", "tnfi", "cd20", "cd28", "il6"), collapse = "|"),
               x = DrugName_clean)) %>%
  group_by(ReferenceKey) %>% 
  mutate(earliest_btsdmard_date = min(PrescriptionStartDate)) %>% 
  mutate(days_to_btsdmard = earliest_btsdmard_date - first_ra) %>%
  distinct(ReferenceKey, days_to_btsdmard) %>%
  select(ReferenceKey, days_to_btsdmard) %>% 
  full_join(merged_df)

# print(temp %>% filter(is.na(days_to_btsdmard)), n = 200)


# combine scores_df with merged_df ----------------------------------------
merged_df <- dplyr::full_join(merged_df, scores_df, by = c("ReferenceKey" = "Reference.Key."))


# 1: How long patients can use b/ts DMARDs in Hong Kong, potentially stratified by years to illustrate the improved uptake rate (My personal guesswork is new patients such as diagnosis in 2019 should access to b/ts DMARDs faster than old patients diagnosis in 2009)


# 2: Among b/ts DMARDs, which one is prefered by local clinicians as first/second line option? Could also stratified by years to demonstrate the change of market share as time goes by (For example, i know the use of infliximab is decreasing year by year)

# for this would need to table without changing into MoA


# 3: The access time and uptake rate of biosimilars in Hong Kong

merged_df %>% pull(DrugName_clean) %>% unique()




# filter to only include when tx duration > 30 days (for continuous period) to be considered significant


# batched ideas as follows:
# filter based on duration and consider removing if less than a threshold
# apply again gap_merge_per_drug so recalculate the gap and then merge based on threshold again




# gantt's chart for treatment trajectory ----------------------------------
# Create a data frame with start, end, drug, and patient columns
df <- merged_df %>%
  head(10000) %>%
  # filter(ReferenceKey == "10140118") %>% 
  mutate(patient = as.factor(ReferenceKey),
         end = PrescriptionEndDate) %>%  # ifelse would give unintended bridges
         # end = ifelse(is.na(gap_output), 
         #              PrescriptionEndDate, 
         #              PrescriptionStartDate + gap_output)) %>%
  select(start = PrescriptionStartDate, end, drug = DrugName_clean, patient)

# sample df just for showing the gantt's chart for one pt, illustrative purposes only
# extract_traj(prescription_traj_list[["10140118"]]) # eg to check validity of the plot

# df <- data.frame(
#   start = as.Date(c("2019-05-15", "2019-06-05", "2019-07-02", "2019-07-29", "2021-02-28", "2021-07-05", "2022-03-08", "2022-05-30")),
#   end = as.Date(c("2019-06-05", "2019-07-02", "2019-07-29", "2021-02-28", "2021-07-05", "2022-03-08", "2022-04-10", "2023-01-08")),
#   drug = c("cdmard", "cdmard+jaki", "cdmard", "cdmard+jaki", "cdmard", "cdmard+il6", "il6", "cdmard+il6"),
#   patient = as.factor(c("10140118", "10140118", "10140118", "10140118", "10140118", "10140118", "10140118", "10140118"))
# )


unique(df$drug)
unique(merged_df$DrugName_clean)
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
    showticklabels = FALSE,
    tickfont = list(size = 6)),
  title = list(text = "<b>Treatment Trajectories of Patients</b>", font = list(size = 15)),
  hovermode = 'closest'
)

fig

# number of days before first change in regimen

# number of days before ADDING one drug

# by MoA?

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

colnames(prescription)
colnames(inpatient)
colnames(diagnosis)
View(death)
View(diagnosis)
View(inpatient)
View(prescription)

