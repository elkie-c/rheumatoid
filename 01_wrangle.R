# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
                 zoo, # adding back non-NA values
                 ggpubr,
                 mgsub, #for multiple substitutions
                 readxl, # read the excel drug list
                 hrbrthemes)

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
cdmard <- c("Sulfasalazine", "Mycophenolate", "Methotrexate", "Leflunomide", "Hydroxychloroquine", "Cyclophosphamide", "Ciclosporin", "Azathioprine", "Apremilast")

# clean diagnosis ------------------------------------------------------
# firstly, only consider pt if diagnosed with RA at least at some point in life
# the following line shows that other than rheumatoid arthritis, you also have rheumatoid nodule arthritis
ra_vector <- unique(grep(
  pattern =
    "rheumatoid|Rheumatoid|[Jj]uv.*rh|714",
  x = diagnosis$All.Diagnosis.Description..HAMDCT..,
  value = TRUE
))

# exclude this strange item called Screen-rheumatoid
ra_vector <- grep(pattern = "^(?!.*Screen)", x = ra_vector, value = TRUE, perl = TRUE)

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
# turns out all the pt given already diagnosed with RA duh!
diagnosis_sub <- dplyr::left_join(x = diagnosis,
                                  y = earliest_ra,
                                  by = "Reference.Key.") %>%
  filter(Reference.Date. <= first_ra) # lesser or equal to so we keep the RA columns and do not exclude them


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

unwanted_ra <- diagnosis_sub[other_biologic, "Reference.Key."]

# how many pts did we exclude this way?
# at first, we have 24099 unique pt with those RA conditions, cancer or not, inclusive.
# unique(diagnosis_sub$Reference.Key.)
# then, unwanted_ra gives us 194 in the following line
# length(unique(unwanted_ra))
# so we have eliminated 194 of those RA pt with some kind of indications for biologic BEFORE diagnosis of RA. But later as I found out even though they do have these indications, none of those pt contributed to our analysis, likely because they were not given biologics, or they were given biologics or an unknown type i.e. biosimilar or biooriginator
diagnosis_sub <- diagnosis_sub %>% 
  filter(!Reference.Key. %in% unwanted_ra)


# dplyr::arrange(diagnosis_sub, Reference.Key.)


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

adalimumab <- bio_df %>% filter(Ingredient == "ADALIMUMAB") %>% select(`Brand name`) %>% pull()
adalimumab <- unique(c("ADALIMUMAB", adalimumab))

rituximab <- bio_df %>% filter(Ingredient == "RITUXIMAB") %>% select(`Brand name`) %>% pull()
rituximab <- unique(c("RITUXIMAB", rituximab))

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
bios_string <- bio_df %>% filter(Type == "Biosimilar") %>% select(`Brand name`) %>% pull()

# now, create the bio column (which says if it is biosimilar or biooriginator)
prescription_sub <- prescription_sub %>% 
  mutate(
    bioo_or_bios = case_when(
      stringr::str_detect(DrugName, paste(bioo_string, collapse = "|")) ~ "o", # using string infliximab, because sometimes only brand name, but not ingredient name given. So you would miss that if you hadn't use this string
      stringr::str_detect(DrugName, paste(bios_string, collapse = "|")) ~ "s",
      TRUE ~ NA_character_)
  )

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
  ) # note even though a lot of brands, just for sake of completeness; our data only has HUMIRA, REMICADE, and REMSIMA

# to prove the point our data only has HUMIRA, REMICADE, REMSIMA
# grep(pattern = paste(bio_df %>% select(`Brand name`) %>% pull(), collapse = "|"), x = unique(prescription$DrugName), value = TRUE)



# question 1: Uptake of b/tsDMARDs (stratified by mode of action and bio-originator / biosimilars) by year (2010-2022) among patients with rheumatoid arthritis (RA);--------
# now we have a prescription table with cleaned information. Now we need to merge the diagnosis table to ONLY get the pts that meet our criteria, and look at the descriptive statistics of their uptake
prescription_sub <- prescription_sub %>% filter(ReferenceKey %in% diagnosis_sub$Reference.Key.) # essential step

# very strangely, pts in prescription_sub is merely a subset of pts in diagnosis_sub. It makes sense the diagnosis_sub is more, because there are pt diagnosed with RA and not given biologics. However, I am confused the pts in prescrpition_sub is entirely a subset. Because I would expect there to be pt prescribed with biologics, but have nothing to do with RA. Or pt prescribed with biologics for autoimmune conditions before RA. But later I realised they may have been prescribed with unlabelled biologics anyways, or simply not given biologics

# full_join(prescription_sub, diagnosis_sub[, c("Reference.Key.", "first_ra")], by = c("ReferenceKey" = "Reference.Key."))




# question 2 treatment trajectory -----------------------------------------
# remove white space and non-breaking space; convert to capital letters
drugs <- gsub("[[:space:]\u00A0]", "", 
              toupper(c(cdmard, bioo, bio_df$`Brand name`, unique(bio_df$Ingredient))))

# these are drugs for treating RA, may be switched around; but what about brand names?
# prescription <- prescription[1:300000, ]

# Clean the drug names using case_when() and str_detect()
prescription_traj <- prescription %>%
  mutate(
    DrugName_clean = case_when(
      str_detect(DrugName, str_c(drugs, collapse = "|")) ~ str_extract(DrugName, str_c(drugs, collapse = "|")),
      TRUE ~ NA_character_
    )
  )

# where it is not NA; so we only take the non-NA entries
prescription_traj <- prescription_traj %>% filter(!is.na(DrugName_clean))

# so sometimes dual drug therapy. Not so straightforward like using one drug then switch to another. But even if combine actually is not undoable. 

# data exploration
prescription_traj %>% filter(ReferenceKey == 20638) # hydroxychloroquine + lefluonomide, never switched
prescription_traj %>% filter(ReferenceKey == 737169) # hydroxychloroquine + lefuonomide, never switched
prescription_traj %>% filter(ReferenceKey == 679310) # tocilizumab monotherapy
prescription %>% filter(ReferenceKey == 679310)  # in case interested to look at other drugs taken by this pt
prescription_traj %>% filter(ReferenceKey == 666358) # methotrexate
prescription_traj %>% filter(ReferenceKey == 673928)  # methotrexate
prescription_traj %>% filter(ReferenceKey == 852239) # methotrexate + lefluonomide, Jan to March, just these two
prescription_traj %>% filter(ReferenceKey == 847624) # methotrexate only
prescription_traj %>% filter(ReferenceKey == 673928) # methotrexate only

# looking at those specifically with three drugs (so triple therapy, or had some switching (?))
prescription_traj

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
prescription_traj %>% filter(ReferenceKey == patients_with_3_or_more_drugs[9]) # last row called "Keep Record Only" ?

output <- "/Users/elsiechan/Desktop/prescription_traj.rds"
saveRDS(object = prescription_traj, file = output)


View(prescription_traj)
View(prescription_traj[, c("ReferenceKey", "PrescriptionStartDate", "PrescriptionEndDate", "DrugName", "DrugName_clean")])

# but after grepping the brandname, you STILL want other medications taken by the same pt, because this will tell us whether the pt switched drug, or took additional drugs
# prescription_sub <- prescription %>% filter(ReferenceKey %in% pt_with_labelled_brand)







# rough -------------------------------------------------------------------
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

