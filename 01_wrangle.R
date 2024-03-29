# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
                 zoo, # adding back non-NA values
                 ggpubr,
                 mgsub, #for multiple substitutions
                 readxl, # read the excel drug list
                 # hrbrthemes, 
                 RColorBrewer,
                 ggalluvial, # treatment trajectory sankey)
                 # plotly, # heatmaps
                 svglite, # save svgs
                 # MASS, # linear regression stepwise # select disrupts dplyr
                 # ggforce # for sankey
                 # knitr, # for tables
                 # kableExtra,
                 # networkD3, # sankey but failed in the end
                 htmlwidgets,
                 riverplot,
                 lubridate,
                 ggraph,
                 # RSelenium, # save plotly
                 # orca, # save plotly
                 # reticulate, # save plotly
                 funchir, # find redundant packages
                 forcats # for rearranging factors for ggplot in 06
)

# replace with your path to kuan's folder of rds
path <- "/Users/elsiechan/Documents/kuan_folder"
setwd(path)
death <- readRDS("Death.RDS")
diagnosis <- readRDS("Diagnosis.RDS")
inpatient <- readRDS("Inpatient.RDS")
prescription <- readRDS("Prescription.RDS")


# clean death sheet -------------------------------------------------------
death <- death %>%
  rename(main_death = `DeathCause(MainCause)`) %>%
  rename(death_date = DateofRegisteredDeath)

# had to use this unclever method because death contains some list in some columns, is a complex object
death_df <- data.frame(ReferenceKey = death$ReferenceKey, 
                       death_date = death$death_date, 
                       main_death = death$main_death)

# read excel sheet of drug list -------------------------------------------
# Set the file path of the Excel file
excel_file <- "Drug list.xlsx"

# Read each sheet as a separate data frame
bio_df <- read_excel(excel_file, sheet = 1)
bioo_df <- read_excel(excel_file, sheet = 2)

# fill with previous non-missing value to give back the value to NA
bio_df <- tidyr::fill(bio_df, Ingredient)

bioo <- bioo_df$Agent

# cdmard_df <- read_excel(excel_file, sheet = 3, col_names = FALSE, trim_ws = TRUE)
# cdmard <- toString(cdmard_df)

# manually extracted because the formating of the sheet is a little crude

cdmard <- c("Sulfasalazine", "Mycophenolate", "Methotrexate", "Leflunomide", "Hydroxychloroquine", "Cyclophosphamide", "Ciclosporin", "Azathioprine", "Apremilast", "Cyclosporin")
cdmard <- toupper(cdmard)


# clean diagnosis ------------------------------------------------------
# firstly, only consider pt if diagnosed with RA at least at some point in life
# the following line shows that other than rheumatoid arthritis, you also have rheumatoid nodule arthritis

# rename to Reference.date, since second time diagnosis.rds the column names are renamed
diagnosis <- diagnosis %>% rename(Reference.Date. = date)

print(paste0("To begin with, we have ", length(unique(diagnosis$ReferenceKey)), " (diagnosis.rds) patients in our cohort before we filtered out those for RA specifically."))

ra_vector <- unique(grep(
  pattern =
    "714\\.[01289]",
  x = diagnosis$description,
  value = TRUE
))

unique(grep(
  pattern =
    "714",
  x = diagnosis$description,
  value = TRUE
))

# there are some strange diagnosis with rheum
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

print(paste0("Then, we removed patients who were ALSO diagnosed with conditions that indicate the use of biologics at ANY point in their life. Now, there are ", length(unique(unwanted_ra)), " patients who meet this condition. Example of their ID include the following:"))

print(paste0(unique(unwanted_ra)[1:5], collapse = ", "))

print(paste0(
  "So the number of patients who remain in the analysis is ",
  length(unique(diagnosis_sub$ReferenceKey)) - length(unique(unwanted_ra))
))

# filter to remove where first_ra is before 2010, because our prescription data is only from 2009 onwards
diagnosis_sub <- diagnosis_sub %>%
  filter(as.Date(first_ra) >= as.Date("2010-01-01")) %>% 
  filter(as.Date(first_ra) <= as.Date("2022-12-31"))

print(paste0(
  "Because our prescription data is from 2009 onwards, we removed patients diagnosed with RA before 2010, and after 2022-12-31, leaving ", 
  length(unique(diagnosis_sub$ReferenceKey))
))

# one potential problem is if pt was diagnosed e.g. with RA at 2022 December. A year from then would be 2023 Dec when we do not have the data yet, but our data would make it appear as tho the pt has stuck with that biologic. Hence we did that filter above.  

# NOTE the step removing pt with any RA-related drug use before 2010-01-01 occurs LATER in this script


# clean prescription df which obtains bioo or bios, for those 3 drugs --------------------------------------------------------------
# from read excel sheet, we have obtained the drug list already. The useful variables would be biosimilar_df, bioo, and cdmard

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
# and therefore I could add the drug name under the bioo_string
# don't HAVE to worry about if it shows both the ingredient and biosimilar; later my mutate if starts by looking for biosimilar name first. This bioo you should still include the ingredient name to capture it in case no other info provided then bioo
bioo_string <- c(bioo_string, c("INFLIXIMAB", "ADALIMUMAB", "RITUXIMAB"))
bios_string <- bio_df %>% filter(Type == "Biosimilar") %>% select(`Brand name`) %>% pull()
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



# create prescription_traj to label all the RELEVANT drugs ------------------------------------

# now we have a prescription table with cleaned information. Now we need to merge the diagnosis table to ONLY get the pts that meet our criteria, and look at the descriptive statistics of their uptake
# so filter prescription table to get only relevant patients
prescription_sub <- prescription_sub %>% filter(ReferenceKey %in% diagnosis_sub$ReferenceKey) # essential step

# remove white space and non-breaking space; convert to capital letters
drugs <- gsub("[[:space:]\u00A0]", "", 
              toupper(c(cdmard, bioo, bio_df$`Brand name`)))

# deliberately removed the adalimumab, rituximab, infliximab, or else i would lose some of them in the process
drugs <- setdiff(drugs, c("INFLIXIMAB", "ADALIMUMAB", "RITUXIMAB"))

# certolizumab pegol, just detect certolizumab instead
drugs <- replace(drugs, drugs == "CERTOLIZUMABPEGOL", "CERTOLIZUMAB")

# check for certolizumab; just searching certolizumab also detects certolizumabpegol
# grep(x = prescription_sub$DrugName, pattern = "CERTOLIZUMAB")
# prescription_sub$DrugName[grep(x = prescription_sub$DrugName, pattern = "CERTOLIZUMAB")]

# Clean the drug names using case_when() and str_detect()
# then after using those brand names, do this cleaning again but with our three drugs, so ensure if there is both the ingredient name and biosimilar brand name and bio-originator brand name, it would preferentially be converted to bio-originator or biosimilar brand name first; then check again if missing both for just the ingredient name which would just be the bio-originator 
prescription_traj <- prescription_sub %>%
  mutate(
    DrugName_clean = case_when(
      str_detect(DrugName, str_c(drugs, collapse = "|")) ~ str_extract(DrugName, str_c(drugs, collapse = "|")), # extracts just the matching part first
      str_detect(DrugName, str_c(c("INFLIXIMAB", "ADALIMUMAB", "RITUXIMAB"), collapse = "|")) ~ str_extract(DrugName, str_c(c("INFLIXIMAB", "ADALIMUMAB", "RITUXIMAB"), collapse = "|")),
      TRUE ~ NA_character_) # this time NA if doesn't fall into any of our drug categories
    )

# check, if NA, what is it?
# prescription_traj %>% filter(is.na(DrugName_clean)) %>% pull(DrugName) %>% unique()

# check drug matched is correct
# unique(prescription_traj$DrugName_clean)
# any(str_detect(prescription_sub$DrugName, "ADALIMUMAB"))

# part of the flowchart
print(
  paste0(
    "Of the ",
    prescription_traj %>% distinct(ReferenceKey) %>% nrow(),
    " patients, ",
    prescription_traj %>% distinct(ReferenceKey) %>% nrow() - prescription_traj %>% filter(!is.na(DrugName_clean)) %>% distinct(ReferenceKey) %>% nrow(),
    " of the RA patients did not have any drug related to RA i.e. cdmard, b/tsmard prescribed to them even though they were labelled with an RA diagnosis. Conversely, ", 
    prescription_traj %>% filter(!is.na(DrugName_clean)) %>% distinct(ReferenceKey) %>% nrow(),
    " RA patients had received any drug of the aforementioned classes."
  )
)

# where it is not NA; so we only take the non-NA entries
prescription_traj <- prescription_traj %>% filter(!is.na(DrugName_clean))

drug_before_2009 <- prescription_traj %>% 
  filter(year(PrescriptionStartDate) < 2010) %>% 
  # filter(!is.na(DrugName_clean)) %>% 
  distinct(ReferenceKey) %>% pull()


print(paste0(
  "Previously I removed patient with RA diagnosis before 2010. So the patients which remain used RA drug after 2010. But now I also have to remove patients who had a cdmard or btsdmard prescription before 2010 (even if they were diagnosed after).Their number is ", 
  length(drug_before_2009), 
  ". So the number drops from ", 
  prescription_traj%>% pull(ReferenceKey) %>% unique() %>% length(), 
  " to ", 
  prescription_traj%>% pull(ReferenceKey) %>% unique() %>% length() - length(drug_before_2009), "."
))

# ensure none of it is found there
prescription_traj <- prescription_traj %>%
  filter(!ReferenceKey %in% drug_before_2009) 
  # distinct(ReferenceKey) %>%
  # nrow()

# then if you rerun the print line above prescription_traj code you will get 9189 which is not true because you already subtracted
# done filtering, 10186, for the manuscript preparation


tnfi <- bioo_df %>% filter(`Mode of action` == "Tumor Necrosis Factor Inhibitor") %>%  pull(Agent) %>% toupper()
#  correct to get rid of pegol
tnfi <- replace(tnfi, tnfi == "CERTOLIZUMAB PEGOL", "CERTOLIZUMAB")

cd28 <- bioo_df %>% filter(`Mode of action` == "CD28") %>%  pull(Agent) %>% toupper()
cd20 <- bioo_df %>% filter(`Mode of action` == "CD20") %>%  pull(Agent) %>% toupper()
il6 <- bioo_df %>% filter(`Mode of action` == "IL-6") %>%  pull(Agent) %>% toupper()
jaki <- bioo_df %>% filter(`Mode of action` == "Janus kinase inhibitor") %>%  pull(Agent) %>% toupper()

# now is just brute force to extract the brand names as there is no alternative, clever, and meaningful way of doing so
# as per Kuan's suggestions to simplify the analysis and since no economic significance just umbrella the cdmard under cdmard, makes it easier
prescription_traj <- prescription_traj %>% 
  mutate(
    DrugName_clean = case_when(
      DrugName_clean %in% cdmard ~ "cdmard",
      TRUE ~ DrugName_clean
    )
  )

prescription_traj <- prescription_traj %>% 
  mutate(
    DrugName_clean_os = case_when(
      str_detect(DrugName_clean, "REMICADE") ~ "INFLIXIMAB_o",
      str_detect(DrugName_clean, "REMSIMA") ~ "INFLIXIMAB_s",
      str_detect(DrugName_clean, "HUMIRA") ~ "ADALIMUMAB_o",
      str_detect(DrugName_clean, "HULIO") ~ "ADALIMUMAB_s",
      str_detect(DrugName_clean, "ADALLOCE") ~ "ADALIMUMAB_s",
      str_detect(DrugName_clean, "AMGEVITA") ~ "ADALIMUMAB_s",
      str_detect(DrugName_clean, "IDACIO") ~ "ADALIMUMAB_s",
      str_detect(DrugName_clean, "HYRIMOZ") ~ "ADALIMUMAB_s",
      str_detect(DrugName_clean, "MABTHERA") ~ "RITUXIMAB_o",
      str_detect(DrugName_clean, "RIXATHON") ~ "RITUXIMAB_s",
      str_detect(DrugName_clean, "TRUXIMA") ~ "RITUXIMAB_s",
      str_detect(DrugName_clean, "INFLIXIMAB") ~ "INFLIXIMAB_o", # kept the three ingredients name at the end because if that is the name which remains, it is the biooriginator

      str_detect(DrugName_clean, "ADALIMUMAB") ~ "ADALIMUMAB_o",
      str_detect(DrugName_clean, "RITUXIMAB") ~ "RITUXIMAB_o",
      TRUE ~ DrugName_clean)
  ) # note even though a lot of brands, just for sake of completeness; our data has mostly HUMIRA, REMICADE, and REMSIMA; then a few more like MABTHERA, RIXATHON, TRUXIMA, AMGEVITA, HYRIMOZ

# checked to make sure behaves correctly
# unique(prescription_traj$DrugName_clean_os)
# this point onwards the brand names have disappeared


# now after creating the os column, also change for the normal column
prescription_traj <- prescription_traj %>% 
  mutate(
    DrugName_clean = case_when(
      str_detect(DrugName_clean, "REMICADE") ~ "INFLIXIMAB",
      str_detect(DrugName_clean, "REMSIMA") ~ "INFLIXIMAB",
      str_detect(DrugName_clean, "HUMIRA") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "HULIO") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "ADALLOCE") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "AMGEVITA") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "IDACIO") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "HYRIMOZ") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "MABTHERA") ~ "RITUXIMAB",
      str_detect(DrugName_clean, "RIXATHON") ~ "RITUXIMAB",
      str_detect(DrugName_clean, "TRUXIMA") ~ "RITUXIMAB",
      str_detect(DrugName_clean, "INFLIXIMAB") ~ "INFLIXIMAB",
      str_detect(DrugName_clean, "ADALIMUMAB") ~ "ADALIMUMAB",
      str_detect(DrugName_clean, "RITUXIMAB") ~ "RITUXIMAB",
      TRUE ~ DrugName_clean)
  ) 

print(
  paste0(
    "Of the ",
    prescription_traj %>% distinct(ReferenceKey) %>% nrow(),
    " patients, ",
    prescription_traj %>% filter(DrugName_clean != "cdmard") %>% distinct(ReferenceKey) %>% nrow(),
    " of the RA patients had taken bDMARD and are therefore included in the biologics-only treatment trajectory analysis."
  )
)

cdmard <- c(cdmard, "cdmard")

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


# prescription_traj %>% filter(ReferenceKey == 2154976) %>% arrange(PrescriptionStartDate) %>% tail()

# this line to give extra info if O or S being used
# prescription_traj <- prescription_traj %>%
#   mutate(DrugName_clean_os = case_when(
#     bioo_or_bios == "o" ~ paste0(DrugName_clean, "_o"),
#     bioo_or_bios == "s" ~ paste0(DrugName_clean, "_s"),
#     TRUE ~ DrugName_clean
#   ))

saveRDS(object = prescription_traj, file = paste0(path, "/saved_rds/prescription_traj.rds"))
# prescription_traj <- readRDS(file = paste0(path, "/saved_rds/prescription_traj.rds"))

# death and incidence -----------------------------------------------------
# calculate now rather than later because later when I merge the treatment trajectories I remove non-btsDMARD patients

# diagnosis_sub is from clean_diagnosis see earlier subtitle of this script
# join the table to obtain the day of first_ra diagnosis
incidence_df <- left_join(prescription_traj,
                       unique(diagnosis_sub[, c("ReferenceKey", 
                                                "first_ra")]),
                       by = "ReferenceKey")

incidence_df$first_ra <- as.Date(incidence$first_ra)

# death_df from earlier part of script
incidence_df <- incidence_df %>%
  left_join(death_df, by = "ReferenceKey")

incidence_df$first_ra_year <- format(as.Date(incidence_df$first_ra), "%Y")

# Calculate the cumulative incidence of patients based on "first_ra" column
cumulative_incidence <- incidence_df %>%
  group_by(first_ra_year) %>%
  summarise(total_patients = n_distinct(ReferenceKey)) %>%
  mutate(cumulative_patients = cumsum(total_patients))



# (debug only) treatment trajectory (would allow us to label pt) -----------------------------------------
# prescription_traj_list <- prescription_traj_list[1:50]

# For debugging etc
# Ref <- 2209539
# df <- df %>% filter(ReferenceKey == Ref)
# df <- df %>% arrange(ReferenceKey, PrescriptionStartDate) %>% head(10000)
# 
# # testing the function after moa changes
# merged_df_drugnamed %>% filter(ReferenceKey == Ref) %>% print(n = 100)
# moanamed_df %>% filter(ReferenceKey == Ref) %>% print(n = 100)
# debug(extract_traj)
# debug(gap_merge_per_drug)
# debug(decompose_dates)
# undebug(extract_traj)
# undebug(gap_merge_per_drug)
# undebug(decompose_dates)
# df %>% filter(ReferenceKey == Ref) %>% select(PrescriptionStartDate, PrescriptionEndDate, DrugName_clean) %>% arrange(PrescriptionStartDate) %>% print
# 
# df %>% filter(ReferenceKey == Ref)

# filter out cDMARDs leaving btsdmards AND reset duration if 0 days ----------------------------------------------------------
# prescription_traj <- readRDS(paste0(path, "/saved_rds/prescription_traj.rds"))
btsdmard <- c(tnfi, cd28, cd20, il6, jaki)

# filter out cdmards
df <- prescription_traj %>%
  filter(DrugName_clean %in% btsdmard)

# looking at just those referencekey and matching the df against it to look at what's going on with those 0 days injections;
# → some which claims that the prescription period is one day has DAILY on the instructions, although most have "ONCE"
# print(df %>% filter(ReferenceKey %in% (df %>% filter(duration == 0) %>% head(7) %>% pull(ReferenceKey))), n = 50)

# to show if those with duration of 0 could be multiple drugs
# df %>% filter(duration == 0) %>% pull(DrugName_clean_os) %>% table()

# hence if the prescription start and end date are the same, now then i change the end date to one day after the start date then at least they have a duration of 1 day (which is at the minimal, true)
df <- df %>%
  mutate(PrescriptionEndDate = ifelse(PrescriptionStartDate == PrescriptionEndDate,
                                      as.POSIXct(as.Date(PrescriptionEndDate) + 1),
                                      PrescriptionEndDate))

# change seconds to days
df$duration <-
  as.numeric(
    difftime(
      df$PrescriptionEndDate,
      df$PrescriptionStartDate,
      units = "secs"
    )
  ) / 86400


# NOTE prescription_traj2 there is a 2
saveRDS(object = df, file = paste0(path, "/saved_rds/prescription_traj2.rds"))




# code run altogether three times in this order: firstly trajectory by drugnamed ---------------------------------------
# df <- readRDS(paste0(path, "/saved_rds/prescription_traj2.rds"))

source("/Users/elsiechan/Documents/GitHub/rheumatoid/02_extract_traj.R")
saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_drugnamed.rds"))

# secondly trajectory by MOA-------------------------
# rationale is: so have to rerun from this line on to generate different df; the custom functions just use DrugName_clean so it would be too troublesome if we put in this as a new column instead, so stick to it
df$DrugName_clean <- df$moa
source("/Users/elsiechan/Documents/GitHub/rheumatoid/02_extract_traj.R")
saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_moanamed.rds"))

# merged_df %>% distinct(ReferenceKey) # 1716

# thirdly trajectory by O or S-----------------------
# using the same, just overwrite the column name so didn't have to change the rest of the code
df$DrugName_clean <- df$DrugName_clean_os
source("/Users/elsiechan/Documents/GitHub/rheumatoid/02_extract_traj.R")
saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_drugnamed_os.rds"))

# have to readRDS again because the same merged_df gets overwritten
merged_df_drugnamed <- readRDS(paste0(path, "/saved_rds/merged_df_drugnamed.rds"))
moanamed_df <- readRDS(paste0(path, "/saved_rds/merged_df_moanamed.rds"))
merged_df_drugnamed_os <- readRDS(paste0(path, "/saved_rds/merged_df_drugnamed_os.rds"))

# moanamed has fewer rows than merged_df, is the reduced version; so we can join, and let it contain redundant information
# now to merge them would lead to duplicate rows in moanamed; so later if need to convert back to moa just use distinct

# merge the drug name with moa, so redundant rows for moa (to merge all three dfs together, merged_df_drugnamed_os, merged_df_drugnamed, then moanamed_df)------------
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

# merged_df %>% distinct(ReferenceKey)
# merged_df %>% filter(drug_os != drug) %>% View()
# merged_df %>% filter(ReferenceKey == 10174025) %>% View()

saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df.rds"))

# calculate days before the use of b/tsDMARD (we don't worry about csDMARD)--------------------------------
# merged_df <- readRDS(paste0(path, "/saved_rds/merged_df.rds"))


# so these are either b or tsdmard, calculate number of days till the use of it

# diagnosis_sub is from clean_diagnosis see earlier subtitle of this script
# join the table to obtain the day of first_ra diagnosis
merged_df <- left_join(merged_df,
                       unique(diagnosis_sub[, c("ReferenceKey", 
                                                "first_ra")]),
                       by = "ReferenceKey")

merged_df$first_ra <- as.Date(merged_df$first_ra)

# prescription after vs prescription before ra ----------------------------
# setting index date, so we can separate pt which we want to include in b/tsDMARD analysis


# you would expect that first_ra is smaller or equal to prescriptionstartdate since we have already excluded, earlier, those who have conditions other than RA which indicate the use of biologics. 
# sensibility check: why some pts receive prescription before their date of first dx with RA? some just human errors, also cdmard start first
# if FALSE, the cohort which we potentially want to exclude from part of our analysis, not all, decision later
merged_df <- merged_df %>%
  arrange(ReferenceKey, PrescriptionStartDate) %>%
  group_by(ReferenceKey) %>%
  mutate(
    earliest_prescription = min(PrescriptionStartDate),
    prescription_after_ra = earliest_prescription >= first_ra,
    index_date = min(earliest_prescription, first_ra)
  ) %>%
  ungroup()


# debugging to make sure correct
# merged_df %>% filter(prescription_after_ra == FALSE) %>% pull(ReferenceKey) %>% unique()
# merged_df %>% filter(ReferenceKey == 1342880) %>% View()


print(paste0("Of the ", length(unique(merged_df$ReferenceKey)), 
             " patients, we would expect that the earliest date of diagnosis of rheumatoid arthritis must have occurred before the prescription of any rheumatoid arthritis drugs. This is indeed the case for ", 
             merged_df %>% filter(prescription_after_ra == TRUE) %>% distinct(ReferenceKey) %>% count(), 
             " patients. However, we found that there were ", 
             merged_df %>% filter(prescription_after_ra == FALSE) %>% distinct(ReferenceKey) %>% count(), " exceptions in which the patient received RA-related prescription before the earliest date of rheumatoid arthritis diagnosis. These patients will be excluded from our analysis as they affect the reliability of the number of days relapsed since the use of b/tsDMARD."))

# code if we decided to just modify and go with the pseudo-dx date instead
# merged_df %>%
#   group_by(ReferenceKey) %>%
#   mutate(
#     first_ra = if_else(row_number() == 1 | first_ra > earliest_prescription, earliest_prescription, first_ra)
#   ) %>%
#   ungroup()


# combine death with merged_df --------------------------------------------
merged_df <- merged_df %>%
  left_join(death_df, by = "ReferenceKey")

# create column for last record (whichever earlier: death date or last PrescriptionEndDate)  --------

# max_date is scheduled to be a later date than our data - but this is not expected to have a effect on our data
# max_date <- max(merged_df$PrescriptionStartDate, na.rm = TRUE)

# if death date is present, then last_record is that. If death date is absent, use the maximum prescriptionEndDate
merged_df <- merged_df %>%
  group_by(ReferenceKey) %>%
  mutate(last_record = ifelse(!is.na(death_date), 
                              yes = as.Date(death_date), # ensure correct formatting; doesn't work if done at a mutate step
                              no = max(PrescriptionEndDate))) %>% 
  ungroup() %>% 
  mutate(last_record = as.Date(last_record))
  # select(ReferenceKey, PrescriptionStartDate, PrescriptionEndDate, last_record, death_date) %>% View()

# merged_df %>% distinct(main_death)
# typeof(merged_df)
# str(merged_df)
# sapply(merged_df, typeof)
# merged_df %>% select(ReferenceKey)


# follow_up: days from RA diagnosis to death -----------------------------------------
merged_df <- merged_df %>%
  mutate(overall_follow_up = difftime(last_record, index_date, units = "days"))

# Extract the year of diagnosis of the RA
merged_df$first_ra_year <- format(merged_df$first_ra, "%Y")


# calculate dx_to_prescription column (will use in next 06_plots and stats------------
merged_df$dx_to_prescription <- merged_df$earliest_prescription - merged_df$first_ra
merged_df$dx_to_prescription <- merged_df$dx_to_prescription + 1

# create column for follow_up from first_biologic (for subsequent survival analysis in 06) -------------------------------------------
merged_df <-
  merged_df %>%
  group_by(ReferenceKey) %>%
  slice(1) %>% 
  mutate(first_btsdmard_follow_up = last_record - PrescriptionStartDate) %>% 
  select(ReferenceKey, first_btsdmard_follow_up) %>% 
  full_join(merged_df)

# merged_df %>% filter(overall_follow_up != first_btsdmard_follow_up) %>% select(overall_follow_up, first_btsdmard_follow_up)

saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df2.rds"))
# merged_df <- readRDS(paste0(path, "/saved_rds/merged_df2.rds"))


# proceed to 06_plots and stats



