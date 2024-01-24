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
                 kableExtra,
                 networkD3, # sankey but failed in the end
                 htmlwidgets,
                 riverplot,
                 lubridate,
                 ggraph,
                 RSelenium, # save plotly
                 orca, # save plotly
                 reticulate # save plotly
)

# replace with your path to kuan's folder of rds
path <- "/Users/elsiechan/Documents/kuan_folder"
setwd(path)
death <- readRDS("Death.RDS")
diagnosis <- readRDS("Diagnosis.RDS")
inpatient <- readRDS("Inpatient.RDS")
prescription <- readRDS("Prescription.RDS")

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
    "714\\.[0128]",
  x = diagnosis$description,
  value = TRUE
))

# exclude this strange item called Screen-rheumatoid
# ra_vector <- grep(pattern = "^(?!.*Screen)", x = ra_vector, value = TRUE, perl = TRUE)

# select out the seronegative, glomerulonephritis
ra_vector <- grep(pattern = "glomerulonephritis|seronegative", x = ra_vector, perl = TRUE, value = TRUE, invert = TRUE)

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

print(paste0("Then, we removed patients who were diagnosed with conditions that indicate the use of biologics at ANY point in their life. Now, there are ", length(unique(unwanted_ra)), " patients who meet this condition. Example of their ID include the following:"))

print(paste0(unique(unwanted_ra)[1:5], collapse = ", "))

print(paste0(
  "So the number of patients who remain in the analysis is ",
  length(unique(diagnosis_sub$ReferenceKey)) - length(unique(unwanted_ra))
))

# filter to remove where first_ra is before 2009, because our prescription data is only from 2009 onwards
diagnosis_sub <- diagnosis_sub %>%
  filter(as.Date(first_ra) >= as.Date("2009-01-01"))

print(paste0(
  "Because our prescription data is from 2009 onwards, we removed patients diagnosed with RA before 2009, leaving ", 
  length(unique(diagnosis_sub$ReferenceKey))
))

# one potential problem is if pt was diagnosed e.g. with RA at 2022 December. A year from then would be 2023 Dec when we do not have the data yet, but our data would make it appear as tho the pt has stuck with that biologic. 

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

# check where NA what is it?
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
prescription_traj%>% pull(ReferenceKey) %>% unique() %>% length() # 11776



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

# unique(prescription_traj$DrugName_clean_os)
# unique(prescription_traj$DrugName_clean)

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

# question 2 treatment trajectory (would allow us to label pt) -----------------------------------------
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



# Sankey diagram ----------------------------------------------------------
# same as line 608 prescription_traj
# readRDS(paste0(path, "/saved_rds/prescription_traj.rds"))
btsdmard <- c(tnfi, cd28, cd20, il6, jaki)

# (OLD) filter out cdmard, infliximab, leaving only btsdmard
# because infliximab is one day IV
# btsdmard_no_inf <- setdiff(btsdmard, "INFLIXIMAB")

df <- prescription_traj %>%
  filter(DrugName_clean %in% btsdmard)

# prescription_traj %>% filter(DrugName_clean == "INFLIXIMAB") %>% mutate(duration = PrescriptionEndDate - PrescriptionStartDate) %>% group_by(duration) %>%
#   summarize(count = n())

# filter to exclude where infliximab AND duration less than 518400
df <- df  %>% mutate(duration = PrescriptionEndDate - PrescriptionStartDate) %>% filter(DrugName_clean != "INFLIXIMAB" | duration >= 518400)

df <- df %>% select(-duration)

unique(df$DrugName_clean)
unique(prescription_traj$DrugName_clean)


source("/Users/elsiechan/Documents/GitHub/rheumatoid/02_extract_traj.R")

unique(merged_df$DrugName_clean)
# saveRDS(object = merged_df, file = paste0(path, /saved_rds/merged_df_sankey.rds"))

sankey_df <- readRDS(paste0(path, "/saved_rds/merged_df_sankey.rds"))

# filter where duration < 14 days
sankey_df <- sankey_df %>% filter(duration >= 14)


# if followed by the same DrugName_clean, then just simply merge because Sankey, breaks are not included
# so only take the first row of each ReferenceKey or where consecutive are not the same
# this step is after filtering by duration in case you get two same drugs if you did it the other way round. Which means regimen <14 days do not constitute a change in treatment trajectory
sankey_df <- sankey_df %>%
  group_by(ReferenceKey) %>%
  filter(row_number() == 1 | lag(DrugName_clean) != DrugName_clean)


# extract only the first 3 levels (max) for each patient
# attempted 4 levels but too diversified
sankey_df <- sankey_df %>% group_by(ReferenceKey) %>%
  slice(1:3)

sankey_df <- sankey_df %>% ungroup() # get rid of referencekey grouping

# df <- sankey_df
# sankey_df <- df

# Split the data by ReferenceKey
split_df <- split(sankey_df %>% select(ReferenceKey, DrugName_clean), sankey_df$ReferenceKey)

label_and_pivot <- function(df, group, DrugName_clean, ReferenceKey) {
  df$group <- seq(nrow(df))
  
  df <- df %>%
    pivot_wider(names_from = group,
                values_from = DrugName_clean,
                names_prefix = "group_")
  
  df <- df %>% select(-ReferenceKey)
  return(df)
}

# sample for debugging
# df <- split_df[[8]]
# label_and_pivot(split_df[[8]])
# sankey_tabulated <- bind_rows(lapply(split_df[1:100], FUN = label_and_pivot))

# rows are bound together
sankey_tabulated <- bind_rows(lapply(split_df, FUN = label_and_pivot))



# add new column called count; then collapse the rows
sankey_tabulated <- sankey_tabulated %>%
  group_by(group_1, group_2, group_3) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  distinct(group_1, group_2, group_3, .keep_all = TRUE)

sankey_tabulated <- sankey_tabulated %>% arrange(desc(count))


write.csv(sankey_tabulated, file = paste0(path, "/sankey.csv"), row.names = FALSE)

# filtering to only include where counts > 5 can easily be done on excel

# for debugging, why more than one biologics
# View(sankey_tabulated)
# i <- 10441268
# i <- 1083834
# i <- 1115849
# i <- 11760263
# i <- 4516092
# merged_df %>% filter(ReferenceKey == i)
# prescription_traj %>% filter(ReferenceKey == i) %>% select(DrugName_clean, PrescriptionStartDate, PrescriptionEndDate) %>% distinct() %>% arrange(PrescriptionStartDate, PrescriptionEndDate)



# trajectory by moa, o_s, drug_name ---------------------------------------

prescription_traj <- readRDS(paste0(path, "/saved_rds/prescription_traj.rds"))
df <- prescription_traj


# if you DON"T uncomment any of the two lines below, you get merged_df_drugnamed.rds (so neither moa or os, altogether run 3 times)
# UNCOMMENT THIS LINE TO REMOVE CDMARD (not used for merging, just for the trajectory where cdmard is not important)
# df <- df %>% filter(DrugName_clean != "cdmard")

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



# so the drugnamed would be when I didn't run any of those two lines e.g. prescription_traj$DrugName_clean <- prescription_traj$moa

# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_nocdmard.rds"))

# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_drugnamed.rds"))
# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_moanamed.rds"))
# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df_drugnamed_os.rds"))


merged_df_drugnamed <- readRDS(paste0(path, "/saved_rds/merged_df_drugnamed.rds"))
moanamed_df <- readRDS(paste0(path, "/saved_rds/merged_df_moanamed.rds"))
merged_df_drugnamed_os <- readRDS(paste0(path, "/saved_rds/merged_df_drugnamed_os.rds"))

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

# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df.rds"))
merged_df <- readRDS(paste0(path, "/saved_rds/merged_df.rds"))




# calculate days before the use of b/tsDMARD (we don't worry about csDMARD most of the time)--------------------------------
# so these are either b or tsdmard, calculate number of days till the use of it
btsdmard <- c(tnfi, cd28, cd20, il6, jaki) # this line of code duplicate, before

# diagnosis_sub is from clean_diagnosis see earlier subtitle of this script
# join the table to obtain the day of first_ra diagnosis
merged_df <- left_join(merged_df,
                       unique(diagnosis_sub[, c("ReferenceKey", 
                                                "first_ra")]),
                       by = "ReferenceKey")


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


# debugging to make sure correct
# merged_df %>% filter(prescription_after_ra == FALSE) %>% pull(ReferenceKey) %>% unique()
# merged_df %>% filter(ReferenceKey == 1342880) %>% View()

# length(unique(prescription_traj$ReferenceKey))


print(paste0("Of the ", length(unique(merged_df$ReferenceKey)), 
             " patients, we would expect that the earliest date of diagnosis of rheumatoid arthritis must have occurred before the prescription of any rheumatoid arthritis drugs. This is indeed the case for ", 
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


# date till first btsdmard if any (NA otherwise)
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





# combine scores_df with merged_df ----------------------------------------
# if you used full_join, will have a lot of NA rows from scores_df
merged_df <- dplyr::left_join(merged_df, scores_df, by = c("ReferenceKey" = "Reference.Key."))

# (OLD) there is one random entry with drug prescription in 1900, but now we filtered by 2009, so it is ok to ignore
# merged_df <- merged_df %>% filter(earliest_start_date > 1950)



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

# Extract the year of diagnosis of the RA
merged_df$first_ra_year <- format(merged_df$first_ra, "%Y")

# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df2.rds"))
merged_df <- readRDS(paste0(path, "/saved_rds/merged_df2.rds"))


# 1a: How long patients can use b/ts DMARDs in Hong Kong, potentially stratified by years to illustrate the improved uptake rate (My personal guesswork is new patients such as diagnosis in 2019 should access to b/ts DMARDs faster than old patients diagnosis in 2009)----------------
# boxplot -----------------------------------------------------------------
df <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  distinct(ReferenceKey, .keep_all = TRUE) %>% 
  group_by(first_ra)

# descriptive for the manuscript
df %>%
  group_by(first_ra_year) %>%
  summarise(mean_days_to_btsdmard = mean(days_to_btsdmard, na.rm = TRUE),
            median_days_to_btsdmard = median(days_to_btsdmard, na.rm = TRUE))



my_plot <- ggplot(df, aes(x = as.factor(first_ra_year), y = days_to_btsdmard)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  xlab("year of diagnosis")


# Define the maximum width for each line of the title
max_title_width <- 40

# Wrap the title text into two lines
wrapped_title <- stringr::str_wrap(paste0("Days to b/tsdmard by year of diagnosis"), width = max_title_width)

# Set the base theme to theme_bw with Arial as the base family
theme_set(theme_bw(base_family = "Arial"))

# Plot the boxplot with the specified style elements
my_plot <- ggplot(df, aes(x = as.factor(first_ra_year), y = days_to_btsdmard)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  labs(title = wrapped_title, x = "Year of diagnosis", y = "Days to b/tsdmard") +
  scale_fill_manual(values = c("slateblue")) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(color = "#DDDDDD"),
    panel.grid.minor = element_line(color = "#EEEEEE", linetype = "dashed"),
    panel.background = element_rect(fill = "#F5F5F5"),
    axis.line = element_line(color = "#333333"),
    axis.text = element_text(color = "#333333", size = 11),
    axis.title = element_text(color = "#333333", size = 11),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Display the plot
print(my_plot)

ggsave(paste0(path, "/charts/", "box_days_to_btsdmard.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "box_days_to_btsdmard.png"), my_plot, device = "png", dpi = 300)

# for making the plots (no longer in publication, OLD)-------------
merged_df$years_to_btsdmard <- as.numeric(merged_df$days_to_btsdmard / 365.25)
merged_df$years_to_cdmard <- as.numeric(merged_df$days_to_cdmard / 365.25)


# problem is many times cdmard is NEGATIVE; so run the below line (or skip) if want to calculate days lapsed between first use of biologics AFTER first use of cDMARD; idea abandoned because when you did prescription_after_ra would already have solved this problem
# merged_df %>%
#   mutate(days_to_btsdmard = case_when(
#     !is.na(days_to_cdmard) ~ days_to_btsdmard + days_to_cdmard,
#     TRUE ~ days_to_btsdmard
#   ))


# summarize solves the issue where for the same first_ra value the value seems to pile up and give a erroneous num for days to btsdmard, perhaps because more than pt diagnosed on the same date
merged_df_years <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  distinct(ReferenceKey, .keep_all = TRUE) %>% 
  group_by(first_ra) %>%
  summarize(mean_years_to_btsdmard = mean(years_to_btsdmard),
            mean_years_to_cdmard = mean(years_to_cdmard))


# # Create a bar chart of days_to_btsdmard, stratified by year of diagnosis of the RA
# # tunable parameters include y = years_to_btsdmard, x = first_ra, first_ra_year

# change the variables for ease of plotting
# x_var <- "first_ra_year"
# x_label <- "Year of Diagnosis of RA"
x_var <- "first_ra"
x_label <- "Date of diagnosis of rheumatoid arthritis"

y_var <- "mean_years_to_cdmard"
y_label <- "Mean years to cDMARD"
y_var <- "mean_years_to_btsdmard"
y_label <- "Mean years to b/tsDMARD"


df <- merged_df_years %>% filter(!is.na(mean_years_to_cdmard))
df <- merged_df_years %>% filter(!is.na(mean_years_to_btsdmard))


# Define the maximum width for each line of the title
max_title_width <- 40

# Wrap the title text into two lines
wrapped_title <- stringr::str_wrap(paste0(y_label, " by ", tolower(x_label)), width = max_title_width)

theme_set(theme_bw(base_family = "Arial"))

# Plot the y_var by x_var with gridlines, modified colors, and minor gridlines
my_plot <- ggplot(
  data = df,
  aes(x = !!sym(x_var), y = !!sym(y_var))
) +
  # ylim(min(merged_df_days$days_to_btsdmard, na.rm = TRUE),
  #      max(merged_df_days$days_to_btsdmard, na.rm = TRUE)) +
  geom_col(fill = "#0072B2", color = "#333333") +
  labs(title = wrapped_title,
       x = x_label, y = y_label) +
  scale_fill_manual(values = c("#0072B2")) +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "#DDDDDD"),
        panel.grid.minor = element_line(color = "#EEEEEE", linetype = "dashed"),
        panel.background = element_rect(fill = "#F5F5F5"),
        axis.line = element_line(color = "#333333"),
        axis.text = element_text(color = "#333333", size = 11),
        axis.title = element_text(color = "#333333", size = 11),
        # plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "none")

my_plot

ggsave(paste0(path, "/charts/", "bar_", y_var, ".svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "bar_", y_var, ".png"), my_plot, device = "png", dpi = 300)


# to show if it is a few drugs that tilted the mean years to less, must show which is the btsdmard for introduced-------------------
# merged_df %>% 
#   filter(prescription_after_ra == TRUE) %>% 
#   distinct(ReferenceKey, .keep_all = TRUE) %>% 
#   group_by(first_ra) %>%
#   summarize(mean_years_to_btsdmard = mean(years_to_btsdmard),
#             mean_years_to_cdmard = mean(years_to_cdmard))

df <- merged_df %>%
  filter(prescription_after_ra == TRUE, drug_os != "cdmard") %>%
  group_by(ReferenceKey) %>%
  slice(1) %>%
  mutate(drug_os = case_when(
    str_detect(drug_os, "\\+cdmard") ~ str_replace(drug_os, "\\+cdmard", ""),
    str_detect(drug_os, "cdmard\\+") ~ str_replace(drug_os, "cdmard\\+", ""),
    TRUE ~ drug_os
  )) %>%
  ungroup() %>%
  select(ReferenceKey, drug_os, years_to_btsdmard, first_ra)


x_var <- "first_ra"
x_label <- "Date of diagnosis of rheumatoid arthritis"
y_var <- "years_to_btsdmard"
y_label <- "Years to b/tsDMARD"

# Define the maximum width for each line of the title
max_title_width <- 30

# Wrap the title text into two lines
wrapped_title <- stringr::str_wrap(paste0(y_label, " by ", tolower(x_label)), width = max_title_width)

theme_set(theme_bw(base_family = "Arial"))


df$drug_os <- factor(df$drug_os, levels = sort(unique(df$drug_os)))

df <- df %>%
  mutate(s_or_o = case_when(
    str_ends(drug_os, "_s") ~ "Biosimilars (adalimumab, infliximab, rituximab)",
    str_ends(drug_os, "_o") ~ "Biooriginators (adalimumab, infliximab, rituximab)",
    TRUE ~ "Other biooriginators"
  ))

my_plot <- ggplot(
  data = df,
  aes(x = first_ra, y = years_to_btsdmard, shape = s_or_o, colors = drug_os)
) +
  geom_point(size = 0.9, alpha = 0.7) +
  labs(title = wrapped_title,
       x = x_label, y = y_label, shape = "First biologic prescribed") +
  scale_shape_manual(values = c(17, 3, 16)) +
  guides(color = guide_legend(label.wrap = 5)) +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "#DDDDDD"),
        panel.grid.minor = element_line(color = "#EEEEEE", linetype = "dashed"),
        panel.background = element_rect(fill = "#F5F5F5"),
        axis.line = element_line(color = "#333333"),
        axis.text = element_text(color = "#333333", size = 9),
        axis.title = element_text(color = "#333333", size = 10),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 8),
        legend.margin = margin(t = 0.5, r = 0, b = 0, l = 0)) 


my_plot

ggsave(paste0(path, "/charts/", "scatter_by_category", y_var, ".svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "scatter_by_category_", y_var, ".png"), my_plot, device = "png", dpi = 300)




# 1b # (OLD) plot the scatter chart to find out any relationship between days to cdmard and days to bdmard----------------------------------------------------------------------

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



# 1c: (OLD) using years_to_cdmard and years_to_bts_dmard correcting for score_before_0 to predict dx_to_death------------

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
# obtain first btsdmard based on grep
first_btsdmard_df <- merged_df %>%
  # filter(ReferenceKey == 10024714 | ReferenceKey == 100096) %>% 
  group_by(ReferenceKey) %>%
  filter(grepl(pattern = paste(btsdmard, collapse = "|"), 
                    x = drug)) %>%
  slice_min(PrescriptionStartDate) %>%
  ungroup() 

# first_btsdmard_df %>% filter(ReferenceKey == 3447394) %>% View()

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
  filter(!is.na(first_btsdmard) & drug != "cdmard") %>% 
  mutate(first_prescription_year = year(PrescriptionStartDate)) %>% 
  group_by(ReferenceKey) %>% 
  slice(1) %>% # to obtain only the first of each ReferenceKey
  ungroup() %>% 
  group_by(first_prescription_year, first_btsdmard) %>%
  summarize(n = n()) %>%
  group_by(first_prescription_year) %>%
  mutate(prop = n / sum(n)) %>% 
  filter(first_prescription_year >= 2009)


# merged_df_proportions <- merged_df %>%
#   filter(!is.na(first_btsdmard)) %>% 
#   group_by(first_ra_year, first_btsdmard) %>%
#   summarize(n = n()) %>%
#   group_by(first_ra_year) %>%
#   mutate(prop = n / sum(n))

print(merged_df_proportions, n = 200)

merged_df_proportions$percentage <- merged_df_proportions$prop * 100

# merged_df_proportions %>% filter(first_btsdmard == "ADALIMUMAB")
merged_df_proportions <- merged_df_proportions %>% arrange(first_btsdmard)

print(merged_df_proportions, n = 200)
write.csv(merged_df_proportions,
          "stacked_barchart_btsdmard_proportion.csv",
          row.names = FALSE)

# Define the maximum width for each line of the title
max_title_width <- 30

# Wrap the title text into two lines
wrapped_title <- stringr::str_wrap("Proportion of first b/tsDMARD by drug and year", width = max_title_width)

my_plot <- ggplot(
  data = merged_df_proportions,
  aes(x = first_prescription_year, y = prop, color = first_btsdmard, group = first_btsdmard)
) +
  # geom_smooth(method = "loess", se = FALSE, span = 1) +
  geom_line() +
  labs(
    title = wrapped_title,
    x = "Year of prescription",
    y = "Proportion of first b/tsDMARD",
    color = "First b/tsDMARD Drug"
  ) +
  scale_color_discrete(name = "First b/tsDMARD", 
                       labels = function(x) stringr::str_to_title(x)) +
  scale_x_continuous(
    breaks = seq(min(merged_df_proportions$first_prescription_year), max(merged_df_proportions$first_prescription_year), by = 4)
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Add major gridlines
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted")  # Add minor gridlines
  )

my_plot

ggsave(paste0(path, "/charts/", "line_first_btsDMARD.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "line_first_btsDMARD.png"), my_plot, device = "png", dpi = 300)

# for geom_smooth instead
ggsave(paste0(path, "/charts/", "smooth_line_first_btsDMARD.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "smooth_line_first_btsDMARD.png"), my_plot, device = "png", dpi = 300)

# stacked barchart--------------------
# Define the maximum width for each line of the title
max_title_width <- 30

# Wrap the title text into two lines
wrapped_title <- stringr::str_wrap("Proportion of first b/tsDMARD by drug and year", width = max_title_width)

my_plot <- ggplot(
  data = merged_df_proportions,
  aes(x = "", y = prop, fill = first_btsdmard)
) +
  geom_bar(stat = "identity", width = 1) +
  labs(
    title = wrapped_title,
    x = "Year of prescription",
    y = "Proportion of first b/tsDMARD",
    fill = "First b/tsDMARD Drug"
  ) +
  scale_fill_discrete(
    name = "First b/tsDMARD Drug",
    labels = function(x) stringr::str_to_title(x)
  ) +
  facet_wrap(~first_prescription_year, nrow = 2) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  )


my_plot

ggsave(paste0(path, "/charts/", "stacked_first_btsDMARD.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "stacked_first_btsDMARD.png"), my_plot, device = "png", dpi = 300)

# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df3.rds"))
merged_df <- readRDS(paste0(path, "/saved_rds/merged_df3.rds"))

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

merged_df <- merged_df %>% 
  filter(str_detect(first_btsdmard, "ADALIMUMAB|RITUXIMAB|INFLIXIMAB")) %>% 
  filter(prescription_after_ra == TRUE)

# adding days to o and earliest o date
merged_df <- merged_df %>% 
  filter(str_detect(drug_os, "_o")) %>% 
  group_by(ReferenceKey) %>%
  mutate(earliest_o_date = min(PrescriptionStartDate)) %>% # can do because already filtered by those with _o
  mutate(days_to_o = earliest_o_date - first_ra) %>% # rather than first_ra unlike cdmard and bdmard
  distinct(ReferenceKey, earliest_o_date, days_to_o) %>%
  full_join(merged_df)

merged_df$days_to_o <- as.numeric(merged_df$days_to_o)

# adding days to s and earliest s date
merged_df <- merged_df %>% 
  filter(str_detect(drug_os, "_s")) %>% 
  group_by(ReferenceKey) %>% 
  mutate(earliest_s_date = min(PrescriptionStartDate)) %>% 
  mutate(days_to_s = earliest_s_date - first_ra) %>%
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


temp <- merged_df %>% 
  distinct(ReferenceKey, os_trajectory) %>% 
  group_by(os_trajectory) %>%
  pull(os_trajectory) %>% 
  table()


paste0("Based on the earliest date of use of bio-originator or bio-similar, we classified patients into bio-originator only (", temp["o"], "), biosimilar only (", temp["s"], "), switch from bio-originator to biosimilar (", temp["o_s"], "), and switch from biosimilar to bio-originator (", temp["s_o"], "). There were no patients who commenced both simultaneously.")

# it makes sense the above don't add up to the !is.na(days_to_btsdmard) because that is btsdmard; whereas we are just looking at the bdmard with bios and bioo 
# merged_df %>% 
#   distinct(ReferenceKey, days_to_btsdmard) %>% pull(days_to_btsdmard) %>% is.na() %>% table()



# attempt to visualise days to bio_s vs bio_o -----------------------------
# those first_btsdmard == "ADALIMUMAB", "RITUXIMAB", "INFLIXIMAB"
# if days_to_o < days_to_s, then bioo group; if days_to_s < days_to_o, then bios group
# filter to get one row per ReferenceKey
# generate boxplot such that y-axis is days to first b/tsDMARD, x-axis would be Drug (those three, and subgroup by bioo or bios)
# boxplot three columns, two for each
# Filter merged_df based on drug names
# Filter merged_df based on drug names


filtered_df <- merged_df %>%
  group_by(ReferenceKey) %>%
  slice(1) %>%
  ungroup 

# merged_df %>% filter(ReferenceKey == 1034367) %>% View()


filtered_df <- filtered_df %>%
  mutate(group = case_when(
    os_trajectory %in% c("o_s", "o") ~ "o",
    os_trajectory %in% c("s", "s_o") ~ "s",
    TRUE ~ NA_character_
  ))

# Aggregate the data to get one row per ReferenceKey
agg_df <- filtered_df %>%
  group_by(ReferenceKey, first_btsdmard, group) %>%
  summarise(days_to_btsdmard = median(days_to_btsdmard))


# Perform t-test between 'o' and 's' within each drug category
t_test_results <- agg_df %>%
  group_by(first_btsdmard) %>%
  filter(group %in% c("o", "s")) %>%
  summarise(t_test_pvalue = t.test(days_to_btsdmard ~ group)$p.value)

t_test_results

p <- ggplot(agg_df, aes(x = first_btsdmard, y = days_to_btsdmard, fill = group)) +
  geom_boxplot(outlier.shape = NA) +  # Remove the dots representing outliers
  labs(x = "Drug", y = "Days to First b/tsDMARD", title = "Days to first b/tsDMARD stratified by biosimilar or bio-originator") + scale_fill_manual(
  values = c("o" = brewer.pal(3, "Set1")[1], "s" = brewer.pal(3, "Set1")[2]),
  labels = c("o" = "Bio-originator", "s" = "Biosimilar")
)

p + stat_compare_means(label = "p.format", 
                                  paired = TRUE,
                                  hide.ns = FALSE,
                                  inherit.aes = TRUE,
                       aes(label = after_stat(p.signif)))

stat_compare_means(method = "anova", label.y = 40)+ # Add global p-value
  stat_compare_means(aes(label = after_stat(p.signif)),
                     method = "t.test", ref.group = "0.5")

ggsave(paste0(path, "/charts/", "box_days_to_btsDMARD.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "box_days_to_btsDMARD.png"), my_plot, device = "png", dpi = 300)

# visualise whether greater proportion of patients received b/tsDMARD in the bio_s group



# saveRDS(object = merged_df, file = paste0(path, "/saved_rds/merged_df4.rds"))
merged_df <- readRDS(paste0(path, "/saved_rds/merged_df4.rds"))


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


# strings <- c("cdmard+a", "cdmard", "a+cdmard", "a+cdmard+b", "g+cdmard+d", "remaining")
# 
# # Remove one "+" sign when appearing before or after "cdmard"
# gsub("\\+?(cdmard)", "", strings)



# downsample the data for speed and clarity
# create groups based on prescription start dates and duration
merged_df_downsampled <- merged_df %>%
  # for downsampling
  # distinct(ReferenceKey) %>%
  # sample_n(size = 300) %>%
  # inner_join(merged_df, by = "ReferenceKey") %>%
  # for replacing cdmard
  mutate(moa = gsub("\\+?(cdmard)", "", moa)) %>%  # remove +cdmard; but some instances have cdmard+b → +b
  mutate(moa = gsub("^\\+", "", moa)) %>%  # so this step just remove the + at the start
  filter(moa != "") %>% 
  # )) %>% # these lines to remove cdmard in case it is not so interesting, we only have biologics instead
  group_by(ReferenceKey) %>% # group_by referencekey before cutting PrescriptionStartDate)
  mutate(start_date_group = 
           cut(earliest_start_date, 
               breaks = seq.Date(as.Date(min(merged_df$PrescriptionStartDate)), 
                                 as.Date(max(merged_df$PrescriptionEndDate)), 
                                 by = "6 months"))) %>% 
  mutate(duration_moa_group = cut(duration_moa, breaks = c(0, 365, 730, Inf))) %>%
  arrange(start_date_group, duration_moa_group, moa) # n_moa previously



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
         moa = moa)


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
# single_colors <- c("cdmard" = "#6d8db8",
#                    "cd28" = "#8fbf8b",
#                    "jaki" = "#db7e84",
#                    "il6" = "#d68a53",
#                    "tnfi" = "#7e7a1c",
#                    "cd20" = "#b8b87d")
# 
# # Define darker colors for drug combinations
# combo_colors <- c("cdmard+jaki" = "#3c1a61",
#                   "cdmard+tnfi" = "#5a1c03",
#                   "cd28+cdmard" = "#145b8d",
#                   "cdmard+il6" = "#525252",
#                   "cdmard+il6+jaki" = "#1a5b1a",
#                   "cd28+cdmard+il6" = "#5e4187",
#                   "cdmard+jaki+tnfi" = "#3c1403",
#                   "cd20+cdmard" = "#8b7a3d",
#                   "cd28+il6" = "#5b3f35",
#                   "cd28+jaki" = "#9c5e88",
#                   "cdmard+il6+tnfi" = "#912525",
#                   "il6+tnfi" = "#7e7a1c",
#                   "jaki+tnfi" = "#9c5e88",
#                   "cd28+cdmard+tnfi" = "#a64c00",
#                   "cd20+cdmard+jaki" = "#8c7c68",
#                   "cd20+cdmard+tnfi" = "#a64c00",
#                   "cd20+jaki" = "#9c5e88",
#                   "cd28+tnfi" = "#a64c00",
#                   "il6+jaki" = "#9c5e88",
#                   "cd28+cdmard+jaki" = "#5e4187",
#                   "cd20+tnfi" = "#a64c00",
#                   "cd20+cdmard+il6" = "#8c7c68",
#                   "cd20+cd28+cdmard" = "#5e4187")

# ALTERNATIVELY no cDMARD combination
single_colors <- c("cd28" = "#8fbf8b",
                   "jaki" = "#db7e84",
                   "il6" = "#d68a53",
                   "tnfi" = "#7e7a1c",
                   "cd20" = "#b8b87d")

combo_colors <- c("cd28+il6" = "#5b3f35",
  "cd28+jaki" = "#9c5e88",
  "il6+tnfi" = "#7e7a1c",
  "jaki+tnfi" = "#9c5e88",
  "cd20+jaki" = "#9c5e88",
  "cd28+tnfi" = "#a64c00",
  "il6+jaki" = "#9c5e88",
  "cd20+tnfi" = "#a64c00"
)


# Combine the two palettes
drug_colors <- c(single_colors, combo_colors)

# Create a new column in the dataframe that assigns the correct color to each drug
df$color <- drug_colors[df$moa]

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
               name = ~moa, 
               hoverinfo = "text",
               text = ~paste("Drug:", moa,
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


# saveRDS(object = fig, file = paste0(path, "/saved_rds/gantt_fig.rds"))
fig <- readRDS(paste0(path, "/saved_rds/gantt_fig.rds"))


paste0(path, "gantt_without_cdmard")

reticulate::use_python("/Users/elsiechan/miniconda3/bin/python")
reticulate::py_available()


fig %>% kaleido(file = paste0(path, "gantt_without_cdmard.png"),
               selenium = RSelenium::rsDriver(browser = "chrome"))

plotly_IMAGE(fig, width = 800, height = 600, format = "png", out_file = )

# png
orca(fig, file = paste0(path, "gantt_without_cdmard.png"), width = 1200, height = 800)

# Save the plot as a high-resolution SVG file
orca(fig, file = paste0(path, "gantt_without_cdmard.svg"), width = 1200, height = 800)

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

