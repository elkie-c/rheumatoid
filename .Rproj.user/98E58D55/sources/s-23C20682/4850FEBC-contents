# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
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
biosimilar_df <- read_excel(excel_file, sheet = 1)
bioo_df <- read_excel(excel_file, sheet = 2)
# cdmard_df <- read_excel(excel_file, sheet = 3, col_names = FALSE, trim_ws = TRUE)
# cdmard <- toString(cdmard_df)

bioo <- bioo_df$Agent
# manually extracted because the formating of the sheet is a little crude
cdmard <- c("Sulfasalazine", "Mycophenolate", "Methotrexate", "Leflunomide", "Hydroxychloroquine", "Cyclophosphamide", "Ciclosporin", "Azathioprine", "Apremilast")



# clean diagnosis ------------------------------------------------------
# firstly, only consider pt if diagnosed with RA at least at some point in life

# the following line shows that other than rheumatoid arthritis, you also have rheumatoid nodule arthritis
ra_vector <- unique(grep(
  pattern =
    "rheumatoid|Rheumatoid|[Jj]uv.*rh",
  x = diagnosis$All.Diagnosis.Description..HAMDCT..,
  value = TRUE
))

# there are some strange diagnosis with rheum, and for the benefit of doubt (and discussion), they are as follows: now the decision is whether you would like to keep these? Could modify the expression above to do that
# Find the strings that contain "rheum" but not in in ra_vector (which we will use for analysis)
not_ra_vector <- unique(grep(
  pattern =
    "rheum",
  x = diagnosis$All.Diagnosis.Description..HAMDCT..,
  value = TRUE
))

not_ra_vector[!not_ra_vector %in% ra_vector] # so these are the strings

# Filter the diagnosis table to only get pt who has, in their lifetime, been diagnosed with any in ra_vec
ra_vector_refkey <- diagnosis[diagnosis$All.Diagnosis.Description..HAMDCT.. %in% ra_vector, "Reference.Key."]
diagnosis_sub <- diagnosis[diagnosis$Reference.Key. %in% ra_vector_refkey, ]

# for pt diagnosed with cancer or autoimmune disease before RA, remove them
diagnosis


colnames(prescription)
colnames(inpatient)
colnames(diagnosis)
# update ensure working
View(death)
View(diagnosis)
View(inpatient)
View(prescription)

