# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
                 ggpubr,
                 mgsub, #for multiple substitutions
                 hrbrthemes)

# setwd("/Users/elsiechan/Documents/GitHub/rheumatoid")

setwd("/Users/elsiechan/Documents/GitHub/rheumatoid")

# replace with your path to kuan's folder of rds
path <- "/Users/elsiechan/Desktop/kuan_folder/"

death <- readRDS(paste0(path, "Death.RDS"))
diagnosis <- readRDS("Diagnosis.RDS")
inpatient <- readRDS("Inpatient.RDS")
prescription <- readRDS("Prescription.RDS")

# update ensure working
