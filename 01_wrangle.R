# loading packages and data --------------------------------------------------------
if (!require("librarian")) install.packages("librarian")

librarian::shelf(haven,
                 tidyverse,
                 ggpubr,
                 mgsub, #for multiple substitutions
                 hrbrthemes)

# setwd("/Users/elsiechan/Documents/GitHub/rheumatoid")

setwd("/Users/elsiechan/Documents/GitHub/rheumatoid/kuan_folder")

death <- readRDS("Death.RDS")
diagnosis <- readRDS("Diagnosis.RDS")
inpatient <- readRDS("Inpatient.RDS")
prescription <- readRDS("Prescription.RDS")


