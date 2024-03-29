# getting rid of redundant packages-------------------
# funchir::stale_package_check("/Users/elsiechan/Documents/GitHub/rheumatoid/01_wrangle.R")


# run at the start --------------------------------------------------------
merged_df <- readRDS(paste0(path, "/saved_rds/merged_df2.rds"))
btsdmard_class <- c("jaki", "tnfi", "cd20", "cd28", "il6")

print(colnames(merged_df))


# demonstrating shortened time before switching of drug (not in manuscript) -----------------
# Use a lagged analysis: Instead of considering the change in drug regimen immediately after the diagnosis year, introduce a lag period to allow sufficient time for patients to potentially change their drug regimen. This helps account for the delay between diagnosis and treatment changes. For example, you can calculate the percentage of patients who changed their drug regimen within the first six months or one year after the diagnosis year.

# did not filter to get only those rows which have changed because then the percentage would be meaningless

# first filter to get only those with follow up of at least a year AFTER earliest prescription so recalculating rather than getting the follow_up
# get the number of rows by year
total_fu <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% 
  filter(last_record - earliest_prescription > 365) %>% 
  group_by(ReferenceKey) %>%
  slice(1) %>% 
  group_by(first_ra_year) %>% 
  summarize(total_count = n())


# DON"T use first btsdmard FU which refers to follow up from the first-btsdmard to the last record. Instead use the duration_os
# you don't use filter to only choose among the ones that have changed. Because that would be bias as older pt would more likely have changed
# so you use last_record - earliest_prescription at least n days of FU, then calculate the number of which the duration of the first btsdmard is shorter than n days, so you get the count that had the switch

#' function to obtain switch before days
#'
#' @param merged_df 
#' @param threshold threshold referring to the number of days as a cut-off for switch
#' @param min_fu minimum FU of pt, set, arbitrary 1 year if want more recent records; if too high then 2022 records meaningless
#'
#' @return
#' @export
#'
#' @examples
switch_before_days <- function(merged_df, 
                               threshold,
                               min_fu = 365) {
  output <- merged_df %>% 
    filter(prescription_after_ra == TRUE) %>% # if it occurred before the RA then the calculations don't make sense
    filter(last_record - earliest_prescription > min_fu) %>%
    group_by(ReferenceKey) %>% 
    slice(1) %>% # for the first record as you don't look at subsequent biologics
    filter(duration_os < threshold) %>%  # less than a certain duration, get the number of that
    group_by(first_ra_year) %>% 
    summarize(count = n())
  
  return(output)
}

# more elegant solution, do the right_join, mutation, then only hv to change threshold num twice
total_fu <- switch_before_days(merged_df, threshold = 31) %>% 
  right_join(total_fu, by = "first_ra_year") %>% 
  mutate(percent_31 = count/total_count * 100) %>% 
  select(-count)

# total_fu <- switch_before_days(merged_df, threshold = 61) %>% 
#   right_join(total_fu, by = "first_ra_year") %>% 
#   mutate(percent_61 = count/total_count * 100) %>% 
#   select(-count)

total_fu <- switch_before_days(merged_df, threshold = 91) %>% 
  right_join(total_fu, by = "first_ra_year") %>% 
  mutate(percent_91 = count/total_count * 100) %>% 
  select(-count)

total_fu <- switch_before_days(merged_df, threshold = 183) %>% 
  right_join(total_fu, by = "first_ra_year") %>% 
  mutate(percent_183 = count/total_count * 100) %>% 
  select(-count)

total_fu <- switch_before_days(merged_df, threshold = 365) %>% 
  right_join(total_fu, by = "first_ra_year") %>% 
  mutate(percent_365 = count/total_count * 100) %>% 
  select(-count)


total_fu

# among those who had a switch within 1 year what was the quartiles and median?
merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% # if it occurred before the RA then the calculations don't make sense
  filter(last_record - earliest_prescription > 365) %>%
  group_by(ReferenceKey) %>% 
  filter(duration_os < 365) %>%  # less than a certain duration, get the number of that
  filter(n() >= 2) %>% # those who actually had a switch
  slice(1) %>% # for the first record as you don't look at subsequent biologics
  group_by(first_ra_year) %>% 
  summarize(q1 = quantile(duration_os, 0.25),
            median = quantile(duration_os, 0.50),
            q3 = quantile(duration_os, 0.75))


# for density plots for the days after prescription, but not used  --------
output <- merged_df %>% 
  filter(prescription_after_ra == TRUE) %>% # if it occurred before the RA then the calculations don't make sense
  filter(last_record - earliest_prescription > min_fu) %>%
  group_by(ReferenceKey) %>% 
  slice(1) %>% # for the first record as you don't look at subsequent biologics
  group_by(first_ra_year, duration_os) %>% 
  summarize(count = n())

ggplot(output %>% filter(duration_os <= 365), 
       aes(x = duration_os)) +
  geom_density() +
  facet_grid(. ~ first_ra_year) +
  labs(x = "Duration (days)", y = "Count") +
  ggtitle("Density Plot with Facet Grid")


# create df_dup -----------------------------------------------------------
# df_dup is after separate then expand where more than one drug used in same period, now just duplicate the rows
# because need df_dup for the btsdmard use each year, and also the retention rates graph
df_dup <- merged_df %>% mutate(drug_os = strsplit(drug_os, "\\+")) %>%
  unnest(drug_os)


# check by number of increased row
# these number of referencekey
unique(merged_df$ReferenceKey) %>% length() 
# df_dup expect more if have +
df_dup %>% distinct(ReferenceKey, drug_os) %>% nrow()


# check by specific pt
# i <- 28
# i <- i + 1
# merged_df %>% filter(ReferenceKey == merged_df$ReferenceKey[i]) %>% select(ReferenceKey, drug_os)
# df_dup %>% filter(ReferenceKey == merged_df$ReferenceKey[i]) %>% select(ReferenceKey, drug_os)

# df_dup %>% distinct(drug_os)
# confirms that the extra number of rows match the number of rows with "+" sign
merged_df %>% filter(str_detect(drug_os, "\\+"))


# drugs vector --------------------------------------------------------------
drugs <- df_dup %>% pull(drug_os) %>% unique() # used for drug retention, or others, basically get all possible drugs

# there's a "drugs" variable also in 01 script

# days from dx to prescription of first_ra drug; normally should be positive as prescription comes after dx (not used in manuscript) ------------------------------------------------------------------
# categorising those NEGATIVE (unwanted data)
# if negative, it means RA ICD diagnosis came after the first prescription of RA-related drug
# num_vec <- merged_df %>% filter(dx_to_prescription < 0) %>% distinct(ReferenceKey, dx_to_prescription) %>% pull(dx_to_prescription)
# 
# range_vec <- cut(as.numeric(num_vec), breaks = c(-Inf, -365, -100, -31, -14, -7, -1), 
#                  labels = c("-365 to -101", "-100 to -31", "-30 to -15", "-14 to -8", "-7 to -2", "-1"))
# 
# # Create a frequency table of the counts within each category
# table(range_vec)

# only pull out positive values in order to exclude those with prescription given before earliest_prescription
# calculate median and quartiles for each year
df <- merged_df %>%
  filter(prescription_after_ra == TRUE) %>% 
  distinct(ReferenceKey, .keep_all = TRUE) %>% 
  group_by(first_ra_year) 

# filtered away 2022 because that year as only one median data point that is not negative for dx_to_prescription
df <- df %>% filter(first_ra_year != 2022)

# why remove 2022: only one positive data point
# merged_df %>% filter(first_ra_year == 2022) %>% View()


df_stats <- df %>% summarize(lower = quantile(dx_to_prescription, 0.25),
            median = quantile(dx_to_prescription, 0.5),
            upper = quantile(dx_to_prescription, 0.75))

# for the median count
df_stats
 
# boxplot for dx_to_prescription----------  (not used in manuscript) -----------------------------------------------------------------
my_plot <- ggplot(df, aes(x = as.factor(first_ra_year), y = dx_to_prescription)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  xlab("year of diagnosis")

# Define the maximum width for each line of the title
max_title_width <- 40

# Wrap the title text into two lines
wrapped_title <- stringr::str_wrap(paste0("Days to b/tsdmard by year of diagnosis"), width = max_title_width)

# Set the base theme to theme_bw with Arial as the base family
theme_set(theme_bw(base_family = "Arial"))

# Plot the boxplot with the specified style elements
my_plot <- ggplot(df, aes(x = as.factor(first_ra_year), y = dx_to_prescription)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  labs(title = wrapped_title, x = "Year of diagnosis", y = "Days to b/tsdmard") +
  scale_fill_manual(values = c("slateblue")) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(color = "#DDDDDD"),
    panel.grid.minor = element_line(color = "#EEEEEE", linetype = "dashed"),
    panel.background = element_rect(fill = "#F5F5F5"),
    axis.line = element_line(color = "#333333"),
    axis.text = element_text(color = "#333333", size = 9),
    axis.title = element_text(color = "#333333", size = 9),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Display the plot
print(my_plot)

ggsave(paste0(path, "/charts/", "box_days_to_btsdmard.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "box_days_to_btsdmard.png"), my_plot, device = "png", dpi = 500)


# for decision making only whether to use dispensing or prescriptionstartdate
# normally perhaps using dispensingdate is good; but here, some dispensing startdate is after prescriptionstartdate which does not make sense. So I decide to use prescriptionstartdate. And also the data would not be different unless the spanned across years
# prescription_traj %>% filter(PrescriptionStartDate != `DispensingDate(yyyy-mm-dd)`)
# prescription_traj %>% mutate(
#   dp_diff = difftime(
#     prescription_traj$PrescriptionStartDate,
#     prescription_traj$`DispensingDate(yyyy-mm-dd)`,
#     units = "days"
#   )
# ) %>% mutate(dp_diff = abs(dp_diff)) %>% 
#   filter(dp_diff > 3) %>% View()

# stacked bar chart number of patients treated with btsDMARD by drug ------------------------
df <- df_dup

# this step sort of undo the merging from earlier but just do it here to get the year


# i is each row; so we can have duplicates if drug switch is A → B → A so remove dup later
ref_drug_year_df <- data.frame(
  ReferenceKey = character(),
  drug_os = character(),
  Year = integer(),
  stringsAsFactors = FALSE
)

# 1:nrow(df[1:10, ] for debug
# this loop just iteratively creates df to list out the years for the ReferenceKey and drug_os
for (i in 1:nrow(df)) {
  j <- df[i, ]
  years <- seq(year(j$PrescriptionStartDate), year(j$PrescriptionEndDate))
  
  df_sub <- data.frame(ReferenceKey = j$ReferenceKey, 
                     drug_os = j$drug_os, 
                     Year = years)
  
  ref_drug_year_df <- rbind(ref_drug_year_df, df_sub)
}

# remove duplicate rows for the A → B → A situation earlier
ref_drug_year_df <- unique(ref_drug_year_df)

df_counts <- ref_drug_year_df %>% 
  distinct(ReferenceKey, drug_os, Year) %>% # so only one entry for patient in that year for each drug taken
  group_by(Year, drug_os) %>% # don't group by referencekey
  summarise(Count = n())

print(df_counts, n = 32)

# up to 2022
df_counts <- df_counts %>% filter(Year < 2023 & Year > 2009)

# order for stacking of the chart
desired_order <- c(
  "ADALIMUMAB_s",
  "INFLIXIMAB_s",
  "RITUXIMAB_s",
  "ADALIMUMAB_o",
  "INFLIXIMAB_o",
  "RITUXIMAB_o",
  "ABATACEPT",
  "CERTOLIZUMAB",
  "ETANERCEPT",
  "GOLIMUMAB",
  "SARILUMAB",
  "TOCILIZUMAB",
  "BARICITINIB",
  "TOFACITINIB",
  "UPADACITINIB"
)

# Change order of rows
index <- order(factor(df_counts$drug_os, levels = desired_order))
df_counts <- df_counts[index, ]

# turn into a factor
df_counts$drug_os <- as.factor(df_counts$drug_os)

my_plot <- df_counts %>%
  ggplot(aes(x = Year, 
             y = Count, 
             fill = factor(drug_os, levels = desired_order))) + # gets it into desired order
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  xlab("Year") +
  ylab("Number of Patients Treated with b/tsDMARD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "B/ts DMARD")) +
  scale_x_continuous(breaks = unique(df_counts$Year), labels = unique(df_counts$Year))
  # scale_fill_viridis(discrete = TRUE)  # for colour

print(my_plot)

ggsave(paste0(path, "/charts/", "stacked_bar_btsdmard_number_treated.svg"), my_plot, device = "svg")
ggsave(paste0(path, "/charts/", "stacked_bar_btsdmard_number_treated.png"), my_plot, device = "png", dpi = 500)


# statistics based on market shares
df_counts

# generating the table for sankey -------------------------------------------

# note we are using merged_df rather than df_dup, so might have more than 1 biologic

# if followed by the same drug_os, then just simply merge because Sankey, breaks are not included
# so only take the first row of each ReferenceKey or where consecutive are not the same
# this step is after filtering by duration in case you get two same drugs if you did it the other way round. Which means regimen <threshold days set in 02_functions do not constitute a change in treatment trajectory
sankey_df <- merged_df %>%
  group_by(ReferenceKey) %>%
  filter(row_number() == 1 | lag(drug_os) != drug_os)

# minimum follow_up of one year
# the follow_up column is for OVERALL follow_up hence use first_btsdmard_follow_up
sankey_df <- sankey_df %>% filter(first_btsdmard_follow_up >= 365)

# if you do not want intra-class switch, then run this to prevent it
# sankey_df <- sankey_df %>%
#   group_by(ReferenceKey) %>%
#   filter(row_number() == 1 | lag(moa) != moa)


# extract only the first 3 levels (max) for each patient
# attempted 4 levels but too diversified
sankey_df <- sankey_df %>% group_by(ReferenceKey) %>%
  slice(1:3) %>% 
  ungroup()

# Split the data by ReferenceKey
split_df <- split(sankey_df %>% select(ReferenceKey, moa), sankey_df$ReferenceKey)

# function just to pivot to get first 3 columns of sankey
label_and_pivot <- function(df, variable) {
  df$group <- seq(nrow(df))
  
  df <- df %>%
    pivot_wider(names_from = group,
                values_from = {{ variable }},
                names_prefix = "group_")

  return(df)
}

# intra-class switch eg
# merged_df %>% filter(ReferenceKey == 10094530) %>% View()
# label_and_pivot(split_df[[8]], variable = "moa")

# example to show where combination drugs during transitory period
# merged_df %>% filter(ReferenceKey == 4238499) %>% View()


# rows are bound together, use moa
sankey_tabulated <- bind_rows(lapply(split_df, 
                                     "moa", 
                                     FUN = label_and_pivot))

saveRDS(object = sankey_tabulated, file = paste0(path, "/saved_rds/sankey_3_moa.rds"))
# write.csv(sankey_tabulated, file = paste0(path, "/sankey.csv"), row.names = FALSE)

# collapsing sankey df (OPTIONAL depending on plot method) --------------------------------------------
# sankey_tabulated <- readRDS(paste0(path, "/saved_rds/sankey_3_moa.rds"))

# add new column called count; then collapse the rows
sankey_tabulated <- sankey_tabulated %>%
  select(-ReferenceKey) %>% 
  group_by(group_1, group_2, group_3) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  distinct(group_1, group_2, group_3, .keep_all = TRUE)

sankey_tabulated <- sankey_tabulated %>% arrange(desc(count))


# sankey statistics for intra, inter-class switch -------------------------
sankey_tabulated

# among tnfi
unique(sankey_tabulated$group_1)

#' switch_percent
#' assign the value for group_1, and group_2. Then calculate the switch. Hence if group_1 is same as group_2 that is the intra-class switch. 
#' @param sankey_tabulated 
#' @param group_1 
#' @param group_2 
#'
#' @return switch_percent
#' @export
#'
#' @examples
# switch_percent(sankey_tabulated, 
# group1 = "tnfi", 
# group2 = "jaki")

switch_percent(sankey_tabulated, group1 = i, group2 = j)
switch_percent <- function(sankey_tabulated, group1, group2) {
  group1_sum <- sankey_tabulated %>% 
    filter(group_1 == group1 & !is.na(group_2)) %>%  # is not NA so you exclude those stuck with just one drug, also did not impose additional condition with + symbol since just transition
    summarize(sum(count)) %>% 
    pull()
  
  group1to2_sum <- sankey_tabulated %>% 
    filter(group_1 == group1 & group_2 == group2) %>% 
    summarize(sum(count)) %>% 
    pull()
  
  switch <- group1to2_sum/group1_sum * 100
  
  return(switch)
}

class_switch_df <- data.frame(
  group_1 = character(),
  group_2 = character(),
  switch = numeric(),
  stringsAsFactors = FALSE
)

# so loop all the possible values for group 1 and group 2
for (i in unique(sankey_tabulated$group_1)) {
  for (j in unique(sankey_tabulated$group_1)) {
    percent <- switch_percent(sankey_tabulated, i, j)
    
    # Append a new row to the dataframe
    class_switch_df <- rbind(class_switch_df, 
                             data.frame(group_1 = i, 
                                        group_2 = j, 
                                        switch = percent))
  }
}

class_switch_df %>% arrange(desc(switch))

merged_df %>% View()

# sankey plot (requires sankey_tabulated) -------------------------------------------------------------
# consider applying count >= 5 to avoid too messy diagram
ggplot(data = sankey_tabulated %>% filter(count >= 5),
       aes(axis1 = group_1,   # First variable on the X-axis
           axis2 = group_2,
           axis3 = group_3,
           y = count)) +
  geom_alluvium(aes(fill = count,
                    show.legend = FALSE,
                    curve_type = "sigmoid")) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_continuous(breaks = 1:3, labels = c("First regimen", "Second regimen", "Third regimen")) +
  # coord_flip() +
  ggtitle("Sankey of bts/DMARD in bio-naive Patients") +
  theme_void() 
  # scale_fill_viridis_d() + # color


# drug retention rate table first btsdmard  (2 years) -----------------------------------------------
# for each drug so has to be based on df_dup; same patient can have 2 trajectories?
# for debug
# i <- drugs[7]

follow_up <- "two_years"

output <- data.frame()

for (i in drugs) {
  # only consider first biologic taken by patient for this table
  first_row <- df_dup %>% group_by(ReferenceKey) %>% slice(1)
  
  # temp then identifies which pt's first drug is i
  temp <- first_row %>% filter(drug_os == i)
  
  # pull referencekey and only look at the drug_os == i of these entries
  # but also remove the other entries of this matched referencekey
  single_drug_rows <- df_dup %>%
    filter(ReferenceKey %in% temp$ReferenceKey &
             drug_os == i)
  
  # two_variables used in source: single_drug_rows and follow_up
  source("/Users/elsiechan/Documents/GitHub/rheumatoid/07_drug_retention_sum.R")
  
  # not memory efficient but just one row anyways and so few drugs
  output <- rbind(output, output_row)
}

colnames(output) <- c("drug_os", "0_month", "6_month", "12_month", "18_month", "24_month")

saveRDS(object = output, file = paste0(path, "/saved_rds/two_year_first_bts.rds"))

# drug retention rate table first btsdmard (1 year) -----------------------------------------------
follow_up <- "one_year"

output <- data.frame()

for (i in drugs) {
  # only consider first biologic taken by patient for this table
  first_row <- df_dup %>% group_by(ReferenceKey) %>% slice(1)

  temp <- first_row %>% filter(drug_os == i)
  
  single_drug_rows <- df_dup %>%
    filter(ReferenceKey %in% temp$ReferenceKey &
             drug_os == i)
  
  source("/Users/elsiechan/Documents/GitHub/rheumatoid/07_drug_retention_sum.R")
  
  output <- rbind(output, output_row)
}

colnames(output) <- c("drug_os", "0_month", "3_month", "6_month", "9_month", "12_month")

saveRDS(object = output, file = paste0(path, "/saved_rds/one_year_first_bts.rds"))

# drug retention rate table in NON-bionaive  (2 years) -----------------------------------------------
# note you might have more data than 1 year. Theoretically a patient can have multiple data points if they started adalimumab and dropped it off, then started another biologic and dropped it off etc
follow_up <- "two_years"

# debug e.g. for slice(-1)
# df_dup %>% filter(ReferenceKey == 10074503)

output <- data.frame()

for (i in drugs) {
  # slice -1 to remove the first drug
  non_first_rows <- df_dup %>% 
    group_by(ReferenceKey) %>% 
    slice(-1)
  
  # identify the first row in the non_first_rows that match the drug; do not worry about combination regimen because this is a df_dup, so no plus
  temp <- non_first_rows %>% filter(drug_os == i)
  
  # unlike previous bio-naive pt, where we sliced out one row. This time we filtered away the first row, so no need to redo the step of matching ReferenceKey
  single_drug_rows <- temp
  
  # two_variables used in source: single_drug_rows and follow_up
  source("/Users/elsiechan/Documents/GitHub/rheumatoid/07_drug_retention_sum.R")
  
  output <- rbind(output, output_row)
}

colnames(output) <- c("drug_os", "0_month", "6_month", "12_month", "18_month", "24_month")

saveRDS(object = output, file = paste0(path, "/saved_rds/two_year_nonfirst_bts.rds"))

# drug retention rate table in NON-bionaive  (1 year) -----------------------------------------------
follow_up <- "one_year"

# reset output
output <- data.frame()

for (i in drugs) {
  
  non_first_rows <- df_dup %>% 
    group_by(ReferenceKey) %>% 
    slice(-1)
  
  temp <- non_first_rows %>% filter(drug_os == i)
  
  single_drug_rows <- temp
  
  source("/Users/elsiechan/Documents/GitHub/rheumatoid/07_drug_retention_sum.R")
  output <- rbind(output, output_row)
}

colnames(output) <- c("drug_os", "0_month", "6_month", "12_month", "18_month", "24_month")

saveRDS(object = output, file = paste0(path, "/saved_rds/one_year_nonfirst_bts.rds"))



# function for two-year retention rate by class  (not used in manuscript) ---------------------------
# two-year retention, jaki
jaki = c("BARICITINIB", "TOFACITINIB", "UPADACITINIB")
tnfi = c(
  "INFLIXIMAB",
  "INFLIXIMAB_s",
  "ADALIMUMAB",
  "ADALIMUMAB_s",
  "CERTOLIZUMAB",
  "ETANERCEPT",
  "GOLIMUMAB"
)

# just function to calculate the two_year rr for convenience
two_year_rr <- function(output, moa) {
  if (moa == "others") {
    # by exclusion, includes rituximab, rituximab_s, cd28
    two_year_retention <- output %>% 
      filter(!(drug_os %in% jaki) & !(drug_os %in% tnfi)) %>%  # calls jaki and tnfi from outside the function so is not actually a good function, just convenience
      mutate(two_year_retention = sum(`24_month`) / sum(`0_month`) * 100) %>% 
      pull(two_year_retention) %>% 
      unique()
  
    return(c("other", two_year_retention))
  } else if (moa == "tnfi") {
    two_year_retention <- output %>% 
      filter(drug_os %in% tnfi) %>%  # calls from outside
      mutate(two_year_retention = sum(`24_month`) / sum(`0_month`) * 100) %>% 
      pull(two_year_retention) %>% 
      unique()
    
    return(c("tnfi", two_year_retention))
  } else if (moa == "jaki") {
      two_year_retention <- output %>% 
        filter(drug_os %in% jaki) %>%  # calls from outside
        mutate(two_year_retention = sum(`24_month`) / sum(`0_month`) * 100) %>% 
        pull(two_year_retention) %>% 
        unique()
      return(c("jaki", two_year_retention))
      
    } else {
    return("Please give a valid class, which can be tnfi, jaki, or others")
  }
}

two_year_rr(output, moa = "jaki")
two_year_rr(output, moa = "tnfi")
two_year_rr(output, moa = "others")


