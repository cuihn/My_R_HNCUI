# Analysis of rating scale data from a perceptual rating online experiment

# ---------------------
# install.packages('lme4') 
library(lme4)
# install.packages('lsmeans') 
library(emmeans)
# install.packages("lmerTest") 
library(lmerTest)
# install.packages("tidyverse") 
library(tidyverse)
# install.packages("ggplot2") 
#library(ggplot2)
library(arm)
library(forcats)
library(psych)
library(stringr)
# install.packages("dplyr")
library(dplyr)
# install.packages("magrittr") 
library(magrittr)
library(tidyr)
# install.packages("labeling")
library(labeling)
# install.packages("ICC")
# library(irr)
# install.packages("openxlsx")
library(openxlsx)
# install.packages("scales")
library(scales)

# --------------------- import data with sentences

all_rating_raw <- read.delim("/Users/hainingcui/Dropbox/Trying/Written Validation Results.txt")

# --------------------- transpose the four groups data for Q1
for (i in 1:4) {
  file_name <- paste0("G", i, "_Q1.txt")
  file_path <- paste0("/Users/hainingcui/Dropbox/Trying/", file_name)
  raw_data_Q1 <- read.delim(file_path)
  raw_data_Q1 <- dplyr::select(raw_data_Q1, 1, 10, 11, 20:ncol(raw_data_Q1), -starts_with("X"))
  names(raw_data_Q1)[1] <- "ID"
  if ("Gender" %in% names(raw_data_Q1)) {
    transposed_data <- raw_data_Q1 %>%
      gather(key = "SentenceType", value = "Rating_Q1", -ID, -Age, -Gender) %>%
      mutate(Domain = ifelse(grepl("(M|D)", SentenceType) & grepl("M", SentenceType), 
                             "M", "D")) %>%
      mutate(Action = ifelse(grepl("(I|A)", SentenceType) & grepl("I", SentenceType), 
                             "I", "A")) %>%
      mutate(Form = ifelse(grepl("(R|W)", SentenceType) & grepl("R", SentenceType), 
                           "R", "W")) %>%
      drop_na()
    assign(paste0("G", i, "_Q1_transposed"), transposed_data)
  } else {
    cat("Gender column not found in", file_path, "\n")
  }
}

# ----------------------- merge the data frames with an index column and Group as an index (column)

# create a list of the transposed data frames
transposed_data_list_Q1 <- lapply(1:4, function(i) {
  file_name <- paste0("G", i, "_Q1_transposed")
  data <- get(file_name)
  data$Group <- paste0("G", i)
  data
})

# merge the data frames with an index column
merged_data_Q1 <- bind_rows(transposed_data_list_Q1)

# --------------------- transpose the four groups data for Q2
for (i in 1:4) {
  file_name <- paste0("G", i, "_Q2.txt")
  file_path <- paste0("/Users/hainingcui/Dropbox/Trying/", file_name)
  raw_data_Q2 <- read.delim(file_path)
  raw_data_Q2 <- dplyr::select(raw_data_Q2, 1:ncol(raw_data_Q2), -starts_with("X")) # remove NA column
  names(raw_data_Q2)[1] <- "Participant_ID"  
  transposed_data_Q2 <- raw_data_Q2 %>%
    gather(key = "SentenceType", value = "Rating_Q2", -Participant_ID) %>%
    mutate(Domain = ifelse(grepl("(M|D)", SentenceType) & grepl("M", SentenceType), 
                           "M", "D")) %>%
    mutate(Action = ifelse(grepl("(I|A)", SentenceType) & grepl("I", SentenceType), 
                           "I", "A")) %>%
    mutate(Form = ifelse(grepl("(R|W)", SentenceType) & grepl("R", SentenceType), 
                         "R", "W")) %>%
    drop_na()
  assign(paste0("G", i, "_Q2_transposed"), transposed_data_Q2)
}

# ----------------------- merge the data frames with an index column and Group as an index (column)

# create a list of the transposed data frames
transposed_data_list2 <- lapply(1:4, function(i) {
  file_name <- paste0("G", i, "_Q2_transposed")
  data <- get(file_name)
  data$Group <- paste0("G", i)
  data
})

# merge the data frames with an index column
merged_data_Q2 <- bind_rows(transposed_data_list2)


# merge merged_q1 & q2 into one
# all_rating_clean <- merge(merged_data_Q1,merged_data_Q2, by = c("SentenceType", "Domain", "Action", "Form", unique = TRUE))

all_rating_clean <- merge(merged_data_Q1, merged_data_Q2, by = c("SentenceType", "Domain", "Action", "Form"))
all_rating_clean <- all_rating_clean[!duplicated(all_rating_clean$SentenceType), ]

# 
# write.xlsx(all_rating_clean, file = "all_rating_clean.xlsx")
# 
# 
# 
# summary_all_rating_clean <- all_rating_clean %>%
#   group_by(SentenceType, Domain, Action, Form, ID, Age, Rating_Q2) %>%
#   summarise(mean_score = mean(Rating_Q1),
#             median_score = median(Rating_Q1),
#             q1 = quantile(Rating_Q1, 0.25),
#             q3 = quantile(Rating_Q1, 0.75),
#             median_range = q3 - q1,
#             sd_score = sd(Rating_Q1, na.rm = TRUE),
#             min_score = min(Rating_Q1),
#             max_score = max(Rating_Q1),
#             pct_1 = round(mean(Rating_Q1 == 1) * 100),
#             pct_2 = round(mean(Rating_Q1 == 2) * 100),
#             pct_3 = round(mean(Rating_Q1 == 3) * 100),
#             pct_4 = round(mean(Rating_Q1 == 4) * 100),
#             pct_5 = round(mean(Rating_Q1 == 5) * 100)) %>%
#   filter((pct_1 + pct_2) >= 80) # simplify the filter condition
# 
# 
# 
# summary_all_rating_clean <- all_rating_clean %>%
#   group_by(SentenceType, Domain, Action, Form, ID, Age) %>%
#   summarise(mean_score = mean(Rating_Q1),
#             median_score = median(Rating_Q1),
#             q1 <- quantile(Rating_Q1, 0.25),
#             q3 <- quantile(Rating_Q1, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating_Q1),
#             min_score = min(Rating_Q1),
#             max_score = max(Rating_Q1),
#             pct_1 = mean(Rating_Q1 == 1),
#             pct_2 = mean(Rating_Q1 == 2),
#             pct_3 = mean(Rating_Q1 == 3),
#             pct_4 = mean(Rating_Q1 == 4),
#             pct_5 = mean(Rating_Q1 == 5),
#             total_pct_1_2 = (mean(Rating_Q1 == 1) + mean(Rating_Q1 == 2)) * 100) %>%
#   filter(total_pct_1_2 >= 80)
# 
# 
# 
# # summary_high_all_rating_clean <- summary_high_all_rating_clean %>%
# #   mutate(across(Q1_pct_1:Q2_total_pct_3_4, percent_format(scale = 100)))



# Merge all data frames in the list by SentenceType
summary_all_rating_clean <- merge(all_rating_clean, all_rating_raw, by = "SentenceType", all.x = TRUE) %>%
  #filter(!is.na(Q1_mean_score)) %>%
  inner_join(all_rating_clean, by = "SentenceType",multiple = "all")

# write out results
write.xlsx(summary_all_rating_clean,file = 'summary_all_rating_clean.xlsx')



# # --------------------------- summary for descriptive stats and select LOW score 
# summary_low_Q1 <- merged_data_Q1 %>%
#   # filter(Action == "A") %>%
#   # filter(Domain == "D") %>%
#   group_by(SentenceType, Domain, Action) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_1_2 = (mean(Rating == 1) + mean(Rating == 2)) * 100) %>%
#   filter(total_pct_1_2 >= 80)
# 
# # Merge all data frames in the list by SentenceType
# summary_low_Q1 <- merge(summary_low_Q1, all_rating, by = "SentenceType", all.x = TRUE) %>%
#   filter(!is.na(mean_score)) %>%
#   inner_join(all_rating, by = "SentenceType")
# 
# # write out results
# write.xlsx(summary_low_Q1,file = 'merged_df_low_Q1.xlsx')
# 

# # --------------------------- summary for descriptive stats and select low score 
# summary_G4_AD_low <- G4_Q1_transposed %>%
#   filter(Action == "A") %>%
#   filter(Domain == "D") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_1_2 = (mean(Rating == 1) + mean(Rating == 2)) * 100) %>%
#   filter(total_pct_1_2 >= 80)
# 
# write.xlsx(summary_G4_AD_low,file = 'summary_G4_AD_low.xlsx')
# 
# 
# # --------------------------- summary for descriptive stats and select high score 
# summary_G4_ID_high <- G4_Q1_transposed %>%
#   filter(Action == "I") %>%
#   filter(Domain == "D") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_4_5 = (mean(Rating == 4) + mean(Rating == 5)) * 100) %>%
#   filter(total_pct_4_5 >= 80)
# 
# write.xlsx(summary_G4_ID_high,file = 'summary_G4_ID_high.xlsx')
# 
# # --------------------------- summary for descriptive stats and select low score 
# summary_G4_ID_low <- G4_Q1_transposed %>%
#   filter(Action == "I") %>%
#   filter(Domain == "D") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_1_2 = (mean(Rating == 1) + mean(Rating == 2)) * 100) %>%
#   filter(total_pct_1_2 >= 80)
# 
# write.xlsx(summary_G4_ID_low,file = 'summary_G4_ID_low.xlsx')
# 
# 
# # --------------------------- summary for descriptive stats and select high score 
# summary_G4_AM_high <- G4_Q1_transposed %>%
#   filter(Action == "A") %>%
#   filter(Domain == "M") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_4_5 = (mean(Rating == 4) + mean(Rating == 5)) * 100) %>%
#   filter(total_pct_4_5 >= 80)
# 
# write.xlsx(summary_G4_AM_high,file = 'summary_G4_AM_high.xlsx')
# 
# # --------------------------- summary for descriptive stats and select low score 
# summary_G4_AM_low <- G4_Q1_transposed %>%
#   filter(Action == "A") %>%
#   filter(Domain == "M") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_1_2 = (mean(Rating == 1) + mean(Rating == 2)) * 100) %>%
#   filter(total_pct_1_2 >= 80)
# 
# write.xlsx(summary_G4_AM_low,file = 'summary_G4_AM_low.xlsx')
# 
# 
# # --------------------------- summary for descriptive stats and select high score 
# summary_G4_IM_high <- G4_Q1_transposed %>%
#   filter(Action == "I") %>%
#   filter(Domain == "M") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_4_5 = (mean(Rating == 4) + mean(Rating == 5)) * 100) %>%
#   filter(total_pct_4_5 >= 80)
# 
# write.xlsx(summary_G4_IM_high,file = 'summary_G4_IM_high.xlsx')
# 
# # --------------------------- summary for descriptive stats and select low score 
# summary_G4_IM_low <- G4_Q1_transposed %>%
#   filter(Action == "I") %>%
#   filter(Domain == "M") %>%
#   group_by(SentenceType) %>%
#   summarise(mean_score = mean(Rating),
#             median_score = median(Rating),
#             q1 <- quantile(Rating, 0.25),
#             q3 <- quantile(Rating, 0.75),
#             median_range <- q3 - q1,
#             sd_score = sd(Rating),
#             min_score = min(Rating),
#             max_score = max(Rating),
#             pct_1 = mean(Rating == 1),
#             pct_2 = mean(Rating == 2),
#             pct_3 = mean(Rating == 3),
#             pct_4 = mean(Rating == 4),
#             pct_5 = mean(Rating == 5),
#             total_pct_1_2 = (mean(Rating == 1) + mean(Rating == 2)) * 100) %>%
#   filter(total_pct_1_2 >= 80)
# 
# write.xlsx(summary_G4_IM_low,file = 'summary_G4_IM_low.xlsx')
# 
# # ---------------- merger data with describe stats
# # Create a list of data frames to merge
# summary_list1 <- list(summary_G1_AD_high, summary_G1_AD_low, 
#                      summary_G1_AM_high, summary_G1_AM_low, 
#                      summary_G1_ID_high, summary_G1_ID_low, 
#                      summary_G1_IM_high, summary_G1_IM_low)
# 
# # Create a list of data frames to merge
# summary_list2 <- list(
#                      summary_G2_AD_high, summary_G2_AD_low,
#                      summary_G2_AM_high, summary_G2_AM_low,
#                      summary_G2_ID_high, summary_G2_ID_low,
#                      summary_G2_IM_high, summary_G2_IM_low
#                      )
# 
# summary_list3 <- list(summary_G3_AD_high, summary_G3_AD_low, 
#                       summary_G3_AM_high, summary_G3_AM_low, 
#                       summary_G3_ID_high, summary_G3_ID_low, 
#                       summary_G3_IM_high, summary_G3_IM_low)
# 
# summary_list4 <- list(summary_G4_AD_high, summary_G4_AD_low, 
#                       summary_G4_AM_high, summary_G4_AM_low, 
#                       summary_G4_ID_high, summary_G4_ID_low, 
#                       summary_G4_IM_high, summary_G4_IM_low)
# 
# 
# 
# # Find index of empty element in list
# empty_index <- which(sapply(summary_list, function(x) is.null(x) || length(x) == 0))

# # Remove empty element from list
# if (length(empty_index) > 0) {
#   summary_list1 <- summary_list1[-empty_index]
#   summary_list2 <- summary_list2[-empty_index]
#   summary_list3 <- summary_list3[-empty_index]
#   summary_list4 <- summary_list4[-empty_index]
# }
# 
# # Merge all data frames in the list by SentenceType
# merged_df <- all_rating
# for (df in summary_list1) {
#   merged_df <- merge(merged_df, df, by = "SentenceType", all = TRUE)
# }
# for (df in summary_list2) {
#   merged_df <- merge(merged_df, df, by = "SentenceType", all = TRUE)
# }
# merged_df <- all_rating
# for (df in summary_list3) {
#   merged_df <- merge(merged_df, df, by = "SentenceType", all = TRUE)
# }
# for (df in summary_list4) {
#   merged_df <- merge(merged_df, df, by = "SentenceType", all = TRUE)
# }
# # Print merged data frame
# merged_df
# 
# 
# write.xlsx(merged_df,file = 'summaary_merged_df_Q1.xlsx')


# # ---------------------------- 
# # Combine the Domain and Action columns
# Q1_comb <- unite(merged_data_Q1 , "Domain_Action", Domain, Action, sep = "")
# 
# # Reshape the data from long to wide format
# Q1_wide <- pivot_wider(Q1_comb, names_from = Domain_Action, values_from = Rating)
# 
# # Print the resulting data frame
# print(Q1_wide)
# 
# # Merge duplicated rows by taking the non-NA value in each column
# Q1_wide_new <- data.frame(SentenceType = unique(Q1_wide$SentenceType))
# for (col in names(Q1_wide)[-1]) {
#   Q1_wide_new[[col]] <- sapply(split(Q1_wide[[col]], Q1_wide$SentenceType), function(x) x[!is.na(x)][1])
# }
# 
# print(Q1_wide_new) 
# 
# # ------------------------------
# # Subset data for G4 and M domain only
# merged_data_G4_M <- merged_data_Q1[merged_data_Q1$Domain == "M" & merged_data_Q1$Group == "G4", ]
# 
# # Reshape data into wide format
# wide_data_G4_M <- pivot_wider(merged_data_G4_M, names_from = c(ID), values_from = "Rating")
# 
# # Merge duplicated rows by taking the non-NA value in each column
# G4_M_IRR <- data.frame(SentenceType = unique(wide_data_G4_M$SentenceType))
# for (col in names(wide_data_G4_M)[-1]) {
#   G4_M_IRR[[col]] <- sapply(split(wide_data_G4_M[[col]], wide_data_G4_M$SentenceType), function(x) x[!is.na(x)][1])
# }
# 
# G4_M_IRR <- dplyr::select(G4_M_IRR, -c("Gender", "Domain", "Action", "Form", "Group"))
# G4_M_IRR <- data.frame(G4_M_IRR,check.names = TRUE)
# 
# #-------------------- Inter rater test
# 
# # Assuming your data frame is called "my_data"
# # Compute inter-rater reliability coefficients using alpha() function for each item
# reliabilities <- apply(G4_M_IRR[, 2:19], 2, alpha, check.keys = FALSE)
# 
# # Add the SentenceType column back to the output
# reliabilities_with_items <- cbind(SentenceType = G4_M_IRR$SentenceType, reliabilities)
# 
# # Print the reliability coefficients for each item
# print(reliabilities)
# 
# # -------------------  Calculate the mode of each item 
# install.packages("DescTools")
# library(DescTools)
# 
# # Calculate the mode of each row (subject)
# modes <- apply(G4_M_IRR[, 2:19], 1, mode)
# 
# # Print the modes for each subject
# print(modes)
# 
# 
# # ----------------------------
# 
# # Create a matrix with the number of raters who assigned each rating
# ratings <- G4_M_IRR[, 2:19]
# rating_matrix <- t(apply(ratings, 1, tabulate, nbins = 5))
# 
# # Calculate Fleiss' kappa for each item with 18 raters
# kappas <- apply(rating_matrix, 1, function(row) {
#   if(sum(!is.na(row)) == 18) {
#     kappam.fleiss(row)
#   } else {
#     NA
#   }
# })

