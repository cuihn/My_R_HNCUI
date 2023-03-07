# Analysis of rating scale data from a perceptual rating online experiment

# install.packages('lme4') 
library(lme4)
# install.packages('lsmeans') 
library(emmeans)
# install.packages("lmerTest") 
library(lmerTest)
# install.packages("tidyverse") 
library(tidyverse)
# install.packages("ggplot2") 
library(ggplot2)
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
library(irr)

for (i in 1:4) {
  file_name <- paste0("G", i, "_Q1.txt")
  file_path <- paste0("/Users/hainingcui/Dropbox/Trying/", file_name)
  raw_data <- read.delim(file_path)
  new_data <- dplyr::select(raw_data, 1, 10, 11, 20:ncol(raw_data), -starts_with("X"))
  names(new_data)[1] <- "ID"
  if ("Gender" %in% names(new_data)) {
    transposed_data <- new_data %>%
      gather(key = "SentenceType", value = "Rating", -ID, -Age, -Gender) %>%
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

# create a list of the transposed data frames
transposed_data_list <- lapply(1:4, function(i) {
  file_name <- paste0("G", i, "_Q1_transposed")
  data <- get(file_name)
  data$Group <- paste0("G", i)
  data
})

# merge the data frames with an index column
merged_data_Q1 <- bind_rows(transposed_data_list)

# Summary statistics
# summary_df <- merged_data_Q1 %>%
#   group_by(Form) %>%
#   summarise(Mean = mean(Rating), Median = median(Rating), SD = sd(Rating))

# Subset data for M and D domains only
merged_data_G1 <- merged_data_Q1[merged_data_Q1$Group %in% c("G1"),]

# Reshape data into wide format
wide_data_G1 <- pivot_wider(merged_data_G1, names_from = c(ID), values_from = "Rating")

# Subset data for M and D domains only
merged_data_G1_M <- merged_data_G1[merged_data_G1$Domain %in% c("M"),]

# Reshape data into wide format
wide_data_G1_M <- pivot_wider(merged_data_G1_M, names_from = c(ID), values_from = "Rating")

# Merge duplicated rows by taking the non-NA value in each column
G1_M_IRR <- data.frame(SentenceType = unique(wide_data_G1_M$SentenceType))
for (col in names(wide_data_G1_M)[-1]) {
  G1_M_IRR[[col]] <- sapply(split(wide_data_G1_M[[col]], wide_data_G1_M$SentenceType), function(x) x[!is.na(x)][1])
}

G1_M_IRR

# Merge duplicated rows by taking the non-NA value in each column
G1_M_IRR <- data.frame(SentenceType = unique(wide_data_G1_M$SentenceType))
for (col in names(wide_data_G1_M)[-1]) {
  G1_M_IRR[[col]] <- sapply(split(wide_data_G1_M[[col]], wide_data_G1_M$SentenceType), function(x) x[!is.na(x)][1])
}

G1_M_IRR <- dplyr::select(G1_M_IRR, -c("Gender", "Domain", "Action", "Form", "Group"))
G1_M_IRR

###
icc_results <- icc(G1_M_IRR[,2:19], model = "twoway", type = "consistency", unit = "single")

# Calculate ICC for each sentence item
icc_results <- apply(G1_M_IRR[,2:19], 2, function(x) icc(x, model = "twoway", type = "consistency", unit = "single"))
icc_results

# # Sort ICC values in descending order
icc_sorted <- sort(icc_results$ICC, decreasing = TRUE)
# 
# # Select sentences with highest ICC values
# high_icc <- icc_results[order(icc_results$ICC, decreasing = TRUE), ][2:5, ]
# 
# 
# 
# # Identify items with low ICC
# low_icc_items <- which(icc$value < 0.6)
# 
# # Create new dataset with low-ICC items removed
# new_ratings <- G1_M_IRR[,-low_icc_items]
# 
# # Print summary statistics for new dataset
# summary(new_ratings)
# 
# # print
# print(low_icc_items )

# # subset the data for the two groups
# Action_A <- merged_data_Q1$Rating[merged_data_Q1$Action == "A"]
# Action_I <- merged_data_Q1$Rating[merged_data_Q1$Action == "I"]
