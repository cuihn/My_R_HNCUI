# Cleaning a rating scale data from a perceptual rating online experiment

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


# Merge all data frames in the list by SentenceType
summary_all_rating_clean <- merge(all_rating_clean, all_rating_raw, by = "SentenceType", all.x = TRUE) %>%
  #filter(!is.na(Q1_mean_score)) %>%
  inner_join(all_rating_clean, by = "SentenceType",multiple = "all")

# write out results
write.xlsx(summary_all_rating_clean,file = 'summary_all_rating_clean.xlsx')

