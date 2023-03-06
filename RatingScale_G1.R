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
merged_data_Q1 <- bind_rows(transposed_data_list, .id = "Domain")

# ggplot to plot rating patterns,conduct basic stats, ICC analysis





# ### import G1_Q1 data
# 
# G1_Q1_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G1_Q1.txt')
# G1_Q1_new <- dplyr::select(G1_raw_data, 1, 10, 11, 20:ncol(G1_raw_data), 
#                         -starts_with("X"))
# names(G1_Q1_new)[1] <- "ID"
# 
# #transpose rows to columns/variables
# G1_Q1_transposed <- G1_Q1_new %>%
#   gather(key = "SentenceType", value = "Rating", -ID, -Age, -Gender) %>%
#   mutate(Domain = ifelse(grepl("(M|D)", SentenceType) & grepl("M", SentenceType), 
#                          "M", "D")) %>%
#   mutate(Action = ifelse(grepl("(I|A)", SentenceType) & grepl("I", SentenceType), 
#                          "I", "A")) %>%
#   mutate(Form = ifelse(grepl("(R|W)", SentenceType) & grepl("R", SentenceType), 
#                          "R", "W")) %>%
#   drop_na()
# 
# print(G1_Q1_transposed)
# 
# ### import G1_Q2 data
# 
# G1_Q2_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G1_Q2.txt') 
# names(G1_Q2_raw_data)[1] <- "ID"
# 
# #transpose rows to columns/variables
# G1_Q2_transposed <- G1_Q2_raw_data  %>%
#   gather(key = "SentenceType", value = "Rating", -ID, -Age, -Gender) %>%
#   mutate(Domain = ifelse(grepl("(M|D)", SentenceType) & grepl("M", SentenceType), 
#                          "M", "D")) %>%
#   mutate(Action = ifelse(grepl("(I|A)", SentenceType) & grepl("I", SentenceType), 
#                          "I", "A")) %>%
#   mutate(Form = ifelse(grepl("(R|W)", SentenceType) & grepl("R", SentenceType), 
#                        "R", "W")) %>%
#   drop_na()
# 
# print(G1_Q2_transposed)
# 
# 
# ### import G2_Q1 data
# 
# G2_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G2_Q1.txt')
# G2_new <- dplyr::select(G2_raw_data, 1, 11, 12, 21:ncol(G2_raw_data),
#                         -starts_with("X")) 
# names(G2_new)[1] <- "ID"
# 
# # transpose rows to columns/variables
# G2_transposed <- G2_new %>%
#   gather(key = "SentenceType", value = "Rating", -ID, -Age, -Gender) %>%
#   drop_na()
# 
# print(G2_transposed)
# 
# ### import G2_Q2 data
# 
# G2_Q2_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G2_Q2.txt') 
# names(G2_Q2_raw_data)[1] <- "ID"
# 
# #transpose rows to columns/variables
# G2_Q2_transposed <- G2_Q2_raw_data  %>%
#   gather(key = "SentenceType", value = "Rating", -ID) %>%
#   drop_na()
# 
# print(G2_Q2_transposed)
# 
# 
# ### import G3_Q1 data
# G3_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G3_Q1.txt')
# G3_new <- dplyr::select(G3_raw_data, 1, 10, 11, 20:ncol(G3_raw_data), 
#                         -starts_with("X")) 
# names(G3_new)[1] <- "ID"
# 
# #transpose rows to columns/variables （wide format to long format)
# G3_transposed <- G3_new %>%
#   gather(key = "SentenceType", value = "Rating", -ID, -Age, -Gender) %>%
#   drop_na()
# 
# print(G3_transposed)
# 
# ### import G3_Q2 data
# 
# G3_Q2_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G3_Q2.txt') 
# names(G3_Q2_raw_data)[1] <- "ID"
# 
# #transpose rows to columns/variables
# G3_Q2_transposed <- G3_Q2_raw_data  %>%
#   gather(key = "SentenceType", value = "Rating", -ID) %>%
#   drop_na()
# 
# print(G3_Q2_transposed)
# 
# 
# ### import G4_Q1 data 
# G4_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G4_Q1.txt')
# G4_new <- dplyr::select(G4_raw_data, 1, 11, 12, 21:ncol(G4_raw_data), 
#                         -starts_with("X")) 
# names(G4_new)[1] <- "ID"
# 
# # transpose rows to columns/variables （wide format to long format)
# G4_transposed <- G4_new %>%
#   gather(key = "SentenceType", value = "Rating", -Gender, -ID, -Age, ) %>%
#   drop_na()
# 
# print(G4_transposed)
# 
# ### import G4_Q2 data
# 
# G4_Q2_raw_data <- read.delim('/Users/hainingcui/Dropbox/Trying/G4_Q2.txt') 
# names(G4_Q2_raw_data)[1] <- "ID"
# 
# #transpose rows to columns/variables
# G4_Q2_transposed <- G4_Q2_raw_data  %>%
#   gather(key = "SentenceType", value = "Rating", -ID) %>%
#   drop_na()
# 
# print(G4_Q2_transposed)
# 
# ### merge all the tansposed datafames into one with sentenceType as index
# 
# merged_all <- merge(G1_transposed,G1_Q2_transposed, merged__all)
