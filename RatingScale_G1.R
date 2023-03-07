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


# Reshape data into wide format
wide_data_G1_M <- pivot_wider(merged_data_G1_M, names_from = c(ID), values_from = "Rating")


# Merge duplicated rows by taking the non-NA value in each column
G1_M_IRR <- data.frame(SentenceType = unique(wide_data_G1_M$SentenceType))
for (col in names(wide_data_G1_M)[-1]) {
  G1_M_IRR[[col]] <- sapply(split(wide_data_G1_M[[col]], wide_data_G1_M$SentenceType), function(x) x[!is.na(x)][1])
}

G1_M_IRR <- dplyr::select(G1_M_IRR, -c("Gender", "Domain", "Action", "Form", "Group"))
G1_M_IRR


