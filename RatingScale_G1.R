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
#library(irr)

# ---------------------
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

# -----------------------
# create a list of the transposed data frames
transposed_data_list <- lapply(1:4, function(i) {
  file_name <- paste0("G", i, "_Q1_transposed")
  data <- get(file_name)
  data$Group <- paste0("G", i)
  data
})

# merge the data frames with an index column
merged_data_Q1 <- bind_rows(transposed_data_list)

# ---------------------------- 
# Combine the Domain and Action columns
Q1_comb <- unite(merged_data_Q1 , "Domain_Action", Domain, Action, sep = "")

# Reshape the data from long to wide format
Q1_wide <- pivot_wider(Q1_comb, names_from = Domain_Action, values_from = Rating)

# Print the resulting data frame
print(Q1_wide)

# Merge duplicated rows by taking the non-NA value in each column
Q1_wide_new <- data.frame(SentenceType = unique(Q1_wide$SentenceType))
for (col in names(Q1_wide)[-1]) {
  Q1_wide_new[[col]] <- sapply(split(Q1_wide[[col]], Q1_wide$SentenceType), function(x) x[!is.na(x)][1])
}

print(Q1_wide_new) 

# ------------------------------
# Subset data for G1 and M domain only
merged_data_G1_M <- merged_data_Q1[merged_data_Q1$Domain == "M" & merged_data_Q1$Group == "G1", ]

# Reshape data into wide format
wide_data_G1_M <- pivot_wider(merged_data_G1_M, names_from = c(ID), values_from = "Rating")

# Merge duplicated rows by taking the non-NA value in each column
G1_M_IRR <- data.frame(SentenceType = unique(wide_data_G1_M$SentenceType))
for (col in names(wide_data_G1_M)[-1]) {
  G1_M_IRR[[col]] <- sapply(split(wide_data_G1_M[[col]], wide_data_G1_M$SentenceType), function(x) x[!is.na(x)][1])
}

G1_M_IRR <- dplyr::select(G1_M_IRR, -c("Gender", "Domain", "Action", "Form", "Group"))
G1_M_IRR <- data.frame(G1_M_IRR,check.names = TRUE)

#-------------------- Inter rater test

# Assuming your data frame is called "my_data"
# Compute inter-rater reliability coefficients using alpha() function for each item
reliabilities <- apply(G1_M_IRR[, 2:19], 2, alpha, check.keys = FALSE)

# Add the SentenceType column back to the output
reliabilities_with_items <- cbind(SentenceType = G1_M_IRR$SentenceType, reliabilities)

# Print the reliability coefficients for each item
print(reliabilities)

# -------------------  Calculate the mode of each item 
install.packages("DescTools")
library(DescTools)

# Calculate the mode of each row (subject)
modes <- apply(G1_M_IRR[, 2:19], 1, mode)

# Print the modes for each subject
print(modes)

