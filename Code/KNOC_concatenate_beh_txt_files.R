# R code for looping a file in a folder and concatenate into a df

# install.packages("plyr")
# install.packages("readr")
# install.packages("stringr")


library(stringr)
library(dplyr)
library(plyr)                                   
library(readr)
library(magrittr) # pip operation package
library(tidyverse)

#####

# Define the path and pattern
path <- "C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_Analysis\\Stimuli"
pattern <- ".CSV"

# Load preEEG stimuli validation files
Recorded_stimuli <- read.csv("C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_Analysis\\Stimuli\\KNOC_recorded_stimuliExp3_20230921.csv")

Recorded_stimuli <- Recorded_stimuli %>%
  mutate(Sentence = tolower(Sentence))

# clean preEEG and recorded stimuli data 
Recorded_stimuli <- Recorded_stimuli[Recorded_stimuli$Selected.for.critical.stimuli.in.Exp3b == 'y', ]

#
PreEEG_Validated <- read.csv("C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_Analysis\\Stimuli\\PreEEG_AllRate_20230913.csv")

# merge preEEG rating and recording
merged_preEEG <- merge(PreEEG_Validated, Recorded_stimuli, by  = c("Set_Number", "Knowledge_Type", "Truth.Value"), all.x = FALSE, all.y = TRUE)

merged_preEEG <- merged_preEEG %>%
  mutate(words = strsplit(Sentence, " ")) %>%
  mutate(last_word = sapply(words, function(x) tail(x, 1))) %>%
  select(-words)

merged_preEEG <- merged_preEEG %>%
  mutate(words = strsplit(Sentence, " ")) %>%
  mutate(mid_word1 = sapply(words, function(x) x[3]))%>%
  select(-words)

merged_preEEG <- merged_preEEG %>%
  mutate(words = strsplit(Sentence, " ")) %>%
  mutate(mid_word3 = sapply(words, function(x) x[5]))%>%
  select(-words)

merged_preEEG <- merged_preEEG %>%
  mutate(words = strsplit(Sentence, " ")) %>%
  mutate(mid_word4 = sapply(words, function(x) x[9]))%>%
  select(-words)


merged_preEEG <- merged_preEEG[merged_preEEG$Truth.Value == 'TRUE', ]

######

# load postEEG stimuli validation file
PostEEG_TrueValue <- read.csv("C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_Analysis\\Stimuli\\PostEEG_TruthValueRat_20230921.csv")
PostEEG_anwser <- read.csv("C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_Analysis\\Stimuli\\PostEEG_knowledge_key_20230921.csv")


# clean post scores
PostEEG_anwser <- PostEEG_anwser %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(last_word = sapply(words, function(x) tail(x, 1)))%>%
  select(-words)

PostEEG_anwser <- PostEEG_anwser %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word1 = sapply(words, function(x) x[3]))%>%
  select(-words)

PostEEG_anwser <- PostEEG_anwser %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word2 = sapply(words, function(x) x[4]))%>%
  select(-words)

PostEEG_anwser <- PostEEG_anwser %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word3 = sapply(words, function(x) x[5]))%>%
  select(-words)

PostEEG_anwser <- PostEEG_anwser %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word4 = sapply(words, function(x) x[9]))%>%
  select(-words)

###
PostEEG_TrueValue <- PostEEG_TrueValue %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(last_word = sapply(words, function(x) tail(x, 1)))%>%
  select(-words)

PostEEG_TrueValue <- PostEEG_TrueValue %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word1 = sapply(words, function(x) x[5]))%>%
  select(-words)

PostEEG_TrueValue <- PostEEG_TrueValue %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word2 = sapply(words, function(x) x[6]))%>%
  select(-words)

PostEEG_TrueValue <- PostEEG_TrueValue %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word3 = sapply(words, function(x) x[7]))%>%
  select(-words)

PostEEG_TrueValue <- PostEEG_TrueValue %>%
  mutate(words = strsplit(Statement, " ")) %>%
  mutate(mid_word4 = sapply(words, function(x) x[11]))%>%
  select(-words)



# merge post rating with key stimuli numbers
Merged_Truth_key <- merge(PostEEG_anwser, PostEEG_TrueValue, by = c("last_word", "mid_word1", "mid_word2", "mid_word3", "mid_word4"),all.x = TRUE, all.y = TRUE)

names(Merged_Truth_key)[9] <- "Knowledge_Type"


##
Merged_pre_post <- merge(Merged_Truth_key, merged_preEEG, by = c("last_word", "mid_word1", "Knowledge_Type"), all = TRUE)

#Merged_pre_post1 <- merge(Merged_Truth_key, merged_preEEG, by = NULL)

write.csv(Merged_pre_post,"C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_Analysis\\Stimuli\\Merged_pre_post_v20.csv")

