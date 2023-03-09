
# Analysis of ERPs data from a Knowledge-belief attribution experiment

# #region
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
library(dplyr)
# #endregion


# model including all channels and regions of interests based on previous studies

#import data from ERPlab generated dataframe in long format
All_Channal <- read.delim('mean_amplitude_N400_peakLatencyDetection_ALLCHA_20230309.txt')

# rename variable names
All_Channal <- setNames(All_Channal, c("value" = "Activation", 
                                       "chindex" = "ChaNum",
                                       "chlabel" = "Electrode", 
                                       "bini" = "BinNum",
                                       "binlable" = "Condition", 
                                       "ERPset" = "Subject"))

# clean data by adding two condtions as columns
Clean_Channal <- All_Channal %>%
  mutate(Familiarity_HL = case_when(
    grepl("H", Condition) ~ "H",
    grepl("L", Condition) ~ "L",
    TRUE ~ NA_character_
  ),
  TruthValue_FT = case_when(
    grepl("F", Condition) ~ "F",
    grepl("T", Condition) ~ "T",
    TRUE ~ NA_character_
  ))

# rename subject by num
Clean_Channal$Subject <- gsub("[^0-9]", " ", Clean_Channal$Subject)

#define the Electrodes into 9 group region
Electrode_patterns <- c("(?i)AF3|(?i)Fp1|(?i)F7|(?i)F5|(?i)F3|(?i)FT7|(?i)FC5|(?i)FC3", 
                       "(?i)T7|(?i)C5|(?i)C3|(?i)TP7|(?i)CP5|(?i)CP3",
                       "(?i)P7|(?i)P5|(?i)P3|(?i)PO9|(?i)PO7|(?i)PO3",
                       "(?i)F1|(?i)FZ|(?i)F2|(?i)FC1|(?i)FCZ|(?i)FC2",
                       "(?i)C1|(?i)CZ|(?i)C2|(?i)CP1|(?i)CPZ|(?i)CP2",
                       "(?i)P1|(?i)PZ|(?i)P2|(?i)O1|(?i)POZ|(?i)O2",
                       "(?i)AF4|(?i)Fp2|(?i)F4|(?i)F6|(?i)F8|(?i)FC4|(?i)FC6|(?i)FT8",
                       "(?i)C4|(?i)C6|(?i)T8|(?i)CP4|(?i)CP6|(?i)TP8",
                       "(?i)P4|(?i)P6|(?i)P8|(?i)PO4|(?i)PO8|(?i)PO10")

#Define the names of each region
Value_names <- c("LA", "LC", "LP", "MA",
                 "MC", "MP", "RA", "RC", "RP")

# create new variable for hemisphere and regions
Clean_Channal <- data.frame(Clean_Channal) %>%
  #mutate(Electrode = as.factor(Electrode)) %>%
  mutate(HEM = ifelse(grepl("[13579]", Electrode), "L", "R"))

for (p in Electrode_patterns) {
  Clean_Channal <- Clean_Channal %>%
    mutate(REG = ifelse(grepl(Electrode_patterns[1], Electrode), Value_names[1],
                        ifelse(grepl(Electrode_patterns[2], Electrode), Value_names[2],
                               ifelse(grepl(Electrode_patterns[3], Electrode), Value_names[3],
                                      ifelse(grepl(Electrode_patterns[4], Electrode), Value_names[4],
                                             ifelse(grepl(Electrode_patterns[5], Electrode), Value_names[5],
                                                    ifelse(grepl(Electrode_patterns[6], Electrode), Value_names[6],
                                                           ifelse(grepl(Electrode_patterns[7], Electrode), Value_names[7],
                                                                  ifelse(grepl(Electrode_patterns[8], Electrode), Value_names[8],
                                                                         ifelse(grepl(Electrode_patterns[9], Electrode), Value_names[9], "Other")))))))))) 
    
  
    
}

Clean_Channal <- Clean_Channal %>%
  filter(REG != "Other")
  subset(Clean_Channal, REG != "Other")

write.csv(Clean_Channal, "clean_data_N400.csv", row.names = TRUE)

# LMM tests
  
# LMM <- lmer(loading ~ Familarity*TruthValue*Electrode + (1|subject), factor)
# 
# summary(LMM)
# 
# emmeans(LMM, pairwise ~ TruthValue*TruthValue|Electrode, adjust="tukey")



