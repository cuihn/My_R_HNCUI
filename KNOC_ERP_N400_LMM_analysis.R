
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
# #endregion


# model including all channels and regions of intrestes based on previous studies

#import data
factor_data_all <- read.delim('KNOC_critical_N400_dataForLMM.txt')

factor <- data.frame(factor_data_all)  %>%
  gather("condition", "loading", 1:225, -subject) %>%
  extract(condition, into = c("Familarity", "TruthValue", "Electrode"), regex = "(\\w+)([\\w\\s]+)_(\\w+)")


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

# Load the data and create a new variable for hemisphere
factor_new <- data.frame(factor) %>%
  mutate(Electrode = as.factor(Electrode)) %>%
  mutate(HEM = ifelse(grepl("[13579]", Electrode), "L", "R"))

for (p in Electrode_patterns) {
  factor_new <- factor_new %>%
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

factor_new <- factor_new %>%
  filter(REG != "other")

View(factor_new)

write.csv(factor_new, "clean_data_N400.csv", row.names = TRUE)

##### LMM tests
  
# LMM <- lmer(loading ~ Familarity*TruthValue*Electrode + (1|subject), factor)
# 
# summary(LMM)
# 
# emmeans(LMM, pairwise ~ TruthValue*TruthValue|Electrode, adjust="tukey")




##### not used 
  # gather(key = "subject", value = "value") %>%
  # separate(condition, c("Familarity", "TruthValue", "Electrode"), sep = c(1,2)) %>%
  #separate(condition, c("Familarity", "Electrode"), sep = c(1,2)) %>%
  # mutate(TruthValue = as.factor(TruthValue)) %>%
  # mutate(TF = fct_relevel(TruthValue, "T")) %>%
  # mutate(Electrode = as.factor(Electrode)) %>%
#Eletrode_patterns <- c("AF3|FP1|F7|F5|F3|FT7|FC5|FC3", 
                       "T7C5|C3|TP7|CP5|CP3",
                       "P7|P5| P3|PO9|PO7|PO3",
                       "F1|FZ|F2|FC1|FCZ|FC2",
                       "C1|CZ|C2|CP1|CPZ|CP2",
                       "P1|PZ|P2|O1|POZ|O2",
                       "AF4|FP2|F4|F6|F8|FC4|FC6|FT8",
                       "C4|C6|T8|CP4|CP6|TP8",
                       "P4|P6|P8|PO4|PO8|PO10")
#mutate(REG = ifelse(grepl("AF3|FP1|F7|F5|F3|FT7|FC5|FC3", Electrode), "LA", "other"))
#####
#factor_new <- data.frame(factor) %>%
 # mutate(Electrode = as.factor(Electrode)) %>%
 # mutate(HEM = ifelse(grepl("[13579]", Electrode), "L", "R")) %>%
#for (p in Eletrode_patterns) {
  #factor_new <- factor_new %>%
  #mutate(REG = !!paste0("has_", p) := ifelse(grepl(p, Electrode), TRUE, FALSE))
}
  # mutate(has_FP_FD_FS = grepl("FP|FD|FS", variables)) %>%
  # mutate(LA = ifelse(has_FP_FD_FS, TRUE, FALSE))
  # mutate(!!paste0("has_", p) := ifelse(grepl(p, Electrode), TRUE, FALSE))
#####