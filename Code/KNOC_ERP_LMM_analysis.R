# Analysis of ERPs data (v2) from a Knowledge-belief attribution experiment
# model including all channels and regions of interests based on previous studies 

# Install pak -------------------------------------------------------------
# install.packages("lmerTest")
# library(lsmeans)
# install.packages("devtools")
# install.packages
# install.packages("purrr")

# Import libs -------------------------------------------------------------
library(lme4)
library(emmeans)
library(lmerTest)
library(tidyverse)
library(ggplot2)
library(arm)
library(forcats)
library(psych)
library(stringr)
library(dplyr)
library(lmerTest)
library(erpscope)

# set work directory ------------------------------------------------------
setwd('/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_ERP_N400_analysis_v2')

#import data from ERPlab generated dataframe in long format
All_Channal <- read.delim('20230901_KNOC_N400_300ms_500ms_mean_raw.txt')

# rename variable names
All_Channal <- setNames(All_Channal, c("value" = "Activation", 
                                       "chindex" = "ChaNum",
                                       "chlabel" = "Electrode", 
                                       "bini" = "BinNum",
                                       "binlable" = "Condition", 
                                       "ERPset" = "Subject"))
# clean data  -------------------------------------------------------------
# adding two manipulated conditions/variables as columns
Clean_Channal <- All_Channal %>%
  mutate(Familiarity = case_when(
    grepl("H", Condition) ~ "H",
    grepl("L", Condition) ~ "L",
    TRUE ~ NA_character_
  ),
  TruthValue = case_when(
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

Clean_Channal <- data.frame (Clean_Channal) %>%
  filter(REG != "Other")
  subset(Clean_Channal, REG != "Other")

write.csv(Clean_Channal, "clean_data_N400_v2.csv", row.names = TRUE)


# summary ---------------------------------------------------------



# first LMM model ---------------------------------------------------------
# first LMM model for F tests and values for main and interactions effect

LMM1 <- lmer(Activation ~ (Familiarity+TruthValue+REG)^2 + (1|Subject) + (1|Electrode), data = Clean_Channal)
summary(LMM1)
anova(LMM1)
summary(LMM1)$coefficients

# Get the summary output as a character vector
summary_LMM1 <- capture.output(summary(LMM1), anova(LMM1))
write.csv(summary_LMM1,"summary_LMM1_N400_v2.csv")

# t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
# t-test for direction of statistical significance 

# main effect on Truth value ----------------------------------------------

(p_emm1 <-emmeans(LMM1,~TruthValue, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm1<-emmeans(LMM1,~TruthValue, pbkrtest.limit = 7808))
(eff1<-eff_size(emm1, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm1<-confint(p_emm1))
p_emm1<-as.data.frame(p_emm1)
eff1<-as.data.frame(eff1)
output1<-data.frame(p_emm1$contrast,round(p_emm1$estimate,1),round(CI_emm1$lower.CL,1),
                    round(CI_emm1$upper.CL,1),round(p_emm1$p.value,3),round(eff1$effect.size,2))

names(output1)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output1

output1$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))


# main effect on Memory strength ------------------------------------------
(p_emm2 <-emmeans(LMM1,~Familiarity, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm2<-emmeans(LMM1,~Familiarity, pbkrtest.limit = 7808))
(eff2<-eff_size(emm2, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm2<-confint(p_emm2))
p_emm2<-as.data.frame(p_emm2)
eff2<-as.data.frame(eff2)
output2<-data.frame(p_emm2$contrast,round(p_emm2$estimate,1),round(CI_emm2$lower.CL,1),
                    round(CI_emm2$upper.CL,1),round(p_emm2$p.value,3),round(eff2$effect.size,2))

names(output2)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output2

output2$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))

# simple main effect  ---------------------------------------------------










#pairwise_TV_REG <- emmeans(LMM1, pairwise ~ TruthValue:REG, adjust="tukey",pbkrtest.limit = 7808)
#pairwise_TV_REG_summary <- as.data.frame(summary(pairwise_TV_REG, infer = c(TRUE, TRUE), adjust = "sidak"))
#summary_result <- summary(pairwise_TV_REG, infer = c(TRUE, TRUE), adjust = "sidak") 

# P_emm1 <- as.data.frame(summary_result)
# 
# write.csv(summary_result,"summary_LMM1_N400_pairwise.csv")
# 
# pairwise_FA_TV_REG <- emmeans(LMM1, pairwise ~ Familiarity:TruthValue:REG, adjust="tukey",pbkrtest.limit = 7808)
# summary_result_all <- summary(pairwise_FA_TV_REG, infer = c(TRUE, TRUE), adjust = "sidak")
# 
# # Create a table with indication of significance
# pairwise_table <- data.frame(
#   Interaction = c("TruthValue:REG", "Familiarity:REG", "Familiarity:TruthValue", "Familiarity:TruthValue:REG"),
#   Pairwise_Results = c(pairwise_TV_REG_summary$contrasts, pairwise_F_REG_summary$contrasts, pairwise_F_TV_summary$contrasts, pairwise_F_TV_REG_summary$contrasts),
#   p_value = c(pairwise_TV_REG_summary$p.value, pairwise_F_REG_summary$p.value, pairwise_F_TV_summary$p.value, pairwise_F_TV_REG_summary$p.value),
#   Significance = c(ifelse(pairwise_TV_REG_summary$p.value < 0.05, "*", ""), ifelse(pairwise_F_REG_summary$p.value < 0.05, "*", ""), ifelse(pairwise_F_TV_summary$p.value < 0.05, "*", ""), ifelse(pairwise_F_TV_REG_summary$p.value < 0.05, "*", ""))
# )
# 
# ####
# emmeans(LMM1, pairwise ~ Familiarity:REG, adjust="tukey",pbkrtest.limit = 7808)
# emmeans(LMM1, pairwise ~ Familiarity:TruthValue, adjust="tukey",pbkrtest.limit = 7808)
# 
# # Pairwise comparisons for the interaction effect between TruthValue and REG
# pairwise_TV_REG <- emmeans(LMM1, pairwise ~ TruthValue:REG, adjust = "tukey", pbkrtest.limit = 7808)
# pairwise_TV_REG_summary <- as.data.frame(summary(pairwise_TV_REG, infer = c(TRUE, TRUE), adjust = "tukey"))
# 
# # Pairwise comparisons for the interaction effect between Familiarity and REG
# pairwise_F_REG <- emmeans(LMM1, pairwise ~ Familiarity:REG, adjust = "tukey")
# pairwise_F_REG_summary <- as.data.frame(summary(pairwise_F_REG, infer = c(TRUE, TRUE), adjust = "tukey"))
# 
# # Pairwise comparisons for the interaction effect between Familiarity and TruthValue
# pairwise_F_TV <- emmeans(LMM1, pairwise ~ Familiarity:TruthValue, adjust = "tukey")
# pairwise_F_TV_summary <- as.data.frame(summary(pairwise_F_TV, infer = c(TRUE, TRUE), adjust = "tukey"))
# 
# # Pairwise comparisons for the three-way interaction effect between Familiarity, TruthValue, and REG
# pairwise_F_TV_REG <- emmeans(LMM1, pairwise ~ Familiarity:TruthValue:REG, adjust = "tukey")
# pairwise_F_TV_REG_summary <- as.data.frame(summary(pairwise_F_TV_REG, infer = c(TRUE, TRUE), adjust = "tukey"))
# 
# # Create a table with indication of significance
# pairwise_table <- data.frame(
#   Interaction = c("TruthValue:REG", "Familiarity:REG", "Familiarity:TruthValue", "Familiarity:TruthValue:REG"),
#   Pairwise_Results = c(pairwise_TV_REG_summary$contrasts, pairwise_F_REG_summary$contrasts, pairwise_F_TV_summary$contrasts, pairwise_F_TV_REG_summary$contrasts),
#   p_value = c(pairwise_TV_REG_summary$p.value, pairwise_F_REG_summary$p.value, pairwise_F_TV_summary$p.value, pairwise_F_TV_REG_summary$p.value),
#   Significance = c(ifelse(pairwise_TV_REG_summary$p.value < 0.05, "*", ""), ifelse(pairwise_F_REG_summary$p.value < 0.05, "*", ""), ifelse(pairwise_F_TV_summary$p.value < 0.05, "*", ""), ifelse(pairwise_F_TV_REG_summary$p.value < 0.05, "*", ""))
# )
# 
# # Print the pairwise table
# print(pairwise_table)
# # write out the pairwise table
# write.csv(pairwise_TV_REG,"pairwise_table.csv")

##### Compute the estimated marginal means and pairwise comparisons (t-tests)
# emmeans_LMM <- emmeans(LMM1, c("TruthValue", "Familiarity"), type = "response", pbkrtest.limit = 8052, lmerTest.limit = 8052)
# 
# # Get the pairwise comparisons for the interaction effect between REG and TruthValue
# emmeans_interaction_REG_TV <- emmeans(emmeans_LMM, pairwise ~ TruthValue | REG)
# 
# # Get the pairwise comparisons for the interaction effect between REG and Familiarity
# emmeans_interaction_REG_F <- emmeans(emmeans_LMM, pairwise ~ Familiarity | REG)
# 
# # Extract the results table for each comparison
# emmeans_table_REG_TV <- as.data.frame(summary(emmeans_interaction_REG_TV, infer = c(TRUE, TRUE), adjust = "tukey"))
# emmeans_table_REG_F <- as.data.frame(summary(emmeans_interaction_REG_F, infer = c(TRUE, TRUE), adjust = "tukey"))
# 
# # Extract the T-values and Cohen's d from the table for each comparison
# t_values_REG_TV <- emmeans_table_REG_TV[, c("contrast", "df", "t.ratio", "p.value")]
# cohen_d_REG_TV <- emmeans_table_REG_TV[, "effsize"]
# t_values_REG_F <- emmeans_table_REG_F[, c("contrast", "df", "t.ratio", "p.value")]
# cohen_d_REG_F <- emmeans_table_REG_F[, "effsize"]

# Visulazation ERPs -------------------------------------------------------





