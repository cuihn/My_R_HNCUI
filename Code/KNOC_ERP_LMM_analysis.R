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
library(erpscope) #ERP visualization, requires to data specification (raw data) with sample rates (2ms in this case)

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

# simple effect on level of Truth value collapsed on Memory strength and REG----------------------------------------------

(p_emm1 <- emmeans(LMM1,~TruthValue, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm1 <- emmeans(LMM1,~TruthValue, pbkrtest.limit = 7808))
(eff1 <- eff_size(emm1, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm1 <- confint(p_emm1))
p_emm1<-as.data.frame(p_emm1)
eff1<-as.data.frame(eff1)
output1<-data.frame(p_emm1$contrast,round(p_emm1$estimate,1),round(CI_emm1$lower.CL,1),
                    round(CI_emm1$upper.CL,1),round(p_emm1$p.value,3),round(eff1$effect.size,2))

names(output1)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output1
# correct p value
output1$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))

# simple effect on level of Memory strength collapsed on Truth Value and REG ------------------------------------------
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

output2$`p-value`<-ifelse(output2$`p-value`<0.001,"<0.001",round(output2$`p-value`,3))

# simple effect between Truth Value and REG collapsed on memory strength (familiarity)  --------------------------------------------------

(p_emm3 <-emmeans(LMM1,~TruthValue:REG, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm3<-emmeans(LMM1,~TruthValue:REG, pbkrtest.limit = 7808))
(eff3<-eff_size(emm3, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm3<-confint(p_emm3))
p_emm3<-as.data.frame(p_emm3)
eff3<-as.data.frame(eff3)
output3<-data.frame(p_emm3$contrast,round(p_emm3$estimate,1),round(CI_emm3$lower.CL,1),
                    round(CI_emm3$upper.CL,1),round(p_emm3$p.value,3),round(eff3$effect.size,2))

names(output3)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output3

output3$`p-value`<-ifelse(output3$`p-value`<0.001,"<0.001",round(output3$`p-value`,3))

## Effect size
(emm4<-emmeans(LMM1,~Familiarity:REG, pbkrtest.limit = 7808))
(eff4<-eff_size(emm4, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm4<-confint(p_emm4))
p_emm4<-as.data.frame(p_emm4)
eff4<-as.data.frame(eff4)
output4<-data.frame(p_emm4$contrast,round(p_emm4$estimate,1),round(CI_emm4$lower.CL,1),
                    round(CI_emm4$upper.CL,1),round(p_emm4$p.value,3),round(eff4$effect.size,2))

names(output4)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output4

output4$`p-value`<-ifelse(output4$`p-value`<0.001,"<0.001",round(output4$`p-value`,3))


# simple effect between Memory Strength, Truth Value, and REG  --------------------------------------------------

(p_emm5 <-emmeans(LMM1,~Familiarity:TruthValue:REG, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm5<-emmeans(LMM1,~Familiarity:TruthValue:REG, pbkrtest.limit = 7808)) # nolint: infix_spaces_linter.
(eff5<-eff_size(emm5, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm5<-confint(p_emm5))
p_emm5<-as.data.frame(p_emm5)
eff5<-as.data.frame(eff5)
output5<-data.frame(p_emm5$contrast,round(p_emm5$estimate,1),round(CI_emm5$lower.CL,1),
                    round(CI_emm5$upper.CL,1),round(p_emm5$p.value,3),round(eff5$effect.size,2))

names(output5)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output5

output5$`p-value`<-ifelse(output5$`p-value`<0.001,"<0.001",round(output5$`p-value`,3))

# simple effect for testing the interaction between Memory Strength and Truth Value collapsed on REG  --------------------------------------------------

(p_emm6 <-emmeans(LMM1,~Familiarity:TruthValue, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm6<-emmeans(LMM1,~Familiarity:TruthValue, pbkrtest.limit = 7808)) 
(eff6<-eff_size(emm6, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm6<-confint(p_emm6))
p_emm6<-as.data.frame(p_emm6)
eff6<-as.data.frame(eff6)
output6<-data.frame(p_emm6$contrast,round(p_emm6$estimate,1),round(CI_emm6$lower.CL,1),
                    round(CI_emm6$upper.CL,1),round(p_emm6$p.value,3),round(eff6$effect.size,2))

names(output6)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output6

output6$`p-value`<-ifelse(output6$`p-value`<0.001,"<0.001",round(output6$`p-value`,3))


# write out post hoc results in one csv -----------------------------------

write.csv(rbind(output1,output2,output3,output4,output5,output6), "postHoc_N400_results_P_effectSize.csv")
write.csv(rbind(p_emm1,p_emm2,p_emm3,p_emm4,p_emm5,p_emm6), "postHoc_N400_t-test_results.csv")

# Visulazation ERPs -------------------------------------------------------





