#install.packages("readxl")
#install.packages("lme4")
#install.packages("ggpubr")


# Import libs -------------------------------------------------------------
library(readxl)
library(tidyverse)
library(lmerTest)
library(Matrix)
library(ggpubr)
library(ggplot2)
library(lme4)

# load files --------------------------------------------------------------

setwd('/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_ERP_LPC_analysis_v2')

# load ERP data N400, LPC ERP files
All_clean_N400 <- read.delim('20230901_KNOC_ERP_LPC_600ms_1000ms_raw.txt')
All_clean_LPC <- read.delim('20230901_KNOC_ERP_LPC_600ms_1000ms_raw.txt')

#load_behavior_online_rating
online_ratings <- read_excel(file.path(file_path,"KNOC_Behav_for_CorrelationAnalysis.xlsx"))

#loading postEEG rating file

postEEG_ratings <- read_excel(file.path(file_path,"KNOC_Behav_for_CorrelationAnalysis.xlsx"))

# clean files -------------------------------------------------------------
ratings <- ratings %>%
  select(Recoded_Response, Truth_value, Knowledge_type, Subject, Items)

# perform LMM on the online ratings scales --------------------------------

lmm <- lmer(Recoded_Response ~ Truth_value * Knowledge_type + (1|Subject) + (1|Items), data = ratings)
lmm_summary_beh <- summary(lmm)

p_values <- lmm_summary_beh$coefficients[, "Pr(>|t|)"]
output <- capture.output(lmm_summary_beh)

writeLines(file.path(file_path, "lmm_summary_beha_rating.txt"))

# t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
# t-test for direction of statistical significance 

# simple effect on level of Truth value collapsed on Memory strength and REG----------------------------------------------

(p_emm1 <- emmeans(lmm_summary_beh,~Truth_Value:Knowledge_type, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm1 <- emmeans(lmm_summary_beh,~Truth_Value:Knowledge_type, pbkrtest.limit = 7808))
(eff1 <- eff_size(emm1, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
CI_emm1 <- confint(p_emm1)
p_emm1<-as.data.frame(p_emm1)
eff1<-as.data.frame(eff1)

output1<-data.frame(p_emm1$contrast,round(p_emm1$estimate,1),round(CI_emm1$lower.CL,1),
                    round(CI_emm1$upper.CL,1),round(p_emm1$p.value,3),round(eff1$effect.size,2))

names(output1)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output1

# correct p value
output1$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))


# perform correlation test on the online ratings and post EEG rati --------

corr.test(factor_data[, 1:4], Posttest[,6:11], adjust = 'none')

# perform correlation test on the online ratings and averaged ERP scores --------

corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')

cor(factor_data[, 1:4], IAT_Dscore$Dscore)

# perform correlation test on the postEEG ratings and averaged ERP scores --------

corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')

cor(factor_data[, 1:4], IAT_Dscore$Dscore)




