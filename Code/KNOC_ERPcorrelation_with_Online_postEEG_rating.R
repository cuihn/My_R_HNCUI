#install.packages("readxl")
#install.packages("lme4")
install.packages("plyr")


# Import libs -------------------------------------------------------------
library(readxl)
library(tidyverse)
library(lmerTest)
library(Matrix)
library(ggpubr)
library(ggplot2)
library(lme4)
library(dplyr)
#library(plyr)

# load files --------------------------------------------------------------
setwd('/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_Beh_ERP_corrrelation_analysis_v2')
file_path <- "/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_Beh_ERP_corrrelation_analysis_v2"

# load N400 data, clean, summarize mean
All_clean_N400 <- read.csv('clean_data_N400_v2.csv')
colnames(All_clean_N400)[8] <- "MemoryStrength" 
colnames(All_clean_N400)[2] <- "Activation_N400" 
All_clean_N400$MemoryStrength[All_clean_N400$MemoryStrength == "H"] <- "Strong"
All_clean_N400$MemoryStrength[All_clean_N400$MemoryStrength == "L"] <- "Weak"
All_clean_N400$TruthValue[All_clean_N400$TruthValue == "FALSE"] <- "False"
All_clean_N400$TruthValue[All_clean_N400$TruthValue == "TRUE"] <- "True"
All_clean_N400$TruthValue <- factor(All_clean_N400$TruthValue, levels = c("False", "True"))
All_clean_N400$MemoryStrength <- factor(All_clean_N400$MemoryStrength, levels = c("Strong", "Weak"))
# summarize the mean and sum of 'Activation_N400' by 'Subjects'
summary_mean_N400 <- All_clean_N400 %>%
  group_by(Subject, TruthValue, MemoryStrength) %>%
  summarize(mean_activation_N400 = mean(Activation_N400))

# load LPC data, clean, summarize mean
All_clean_LPC <- read.csv('clean_data_LPC_v2.csv')
colnames(All_clean_LPC)[8] <- "MemoryStrength" 
colnames(All_clean_LPC)[2] <- "Activation_LPC" 
All_clean_LPC$MemoryStrength[All_clean_LPC$MemoryStrength == "H"] <- "Strong"
All_clean_LPC$MemoryStrength[All_clean_LPC$MemoryStrength == "L"] <- "Weak"
All_clean_LPC$TruthValue[All_clean_LPC$TruthValue == "FALSE"] <- "False"
All_clean_LPC$TruthValue[All_clean_LPC$TruthValue == "TRUE"] <- "True"
All_clean_LPC$TruthValue <- factor(All_clean_LPC$TruthValue, levels = c("False", "True"))
All_clean_LPC$MemoryStrength <- factor(All_clean_LPC$MemoryStrength, levels = c("Strong", "Weak"))

# summarize the mean and sum of 'Activation_LPC' by 'Subjects'
summary_mean_LPC <- All_clean_LPC %>%
  group_by(Subject, TruthValue, MemoryStrength) %>%
  summarize(mean_activation_LPC = mean(Activation_LPC))

# #loading postEEG rating file
# postEEG_ratings <- read.delim("KNOC_online_post_rating_ave_ForCorrelate.txt")

# load online behavior rating, clean data, and checking missing val --------
online_ratings <- read.delim("KNOC_EEG_beha_online_neutreal_all2.txt")
online_ratings$TV[online_ratings$TV == "FF"] <- "False"
online_ratings$TV[online_ratings$TV == "TT"] <- "True"
online_ratings$KT[online_ratings$KT == "H"] <- "Strong"
online_ratings$KT[online_ratings$KT == "L"] <- "Weak"
online_ratings$TV <- factor(online_ratings$TV, levels = c("False", "True"))
online_ratings$KT <- factor(online_ratings$KT, levels = c("Strong", "Weak"))
print(is_factor <- is.factor(online_ratings$KT))
print(is_factor <- is.factor(online_ratings$TV))
colnames(online_ratings)[23] <- "TruthValue" 
colnames(online_ratings)[25] <- "MemoryStrength" 
# summary of mean
online_ratings_1 <- ddply(online_ratings,.(TruthValue,MemoryStrength),summarise, val = mean(Recoded_Response))

# calculate the mean and sum of 'Value' by 'Subjects'
summary_mean_beha <- online_ratings %>%
  group_by(Subject, TruthValue, MemoryStrength) %>%
  summarize(mean_rating = mean(Recoded_Response))
# rename subject columns 
summary_mean_beha$Subject <- gsub("sub0", "", as.character((summary_mean_beha$Subject)))
summary_mean_beha$Subject <- gsub("sub", "", as.character((summary_mean_beha$Subject)))
summary_mean_beha$Subject <- as.numeric(summary_mean_beha$Subject)
# Print the summarized data
print(summary_data)

# merge_ERP_beha_mean ----------------------------------------------------
summary_mean_list <- list(summary_mean_beha, summary_mean_N400, summary_mean_LPC) %>% 
  reduce(full_join, by= c("Subject", "MemoryStrength", "TruthValue"))  
summary_mean_all <- data.frame(summary_mean_list)

# Create the boxplot with mean and SD ------------------------------------
graph4 <- ggplot(online_ratings,
                aes(x=MemoryStrength, y=Recoded_Response, fill=interaction(TruthValue, MemoryStrength))) +
  geom_violin(trim = TRUE)+
  facet_grid(~TruthValue) +
  theme_classic() +
  geom_line(data = online_ratings_1, aes(y = val, group = MemoryStrength))

graph4 <- graph4 +stat_summary(fun=mean, geom="point",
                     shape=15, color="black",
                     fill="black")

print(graph4)

# perform LMM on the online ratings scales --------------------------------
# Set "false" as the reference level for "TruthValue"
online_ratings$TruthValue <- relevel(online_ratings$TruthValue, ref = "False")

# Set "Weak" as the reference level for "MemoryStrength"
online_ratings$MemoryStrength <- relevel(online_ratings$MemoryStrength, ref = "Weak")

lmm <- lmer(Recoded_Response ~ 1 + TruthValue + MemoryStrength +(1|Subject) + (1|Item), data = online_ratings, REML=F)

lmm1 <- lmer(Recoded_Response ~1 +  TruthValue:MemoryStrength + (1|Subject) + (1|Item), data = online_ratings,REML=F)

lmm2 <- lmer(Recoded_Response ~ 1 + TruthValue + (1|Subject) + (1|Item), data = online_ratings, REML=F)

lmm3 <- lmer(Recoded_Response ~1 +  MemoryStrength + (1|Subject) + (1|Item), data = online_ratings,REML=F)

# Get the summary output as a character vector
summary_lmm <- capture.output(summary(lmm), anova(lmm))
print(summary_lmm)
summary_lmm1 <- capture.output(summary(lmm1), anova(lmm1))
print(summary_lmm1)
summary_lmm2 <- capture.output(summary(lmm2), anova(lmm2))
summary_lmm3 <- capture.output(summary(lmm3), anova(lmm3))
write.csv(summary_lmm,"summary_LMM_beh_main.csv")
write.csv(summary_lmm1,"summary_LMM_beh_interaction.csv")

# t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
# t-test for direction of statistical significance 
# simple effect on level of Truth value collapsed on Memory strength (knowledge type) ----------------------------------------------
library(emmeans)
(p_emm1 <- emmeans(lmm,~KT:TV, pbkrtest.limit = 4578) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm1 <- emmeans(lmm,~TV:KT, pbkrtest.limit = 4578))
#(eff1 <- eff_size(emm1, sigma = sigma(lmm), edf = df.residual(lmm)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)
## Output
CI_emm1 <- confint(p_emm1)
p_emm1<-as.data.frame(p_emm1)
#eff1<-as.data.frame(eff1)

output1<-data.frame(p_emm1$contrast,round(p_emm1$estimate,1),round(CI_emm1$lower.CL,1),
                    round(CI_emm1$upper.CL,1),round(p_emm1$p.value,3))

names(output1)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value")
output1
# correct p value
output1$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))

write.csv(output1, "postHoc_beh_results_P_effectSize.csv")
write.csv(p_emm1, "postHoc_beh_t-test_results.csv")


# perform correlation test on the online ratings and post EEG rating --------

Beha_strong_true <- summary_mean_all$mean_rating[summary_mean_all$TruthValue == "True" & summary_mean_all$MemoryStrength == "Strong"]
N400_strong_true <- summary_mean_all$mean_activation_N400[summary_mean_all$TruthValue == "True" & summary_mean_all$MemoryStrength == "Strong"]
LPC_strong_true <- summary_mean_all$mean_activation_LPC[summary_mean_all$TruthValue == "True" & summary_mean_all$MemoryStrength == "Strong"]

correlate_Beha_N400_strong_true <- corr.test(Beha_strong_true, N400_strong_true, adjust = 'none')
correlate_Beha_LPC_strong_true <- corr.test(Beha_strong_true, LPC_strong_true, adjust = 'none')

# perform correlation test on the online ratings and post EEG rating --------
Beha_weak_true <- summary_mean_all$mean_rating[summary_mean_all$TruthValue == "True" & summary_mean_all$MemoryStrength == "Weak"]
N400_weak_true <- summary_mean_all$mean_activation_N400[summary_mean_all$TruthValue == "True" & summary_mean_all$MemoryStrength == "Weak"]
LPC_weak_true <- summary_mean_all$mean_activation_LPC[summary_mean_all$TruthValue == "True" & summary_mean_all$MemoryStrength == "Weak"]

correlate_Beha_N400_weak_true <- corr.test(Beha_weak_true, N400_weak_true, adjust = 'none')
correlate_Beha_LPC_weak_true <- corr.test(Beha_weak_true, LPC_weak_true, adjust = 'none')


# perform correlation test on the online ratings and post EEG rating --------
Beha_strong_false <- summary_mean_all$mean_rating[summary_mean_all$TruthValue == "False" & summary_mean_all$MemoryStrength == "Strong"]
N400_strong_false <- summary_mean_all$mean_activation_N400[summary_mean_all$TruthValue == "False" & summary_mean_all$MemoryStrength == "Strong"]
LPC_strong_false <- summary_mean_all$mean_activation_LPC[summary_mean_all$TruthValue == "False" & summary_mean_all$MemoryStrength == "Strong"]

correlate_Beha_N400_strong_false <- corr.test(Beha_strong_false, N400_strong_false, adjust = 'none')
correlate_Beha_LPC_strong_false <- corr.test(Beha_strong_false, LPC_strong_false, adjust = 'none')

# perform correlation test on the online ratings and post EEG rating --------
Beha_weak_false <- summary_mean_all$mean_rating[summary_mean_all$TruthValue == "False" & summary_mean_all$MemoryStrength == "Weak"]
N400_weak_false <- summary_mean_all$mean_activation_N400[summary_mean_all$TruthValue == "False" & summary_mean_all$MemoryStrength == "Weak"]
LPC_weak_false <- summary_mean_all$mean_activation_LPC[summary_mean_all$TruthValue == "False" & summary_mean_all$MemoryStrength == "Weak"]

correlate_Beha_N400_weak_false <- corr.test(Beha_weak_false, N400_weak_false, adjust = 'none')
correlate_Beha_LPC_weak_false <- corr.test(Beha_weak_false, LPC_weak_false, adjust = 'none')
cor(Beha_weak_false, LPC_weak_false)

# # perform correlation test on the postEEG ratings and averaged ERP response --------
# 
# corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')
# 
# cor(factor_data[, 1:4], IAT_Dscore$Dscore)




