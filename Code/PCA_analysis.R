
# Analysis of ERP PCA data from a complaint perception experiment




# install.packages('lme4')
library(lme4)
# install.packages('lsmeans')
library(emmeans)
# install.packages("lmerTest")
library("lmerTest")
# install.packages("tidyverse")
library(tidyverse)
# install.packages("ggplot2")
library(ggplot2)
library(arm)
library(forcats)
library(psych)




# #Read Post-test
# Posttest <- read.csv('EEG Post-test summary.csv')
# 
# 
# #Read IAT
# IAT_Results <- read.csv("Results_IAT.csv")
# IAT_Results <- IAT_Results %>% filter(Code != "E", ReactionTime > 5, ReactionTime < 5000)
# IAT_Dscore <- IAT_Results %>%
#   mutate(Block = recode(Block, "BlockA3 - IAT task" = "A", "Block B3 - IAT task (reversed)" = "B")) %>%
#   spread(Block, ReactionTime) %>%
#   group_by(Participant) %>%
#   summarize(Dscore = (mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE))/((sd(A, na.rm = TRUE) + sd(B, na.rm = TRUE))/2))
# 
# 
# # Read behavior
# 
# Behav_results = read.csv("Participant_behavior.csv")
# 
# 
# 
# ### TF1SF1 / Not significant
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF1SF1.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# 
# 
# ### TF1SF2 / Not significant
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF1SF2.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# 
# 
# 
# 
# 
# ### TF2SF1 - P200
# 
# Model at peak electrode (Cz)
factor_data <- read.delim("Critical-TF02SF1.txt")

factor <- factor_data  %>%
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Familarity", "TruthValue"), sep = 1)%>%
  mutate(Prosody = fct_relevel(TruthValue, "N"))

PCA_LMM <- lmer(loading ~ Familarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

emmeans(PCA_LMM, pairwise~Familarity|TruthValue, adjust="tukey")
# 
# 
# 
# corr.test(factor_data[, 1:4], Posttest[,6:11], adjust = 'none')
# 
# corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')
# 
# cor(factor_data[, 1:4], IAT_Dscore$Dscore)



# model including all channels
# Only used for post-hoc comparison per electrode

factor_data_all <- read.delim('mean_amplitude_N400_neutreal_edit.txt')

factor <- factor_data_all  
  gather("condition", "loading", 1:252) %>%
  separate(condition, c("Familarity", "TruthValue", "Electrode"), sep = c(1,2)) %>%
  mutate(TF = fct_relevel(TruthValue, "N")) %>%

PCA_LMM <- lmer(loading ~ Familarity*TruthValue*Electrode + (1|Sub), factor_all)

summary(PCA_LMM)

emmeans(PCA_LMM, pairwise ~ TruthValue*TruthValue|Electrode, adjust="tukey")


# ### TF2SF2 - EPN
# 
# # Model at peak electrode (POz)
# factor_data <- read.delim("CESC_PCA_Onset-TF2SF2_POz.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
#   
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Accent|Prosody, adjust="tukey")
# 
# 
# corr.test(factor_data[, 1:4], Posttest[,6:11], adjust = 'none')
# corr.test(factor_data[, 1:4], IAT_Dscore$Dscore)
# corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')
# 
# 
# # model including all channels
# # Only used for post-hoc comparison per electrode
# factor_data_all <- read.delim('CESC_PCA_Onset-TF2SF2_all.txt')
# 
# factor_all <- factor_data_all  %>% 
#   gather("condition", "loading", 1:260) %>%
#   separate(condition, c("Accent", "Prosody", "Electrode"), sep = c(1,2)) %>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody*Electrode + (1|Subject), factor_all)
# summary(PCA_LMM)
# emmeans(PCA_LMM, pairwise~Prosody*Accent|Electrode, adjust="tukey")
# 
# 
# 
# 
# ### TF3SF1 / Not significant
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF3SF1.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Prosody|Accent, adjust="tukey")
# 
# 
# 
# 
# ### TF3SF2 / Not significant
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF3SF2.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# 
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Prosody|Accent, adjust="tukey")
# 
# 
# 
# 
# 
# ### TF4SF1 / Not significant
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF4SF1.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# 
# 
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Accent|Prosody, adjust="tukey")
# 
# 
# 
# 
# 
# ### TF4SF2 - N400
# 
# # Model at peak electrode (P2)
# factor_data <- read.delim("CESC_PCA_Onset-TF4SF2_P2.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# 
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Accent|Prosody, adjust="tukey")
# 
# corr.test(factor_data[, 1:4], Posttest[,6:11], adjust = 'none')
# corr.test(factor_data[, 1:4], IAT_Dscore$Dscore)
# corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')
# 
# 
# 
# # model including all channels
# # Only used for post-hoc comparison per electrode
# factor_data_all <- read.delim('CESC_PCA_Onset-TF4SF2_all.txt')
# 
# factor_all <- factor_data_all  %>% 
#   gather("condition", "loading", 1:260) %>%
#   separate(condition, c("Accent", "Prosody", "Electrode"), sep = c(1,2)) %>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody*Electrode + (1|Subject), factor_all)
# summary(PCA_LMM)
# emmeans(PCA_LMM, pairwise~Prosody*Accent|Electrode, adjust="tukey")
# 
# 
# 
# 
# ### TF5SF1 - Late negativity
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF5SF1.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Prosody|Accent, adjust="tukey")
# 
# cor(factor_data[, 1:4], Posttest[,6:11])
# cor(factor_data[, 1:4], IAT_Dscore$Dscore)
# corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')
# 
# 
# 
# # model including all channels
# # Only used for post-hoc comparison per electrode
# factor_data_all <- read.delim('CESC_PCA_Onset-TF5SF1_all.txt')
# 
# factor_all <- factor_data_all  %>% 
#   gather("condition", "loading", 1:260) %>%
#   separate(condition, c("Accent", "Prosody", "Electrode"), sep = c(1,2)) %>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody*Electrode + (1|Subject), factor_all)
# summary(PCA_LMM)
# emmeans(PCA_LMM, pairwise~Prosody*Accent|Electrode, adjust="tukey")
# 
# 
# 
# 
# 
# ### TF5SF2 / Not significant
# 
# factor_data <- read.delim("CESC_PCA_Onset-TF5SF2.txt")
# 
# factor <- factor_data  %>% 
#   gather("condition", "loading", 1:4) %>%
#   separate(condition, c("Accent", "Prosody"), sep = 1)%>%
#   mutate(Prosody = fct_relevel(Prosody, "N"))
# 
# 
# 
# PCA_LMM <- lmer(loading ~ Accent*Prosody + (1|Subject), factor)
# summary(PCA_LMM)
# 
# emmeans(PCA_LMM, pairwise~Prosody|Accent, adjust="tukey")
# 
# 
# 
# 
# 
# 
# 
# #### Plot of temporal factor loadings
# 
# TFloadings <- read.delim('TFloadings.txt', header = FALSE)
# TFloadings <- TFloadings %>%
#   mutate(Time = seq(-200,1198,2))
# 
# TFloadings %>%
#   ggplot(aes(x = Time)) +
#   geom_line(aes(y = V2), color = 'blue', size = 1) +
#   geom_line(aes(y = V4), color = 'red', size = 1) +
#   geom_line(aes(y= V5), size = 1)+
#   geom_hline(aes(yintercept=0))+
#   geom_vline(aes(xintercept = 0))+
#   theme_minimal()+
#   scale_x_continuous(breaks = seq(-200, 1198, 200), expand = c(0,0))+
#   labs(x = "Time(ms)", y = "Amplitude")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"))
# 
#             