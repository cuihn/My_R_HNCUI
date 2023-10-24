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
library(plyr)

# load files --------------------------------------------------------------

setwd('/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_Beh_ERP_corrrelation_analysis_v2')
file_path <- "/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_Beh_ERP_corrrelation_analysis_v2"

# # load ERP data N400, LPC ERP files
# All_clean_N400 <- read.csv('clean_data_N400_v2.csv')
# All_clean_LPC <- read.delim('clean_data_LPC_v2.csv')
# #loading postEEG rating file
# postEEG_ratings <- read.delim("KNOC_online_post_rating_ave_ForCorrelate.txt")

# load online behavior rating, clean data, and checking missing val --------
online_ratings <- read.delim("KNOC_EEG_beha_online_neutreal_all2.txt")
#online_ratings$TV[online_ratings$TV == "WRONG"] <- "F"
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

# Remove rows with missing values
#online_ratings <- online_ratings[complete.cases(online_ratings$Recoded_Response), ]
#online_ratings <- na.omit(online_ratings)

# # Create a logical matrix indicating missing values
# missing_matrix <- is.na(online_ratings)
# print(missing_matrix)
# missing_count <- colSums(missing_matrix)
# print(missing_count)
# any_missing <- any(is.na(online_ratings))

# Create the box plot with mean and SD ------------------------------------
# plotting the data frame
graph4 <- ggplot(online_ratings,
                aes(x=MemoryStrength, y=Recoded_Response, fill=interaction(TruthValue, MemoryStrength))) +
  #geom_violin(trim = FALSE) +
  geom_violin(trim = TRUE)+
  facet_grid(~TruthValue) +
  theme_classic() +
  #geom_line()
  geom_line(data = online_ratings_1, aes(y = val, group = MemoryStrength))

graph4 <- graph4 +stat_summary(fun=mean, geom="point",
                     shape=15, color="black",
                     fill="black")

print(graph4)

# Add p-value
graph4 + stat_compare_means()

# perform LMM on the online ratings scales --------------------------------
# Set "T" as the reference level for "TV"
online_ratings$TruthValue <- relevel(online_ratings$TruthValue, ref = "False")

# Set "H" as the reference level for "KT"
online_ratings$MemoryStrength <- relevel(online_ratings$MemoryStrength, ref = "Weak")

#lmm <- lmer(Recoded_Response ~ 1 + TV + KT +(1|Subject) + (1|Item), data = online_ratings, REML=F)

lmm1 <- lmer(Recoded_Response ~1 +  TruthValue:MemoryStrength + (1|Subject) + (1|Item), data = online_ratings,REML=F)

# lmm2 <- lmer(Recoded_Response ~ 1 + TV + (1|Subject) + (1|Item), data = online_ratings, REML=F)
# lmm3 <- lmer(Recoded_Response ~1 +  KT + (1|Subject) + (1|Item), data = online_ratings,REML=F)


# Get the summary output as a character vector
#summary_lmm <- capture.output(summary(lmm), anova(lmm))
summary_lmm1 <- capture.output(summary(lmm1), anova(lmm1))
#summary_lmm2 <- capture.output(summary(lmm2), anova(lmm2))
#summary_lmm3 <- capture.output(summary(lmm3), anova(lmm3))
#write.csv(summary_lmm,"summary_LMM_beh_main.csv")
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


# perform correlation test on the online ratings and post EEG rati --------

corr.test(factor_data[, 1:4], Posttest[,6:11], adjust = 'none')

# perform correlation test on the online ratings and averaged ERP scores --------

corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')

cor(factor_data[, 1:4], IAT_Dscore$Dscore)

# perform correlation test on the postEEG ratings and averaged ERP scores --------

corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')

cor(factor_data[, 1:4], IAT_Dscore$Dscore)


# Create a new variable to represent the combination of Truth_value and Knowledge_type
online_ratings$conditions <- paste(online_ratings$TV, online_ratings$KT, sep = "_")

# Set the order of the bars
bar_order <- c("True_Highly Known", "True_Lower Known", "False_Highly Known", "False_Lower Known")

# Create a count of each combination of Group and Recoded_Response
count_data <- data.frame(table(online_ratings$conditions, online_ratings$Recoded_Response))

# Calculate the average recoded response for each combination of Knowledge_type and Truth_value
avg_data <- aggregate(Recoded_Response ~ KT + TV, data = online_ratings, FUN = mean)


# Rename the count column
colnames(count_data) <- c("Conditions", "Recoded_Response", "Count")

# Convert Recoded_Response to factor for correct ordering
count_data$Recoded_Response <- factor(count_data$Recoded_Response, levels = 1:5)

# Remove rows with missing values
count_data <- count_data[complete.cases(count_data), ]






