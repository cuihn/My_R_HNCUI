#install.packages("readxl")
#install.packages("lme4")
install.packages("ggpubr")


library(readxl)
library(tidyverse)
library(lmerTest)
library(Matrix)
library(ggpubr)
library(ggplot2)
#library(lme4)

file_path <- "C:\\Users\\hcui8\\Dropbox\\Trying\\EEG_KNOC_N400"

# load ERP data N400
ERP_LPP_amplitude <- read.csv(file.path(file_path, "clean_data_lateEffect.csv"))

#load_behavior_online_rating
ratings <- read_excel(file.path(file_path,"KNOC_Behav_for_CorrelationAnalysis.xlsx"))
colnames(ratings)[colnames(ratings) == 'Presentation trial number'] <- "Items"
colnames(ratings)[colnames(ratings) == 'Truth Value'] <- "Truth_value"
colnames(ratings)[colnames(ratings) == 'Knowledge Type'] <- "Knowledge_type"

ratings <- ratings %>%
  select(Recoded_Response, Truth_value, Knowledge_type, Subject, Items)



#########

#perform LMM on the ratings scales
lmm <- lmer(Recoded_Response ~ Truth_value * Knowledge_type + (1|Subject) + (1|Items), data = ratings)
lmm_summary <- summary(lmm)

p_values <- lmm_summary$coefficients[, "Pr(>|t|)"]
output <- capture.output(lmm_summary)

writeLines(file.path(file_path, "lmm_summary_Rating.txt"))


# Create a new variable to represent the combination of Truth_value and Knowledge_type
ratings$conditions <- paste(ratings$Truth_value, ratings$Knowledge_type, sep = "_")

# Set the order of the bars
bar_order <- c("True_Highly Known", "True_Lower Known", "False_Highly Known", "False_Lower Known")

# Create a count of each combination of Group and Recoded_Response
count_data <- data.frame(table(ratings$conditions, ratings$Recoded_Response))

# Calculate the average recoded response for each combination of Knowledge_type and Truth_value
avg_data <- aggregate(Recoded_Response ~ Knowledge_type + Truth_value, data = ratings, FUN = mean)


# Rename the count column
colnames(count_data) <- c("Conditions", "Recoded_Response", "Count")

# Convert Recoded_Response to factor for correct ordering
count_data$Recoded_Response <- factor(count_data$Recoded_Response, levels = 1:5)

# Remove rows with missing values
count_data <- count_data[complete.cases(count_data), ]

# Create the bar plot
ggplot(avg_data, aes(x = Knowledge_type, y = Recoded_Response, fill = interaction(Knowledge_type, Truth_value))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Knowledge Type", y = "Average Recoded Response", fill = "Interaction") +
  scale_fill_manual(values = c("blue", "red", "green", "orange")) +
  scale_y_continuous(limits = c(0, 5), breaks = 1:5) +
  theme_minimal()


# Summarize the mean and standard deviation of rating scores
avg_scores <- ratings %>%
  group_by(Knowledge_type, Truth_value) %>%
  summarise(
    avg_rate = mean(Recoded_Response, na.rm = TRUE),
    sd_rate = sd(Recoded_Response, na.rm = TRUE),
    na.rm = TRUE
  )

# Print the resulting dataframe
avg_scores

# Create the box plot with mean and SD

ggplot(ratings, aes(x = Knowledge_type, y = Recoded_Response, fill = interaction(Knowledge_type, Truth_value))) +  geom_boxplot() + facet_grid(~Truth_value) 

ggplot(ratings, aes(x = Knowledge_type, y = Recoded_Response, fill =  Knowledge_type)) +  geom_boxplot(notch=TRUE) + facet_grid(~Truth_value) 

######












# # Summarize the mean and standard deviation of rating scores
# avg_scores <- ratings %>%
#   group_by(Knowledge_type, Truth_value) %>%
#   summarise(
#     avg_rate = mean(Recoded_Response, na.rm = TRUE),
#     sd_rate = sd(Recoded_Response, na.rm = TRUE),
#     na.rm = TRUE
#   )
# 
# # Print the resulting dataframe
# avg_scores
# 
# # Calculate mean and standard deviation
# summary_data <- ave_scores %>%
#   group_by(Condition) %>%
#   summarise(
#     Mean = mean(Recoded_Response),
#     SD = sd(Recoded_Response),
#     .groups = "drop"
#   )
# 


# #summarize the mean of ERP amplitudes
# ave_activation <- ERP_LPP_amplitude %>%
#   group_by(Subject, Condition) %>%
#   summarise(ave_activation = mean (Activation))
# 
# # merge ave_scores and ave_activation into one df for later correlation analysis
# merged_df <- left_join(ave_scores, ave_activation, by = c("Subject", "Condition"))




#correlation between Behavior and amplitudes
conditions <- unique(merged_df$Condition)

cor_results <- list()

for (condition in conditions) {
  subset_data <- merged_df[merged_df$Condition == condition, ]
  cor_test <- cor.test(subset_data$ave_scores, subset_data$ave_activation, method = "pearson")
  
  cor_results[[condition]] <- list(
    Correlation = cor_test$estimate,
    P_Value = cor_test$p.value,
    Significance = ifelse(cor_test$p.value < 0.05, "Significant", "Not Significant")
  )
}

# Print and write out the results
for (condition in conditions) {
  print(paste("Correlation for", condition, ":", cor_results[[condition]]$Correlation))
  print(paste("P-value for", condition, ":", cor_results[[condition]]$P_Value))
  print(paste("Significance for", condition, ":", cor_results[[condition]]$Significance))
}

result <- data.frame(
  Condition = conditions,
  Correlation = sapply(cor_results, function(x) x$Correlation),
  P_Value = sapply(cor_results, function(x) x$P_Value),
  Significance = sapply(cor_results, function(x) x$Significance)
)

write.csv(result, file = "correlation_LPP_results.csv", row.names = FALSE)


