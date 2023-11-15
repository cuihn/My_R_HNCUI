library(dplyr)
library(magrittr) # pip operation package
library(readr)

# import

df1 <- read.csv("/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/Stimuli/Real_order_post.csv")

df2 <- read.csv("/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/Stimuli/Answer_key_post.csv")

# sort 

merged_df <- merge(df1, df2, by  = c("Con_Num"), all.x = TRUE, all.y = TRUE )

write.csv(merged_df,"/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/Stimuli/Merged_key_condition_v2.csv")



