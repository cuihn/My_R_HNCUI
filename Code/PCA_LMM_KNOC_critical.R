# R script for KNOC neutral condition PCA_LMM analysis 


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
library(pbkrtest)



# 1 -----------------------------------------------------------------------


### 1. TF1SF1 - none sig.

# Model at peak electrode (Cz)
# Dataset Name: tempSpa2_critical
# -178--176 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF1SF1 (1)

factor_data <- read.delim("tempSpa2_critical-TF1SF1.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM_1 <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test_1 <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF1SF1.txt")
summary(PCA_LMM_1)
sink(file = NULL)

sink(file = "post_output_TF1SF1.txt")
summary(post_test_1)
sink(file = NULL)


# 2 -----------------------------------------------------------------------

### 2. TF1SF2 - 

# Model at peak electrode (AF7)
# Dataset Name: tempSpa2_critical
# -178--176 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF1SF2 (2)

factor_data <- read.delim("tempSpa2_critical-TF1SF2.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF1SF2.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF1SF2.txt")
summary(post_test)
sink(file = NULL)


# 3 -----------------------------------------------------------------------

### 3. TF2SF1 - 

# Model at peak electrode (C2)
# Dataset Name: tempSpa2_critical
# 976-978 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF2SF1 (3)

factor_data <- read.delim("tempSpa2_critical-TF2SF1.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF2SF1.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF2SF1.txt")
summary(post_test)
sink(file = NULL)


# 4 -----------------------------------------------------------------------

### 4. TF2SF2 - 

# Model at peak electrode (AF7)
# Dataset Name: tempSpa2_critical
# 976-978 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF2SF2 (4)

factor_data <- read.delim("tempSpa2_critical-TF2SF2.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF2SF2.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF2SF2.txt")
summary(post_test)
sink(file = NULL)


# 5 -----------------------------------------------------------------------


### 5. TF3SF1 - 

# Model at peak electrode (C4)
# Dataset Name: tempSpa2_critical
# 730-732 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF3SF1 (5)

factor_data <- read.delim("tempSpa2_critical-TF3SF1.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF3SF1.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF3SF1.txt")
summary(post_test)
sink(file = NULL)


# 6 -----------------------------------------------------------------------


### 6. TF3SF2 - 

# Model at peak electrode (AF7)
# Dataset Name: tempSpa2_critical
# 730-732 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF3SF2 (6)

factor_data <- read.delim("tempSpa2_critical-TF3SF2.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF3SF2.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF3SF2.txt")
summary(post_test)
sink(file = NULL)


# 7 -----------------------------------------------------------------------


### 7. TF4SF1 - 

# Model at peak electrode (AF7)
# Dataset Name: tempSpa2_critical
# 1434-1436 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF4SF1 (7)

factor_data <- read.delim("tempSpa2_critical-TF4SF1.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF4SF1.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF4SF1.txt")
summary(post_test)
sink(file = NULL)


# 8 -----------------------------------------------------------------------


### 8. TF4SF2 - 

# Model at peak electrode (Fp2)
# Dataset Name: tempSpa2_critical
# 1434-1436 ms
# One sample of voltage.
# Channel Group: autoPCA
# Factor: TF4SF2 (8)

factor_data <- read.delim("tempSpa2_critical-TF4SF2.txt")
factor <- factor_data  %>% 
  gather("condition", "loading", 1:4) %>%
  separate(condition, c("Farmilarity", "TruthValue"), sep = 1) %>%
  mutate(TruthValue = fct_relevel(TruthValue, "T"))

PCA_LMM <- lmer(loading ~ Farmilarity*TruthValue + (1|subject), factor)
summary(PCA_LMM)

post_test <- emmeans(PCA_LMM, pairwise~Farmilarity*TruthValue, adjust="tukey")
summary(post_test)

# save output (LMM, post-hoc results) to txt. files
sink(file = "lmm_output_TF4SF2.txt")
summary(PCA_LMM)
sink(file = NULL)

sink(file = "post_output_TF4SF2.txt")
summary(post_test)
sink(file = NULL)