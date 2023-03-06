# create a sample data frame
df <- data.frame(Var1 = c("FP1", "AF8", "F3", "OZ", "FD5"), Var2 = c(1,2,3,4,5))

# define the string patterns to be searched for
patterns <- c("FP", "FD")

# create new variables for each pattern
for (p in patterns) {
  df <- df %>%
    mutate(!!paste0("has_", p) := ifelse(grepl(p, Var1), TRUE, FALSE))
}

# view the updated data frame
df