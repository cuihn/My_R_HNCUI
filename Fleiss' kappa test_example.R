# chatGPT Generated example data for Fleiss' kappa test 


# set.seed(123)
# ratings <- data.frame(
#   participant = rep(1:10, each = 5),
#   item = rep(1:5, times = 10),
#   rating = sample(1:7, size = 50, replace = TRUE)
# )
# 
# # Check for negatively correlated items
# psych::alpha(ratings, check.keys = TRUE)
# 
# # Calculate overall variability of ratings
# overall_variability <- var(ratings$rating)
# overall_variability
# 
# # Assess reliability of rating scale
# library(psych)
# reliability <- alpha(ratings)
# reliability$total
