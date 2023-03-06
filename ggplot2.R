# install library
# chatgpt code example
install.packages("farver")
library(ggplot2)
# create example data

set.seed(123)
df <- data.frame(x = rnorm(50, 3, 1), y = rnorm(50, 2, 0.5))

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
  scale_y_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
  labs(title = "Scatter plot with 1-5 scale", x = "X Variable (1-5)", y = "Y Variable (1-5)")

### ggplot2 example

library(ggplot2)
df
# excise 2.2
#data(package = "ggplot2")

# excise 2.3
print (mpg["cty"])
df$cty

df$cty_10km_city <- 235.215 / mpg$cty
df$hwy_10km_high <- 235.215 / mpg$hwy

head(df)

#excise 2.4

table(df$manufacturer)
