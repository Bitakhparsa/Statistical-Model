
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

x <- heights %>% filter(sex == "Male") %>%
  .$height

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  mo <- mean(X)
  z <- qt(0.975,(N-1))
  interval <- c(mo-z*se, mo+z*se)
  mu <- between(mu, mo-z*se, mo+z*se)
})
mean(res)

