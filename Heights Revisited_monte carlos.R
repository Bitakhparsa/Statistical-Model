# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height




# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu



res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  mo <- mean(X)
  z <- qnorm(1-(1-0.95)/2)
  interval <- c(mo-z*se, mo+z*se)
  mu <- between(mu, mo-z*se, mo+z*se)
})
res






# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)
