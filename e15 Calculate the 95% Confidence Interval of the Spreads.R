# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster )%>% summarise(ave=mean(spread),se=sd(spread)/sqrt(length(spread)))
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.

estimate <- abs(res$ave[1]-res$ave[2])
estimate



# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.

se_hat <- sqrt((res$se[2])^2+(res$se[1])^2)
se_hat


# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.

q <- 0.95
Z <- qnorm(1-(1-q)/2)

Z
ci <- c(estimate-Z*se_hat,estimate+Z*se_hat)
ci



# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value

2 * (1 - pnorm(estimate / se_hat))
