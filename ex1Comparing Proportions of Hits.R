library(tidyverse)

polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

Z <- qnorm(1-(1-0.95)/2)
cis <- polls %>% mutate(X_hat =(spread+1)/2,  se = 2*sqrt(X_hat*(1-X_hat)/samplesize)  , lower=spread - Z*se, upper=spread + Z*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

errors <- ci_data %>% mutate(errors= spread-actual_spread , hit=(sign(spread)==sign(actual_spread)) )

head(errors)
#______________
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c("A-","C-")) %>% group_by(grade,hit) %>% summarise(n=n()) %>% spread(grade, n)




# Print the proportion of hits for grade A- polls to the console
totals[2,3]/(totals[1,3]+totals[2,3])

# Print the proportion of hits for grade C- polls to the console
totals[2,2]/(totals[2,2]+totals[1,2])



# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])

totals[[2,2]]/sum(totals[[2]])
#______________
#ex2Chi-squared Test
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test()
chisq_test

# Print the p-value of the chi-squared test to the console
chisq_test$p.value



# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C
