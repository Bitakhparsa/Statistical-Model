# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
Z <- qnorm(1-(1-0.95)/2)
cis <- polls %>% mutate(X_hat =(spread+1)/2,  se = 2*sqrt(X_hat*(1-X_hat)/samplesize)  , lower=spread - Z*se, upper=spread + Z*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)


#_____________________________
#ex2 - Compare to Actual Results
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
hit <- ci_data %>% mutate(ac_sp_f= (actual_spread >= lower & actual_spread <= upper ))
p_hits <- hit %>% summarise(mean(ac_sp_f))


#_____________________________
#ex3 - Stratify by Pollster and Grade

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.


p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade) %>%
  arrange(desc(proportion_hits))
p_hits