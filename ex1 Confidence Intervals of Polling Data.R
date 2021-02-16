# Load the libraries and data
library(dplyr)
library(dslabs)
library(ggplot2)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
Z <- qnorm(1-(1-0.95)/2)
cis <- polls %>% mutate(X_hat =(spread+1)/2,  se = 2*sqrt(X_hat*(1-X_hat)/samplesize)  , lower=spread - Z*se, upper=spread + Z*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
head(cis)

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
p_hits <- ci_data %>% mutate(hit= (actual_spread >= lower & actual_spread <= upper )) %>% 
  group_by(pollster)%>%  
  filter(n()>=5) %>% 
  summarise(proportion_hits=mean(hit), n=n(), garde=grade[1])   

p_hits %>% arrange(desc(proportion_hits))


p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))
p_hits
#_____________________________
#ex4 - Stratify by State



# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.


p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))
p_hits
#_____________________________
#ex5 - Plotting Prediction Results


# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state , proportion_hits))+
  geom_bar(stat = "identity")+
  coord_flip()
#_____________________________
#ex6 - Predicting the Winner
cis <- ci_data
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(errors= spread-actual_spread , hit=(sign(spread)==sign(actual_spread)) )

# Examine the last 6 rows of `errors`
tail(errors)
#_____________________________
#ex7 - Plotting Prediction Results

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls

p_hits <- errors %>% group_by(state) %>% filter(n()>=5) %>% summarise(proportion_hits=mean(hit), n=n()) 


# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(x=state,y=proportion_hits))+
  geom_bar(stat = "identity")+
  coord_flip()

#_____________________________
#ex8-Plotting the Errors

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)
# Calculate the median of the errors. Print this value to the console.
median(errors$error)
#_____________________________
#ex9-Plotting the Errors

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade ==c("A+", "A", "A-","B+")) %>% mutate(state = reorder(state, error)) %>%
  ggplot(aes(x=state,y=error))+
  geom_boxplot()+
  geom_point()
#_____________________________
#ex10-Filter Error Plot ???

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher

errors %>% filter(grade ==c("A+", "A", "A-","B+")) %>% group_by(state) %>% mutate(n=n()) %>% filter(n >= 1) %>% 
  ungroup %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(x=state,y=error))+
  geom_boxplot()+
  geom_point()
errors


