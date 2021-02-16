
# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

Ex <- p
Sex <- sqrt(N)*sqrt(p*(1-p))
x_bar <- p
se_bar <- sqrt(p*(1-p)/N)



ex_spread <- 2*x_bar-1
ex_spread 
Se_spread<- 2*se_bar
Se_spread
#___________________________________________
head(brexit_polls)
brexit_polls <- brexit_polls %>%   mutate(x_hat =(spread+1)/2 ) 
  
  
 brexit_polls %>% summarise(ave_sp=mean(spread ),sd_spread=sd(spread),ave_x_hat=mean(x_hat), sd_hat=sd(x_hat) )
q <- 0.95
z <- qnorm(1-(1-q)/2)  
brexit_polls <- brexit_polls %>% mutate(se_hat=sqrt(x_hat*(1-x_hat)/samplesize),lower=x_hat-z*se_hat, upper=x_hat+z*se_hat , hit=(lower<p & upper>p)) 

brexit_polls %>% summarise(mean(hit))
head(brexit_polls)

#___________________________________________
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
d <- -0.038

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")%>%
  mutate(se_hat =sqrt(x_hat*(1-x_hat)/samplesize))%>%
  mutate(se_spread =2*se_hat) %>%
  mutate(lower_sp=2*x_hat-2*z*se_hat-1, upper_sp=2*x_hat+2*z*se_hat-1) %>%
  mutate(hit=(lower_sp<d & upper_sp>d))

       
n <- nrow(june_polls)
n
#_____________________
june_polls_zero <- june_polls %>% filter(lower_sp *upper_sp<0   )

n_zero<-nrow(june_polls_zero)

n_zero/n
    #___
june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         ci_lower_spread = spread - qnorm(0.975)*se_spread,
         ci_upper_spread = spread + qnorm(0.975)*se_spread)
mean(june_polls$ci_lower_spread < 0 & june_polls$ci_upper_spread > 0)

#_____________________
june_polls_remain <- june_polls %>% filter(lower_sp > 0 & upper_sp> 0   )

n_remain<- nrow(june_polls_remain)

n_remain/n
          #___

mean(june_polls$ci_lower_spread > 0)

#_____________________
june_polls_d <- june_polls %>% filter(lower_sp<=d & upper_sp>=d)
n_d <- nrow(june_polls_d)
n_d/n
          #___

june_polls <- june_polls %>%
  mutate(hit = (2*p-1 > ci_lower_spread) & (2*p-1 < ci_upper_spread))
mean(june_polls$hit)
#_____________________

head(june_polls)

june_polls1 <- june_polls %>% group_by(pollster ) %>% summarise(n=n(),hit , mean_hit=mean(hit) ) %>% arrange(mean_hit)
view(june_polls1)
#_____________________

june_polls %>% 
ggplot(aes(x=poll_type  ,y=spread ))+
  geom_boxplot()+
  geom_point()
#_____________________




z

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>% mutate(se=2*sqrt(p_hat*(1-p_hat)/N),lower=spread-z*se,upper=spread+z*se)

combined_by_type

#_____________________

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)






brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value

two_by_two <- table( brexit_hit$hit, brexit_hit$poll_type)

# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05


#_____________________

#Odds ratio of online and telephone poll hit rate
odds_online <- (two_by_two[[2,1]] / two_by_two[[1,1]] )
odds_online
odds_phone<- (two_by_two[[2,2]] / two_by_two[[1,2]])
odds_phone
odds_online/odds_phone
#____

# from previous question
brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)

# convert to data frame
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds


phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]

phone_odds <- phone_true/phone_false
phone_odds

#_____________________
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

#_____________________
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% 
  ggplot(aes(enddate, proportion, color = vote )) +
  geom_smooth(method = "loess", span = 0.3) 
