library(dplyr)
library(ggplot2)

dataset <- read.csv("final_project_data.csv")


#GRAPHICS
#detach(package:plyr) - is not compatible with dplyr

#President
dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(as.integer(president_approval))) %>%
  ggplot(aes(x = wave, y = mean_pres))+
  geom_line() #add abb line - mean

cdplot(as.factor(president_approval) ~ wave, data = dataset)

ggplot(dataset, 
       aes(x = region, 
           fill = as.factor(president_approval))) + 
  geom_bar(position = "dodge")


#Governor
dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(as.integer(governor_approval))) %>%
  ggplot(aes(x = wave, y = mean_pres))+
  geom_line() #add abb line - mean

cdplot(as.factor(governor_approval) ~ wave, data = dataset)


ggplot(dataset, 
       aes(x = region, 
           fill = as.factor(governor_approval))) + 
  geom_bar(position = "dodge")

