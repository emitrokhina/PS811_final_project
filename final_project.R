#Final project 811
#Evgeniya Mitrokhina
#Dare: 23.11.2020

library(dplyr)
library(ggplot2)

dataset <- read.csv("final_project_data.csv")


######TV frequency
table(dataset$tv)
dataset$tv_rec <- 99
dataset$tv_rec[dataset$tv == 6] <- 1 #don't watch (Не смотрю)
dataset$tv_rec[dataset$tv == 5] <- 2 #episodically, no less than once per half a year (Смотрю эпизодически, но не менее 1 раза в полгода)
dataset$tv_rec[dataset$tv == 4] <- 3 #several times per month (Смотрю несколько раз в месяц)
dataset$tv_rec[dataset$tv == 3] <- 4 #several times per week (Смотрю несколько раз в неделю)
dataset$tv_rec[dataset$tv == 2] <- 5 #every day less than 4hrs per day (Смотрю ежедневно, менее 4 часов в день)
dataset$tv_rec[dataset$tv == 1] <- 6 #more than 4hrs per day (Смотрю более 4 часов ежедневно)

table(dataset$tv_rec)

#detach(package:plyr)

#GRAPHICS
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


#Logit regression
model1<- glm(president_approval ~ as.factor(tv) + age + as.factor(education) + as.factor(internet) +
               income + protest +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model1)

stargazer(model1, model2, type = 'text',
          title = "Political actors approval assosiated with watching Tv frequency",
          dep.var.labels = c("President approval", "Governor approval"),
          header = FALSE, out = "file.txt"
)



model2<- glm(governor_approval ~ as.factor(tv) + age + as.factor(education) + as.factor(internet) +
               income + protest +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model2)
