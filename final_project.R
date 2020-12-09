#Final project 811
#Evgeniya Mitrokhina
#Dare: 23.11.2020

library(dplyr)
library(ggplot2)
library("magrittr")
library("tidyverse")
library("haven")
library("here")
library("stargazer")
library("modelsummary")
library("sjlabelled")

dataset <- read.csv("final_project_data.csv")

#Recoding variables

#President approval
table(dataset$president_approval)

#Governor approval
table(dataset$governor_approval)

#Government approval
table(dataset$government_approval)

dataset$government_approval[dataset$government_approval == 1] <- 0
dataset$government_approval[dataset$government_approval == 2] <- 1
dataset$government_approval[dataset$government_approval == 99] <- NA

table(dataset$government_approval)



#Government approval
table(dataset$mayor_approval)

dataset$mayor_approval[dataset$mayor_approval == 1] <- 0
dataset$mayor_approval[dataset$mayor_approval == 2] <- 1
dataset$mayor_approval[dataset$mayor_approval == 99] <- NA

table(dataset$mayor_approval)

######TV frequency (recoding)
table(dataset$tv)
dataset$tv_rec <- 99
dataset$tv_rec[dataset$tv == 6] <- 1 #don't watch (Не смотрю)
dataset$tv_rec[dataset$tv == 5] <- 2 #episodically, no less than once per half a year (Смотрю эпизодически, но не менее 1 раза в полгода)
dataset$tv_rec[dataset$tv == 4] <- 3 #several times per month (Смотрю несколько раз в месяц)
dataset$tv_rec[dataset$tv == 3] <- 4 #several times per week (Смотрю несколько раз в неделю)
dataset$tv_rec[dataset$tv == 2] <- 5 #every day less than 4hrs per day (Смотрю ежедневно, менее 4 часов в день)
dataset$tv_rec[dataset$tv == 1] <- 6 #more than 4hrs per day (Смотрю более 4 часов ежедневно)

table(dataset$tv_rec)

#### Internet frequency (recoding)
table(dataset$internet)
datdaset$internet_rec <- 99
dataset$internet_rec[dataset$internet == 96] <- 1 #don't use (Не пользуюсь)
dataset$internet_rec[dataset$internet == 5] <- 2 #episodically, no less than once per half a year (Смотрю эпизодически, но не менее 1 раза в полгода)
dataset$internet_rec[dataset$internet == 4] <- 3 #several times per month (Смотрю несколько раз в месяц)
dataset$internet_rec[dataset$internet == 3] <- 4 #several times per week (Смотрю несколько раз в неделю)
dataset$internet_rec[dataset$internet == 2] <- 5 #every day less than 4hrs per day (Смотрю ежедневно, менее 4 часов в день)
dataset$internet_rec[dataset$internet == 1] <- 6 #more than 4hrs per day (Смотрю более 4 часов ежедневно)


write.csv(dataset,"presentation_data.csv", row.names = FALSE)

#Summary statistics


datasummary(('President approval' = president_approval) +
              ('Governor approval' = governor_approval) +
              ('Government approval' = government_approval) +
              ('Mayor approval' = mayor_approval) +
              ('TV watching frequncy' = tv_rec) +
              ('Internet frequncy' = internet_rec) ~
              Mean + SD + Min + Max,
            data = dataset,
            output = 'markdown')

#Plots
dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(as.integer(president_approval))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "darkred") +
  theme_minimal() +
  labs(title = "Average approval of the President (monthly)", x = "months", y = "Share of supporters")

dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(governor_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Governor (monthly)", x = "months", y = "Share of supporters")

dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(government_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Government (monthly)", x = "months", y = "Share of supporters")

dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(mayor_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Mayor (monthly)", x = "months", y = "Share of supporters")
#Logit regression

#President 

model1<- glm(president_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
               income + protest +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model1)

stargazer(model1, model2, type = 'text',
          title = "Political actors approval assosiated with watching Tv frequency",
          dep.var.labels = c("President approval", "Governor approval"),
          header = FALSE, out = "file.txt"
)

#Governor

model2<- glm(governor_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
               income + protest +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model2)
