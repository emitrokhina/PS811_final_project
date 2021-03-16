#Final project 811
#Evgeniya Mitrokhina
#Date: 23.11.2020

library(dplyr)
library(ggplot2)
library("magrittr")
library("tidyverse")
library("haven")
library("here")
library("stargazer")
library("modelsummary")
library("sjlabelled")
library("estimatr")
library('plm')

dataset <- read.csv("final_project_data.csv")

names(dataset)


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

table(dataset$internet_rec)

#recoding voting
table(dataset$vote_choice)

dataset$vote_recoded <- NA

dataset$vote_recoded[dataset$vote_choice == 5] <- 1 #United RUssia
dataset$vote_recoded[dataset$vote_choice == 1] <- 2 #Parliament opposition
dataset$vote_recoded[dataset$vote_choice == 2] <- 2
dataset$vote_recoded[dataset$vote_choice == 3] <- 2
dataset$vote_recoded[dataset$vote_choice == 4] <- 3 #all the others including non voters
dataset$vote_recoded[dataset$vote_choice == 6] <- 3 
dataset$vote_recoded[dataset$vote_choice == 7] <- 3 
dataset$vote_recoded[dataset$vote_choice == 8] <- 3 
dataset$vote_recoded[dataset$vote_choice == 98] <- 3 
dataset$vote_recoded[dataset$vote_choice == 96] <- 3 
dataset$vote_recoded[dataset$vote_choice == 97] <- 3 
dataset$vote_recoded[dataset$vote_choice == 99] <- 3 


table(dataset$vote_recoded)
write.csv(dataset,"presentation_data.csv", row.names = FALSE)

#Summary statistics


datasummary(('President approval' = president_approval) +
              ('Governor approval' = governor_approval) +
              ('Government approval' = government_approval) +
              ('Mayor approval' = mayor_approval) +
              ('TV watching frequncy' = tv_rec) +
              ('Internet frequncy' = internet_rec)+
              ('Voting' = vote_recoded) ~
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


#Data analysis
library("foreign")

coplot(president_approval ~ wave|region, type = "l", data = dataset)

library(car)
scatterplot(president_approval ~ wave|region, boxplots = FALSE, smooth = TRUE, reg.line = FALSE, data = dataset)

library(gplots)
plotmeans(president_approval ~ region, main = "Heterogeineity across regions", ylab = "Regions", xlab = "President approval",
          data = dataset) 
#use this plot for all indicators

plotmeans(president_approval ~ wave, main = "Heterogeineity across years", data = dataset)


#Logit regression


#President 
model1<- glm(president_approval ~ factor(tv_rec) - 1 + age + as.factor(education) + as.factor(internet_rec) +
               income + protest + as.factor(vote_recoded) +
               factor(region) - 1 + factor(wave) - 1, data = dataset, family = "binomial")
summary(model1)

#FE model
model_1a <- plm(president_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
                  income + protest + as.factor(vote_recoded) + factor(wave) - 1, 
                    data = dataset,
                    index = c("region"), 
                    model = "within")

summary(model_1a)

fixef(model_1a)

pFtest(model1, model_1a)

#Governor

model2<- glm(governor_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
               income + protest + as.factor(vote_recoded) +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model2)


#Government

model3<- glm(government_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
               income + protest + as.factor(vote_recoded) +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model3)

#Mayor
model4<- glm(mayor_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
               income + protest + as.factor(vote_recoded) +
               as.factor(region) + as.factor(wave), data = dataset, family = "binomial")
summary(model4)



stargazer(model1, model2, model3, model4, type = 'text',
          title = "Political actors approval assosiated with watching Tv frequency",
          dep.var.labels = c("President approval", "Governor approval", "Government approval", "Mayor approval"),
          covariate.labels = c("Watch TV very rarely", "Watch TV several times per month",
                               "Watch TV several times per week", "Watch TV every day less than 4hrs",
                               "Watch TV every day more than 4hrs", "Age", "Use the Internet very rarely",
                               "Use the Internet several times per month", "Use the Internet several times per week",
                               "Use the Internet every day less than 4hrs", "Use the Internet every day more than 4hrs",
                               "Income", "Protest"),
          omit = c("region", "wave", "education"),
          header = FALSE, out = "file.txt")


