library(survival)
library(plyr)
library(tidyr)
library("modelsummary")
library(dplyr)
library(ggplot2)
library(stargazer)

dataset <- read.csv("presentation_data.csv")

dataset$dohod[dataset$dohod == 99] <- NA
dataset$protest[dataset$protest == 99] <- NA



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
            output = 'summary.docx')

datasummary(("Age "= age) +
              ("Education" = education) +
              ("Income" = dohod) +
              ("Protest participation" = protest) +
              ("Voting last elections" = vote_recoded)  ~
              Mean + SD + Min + Max,
            data = dataset,
            output = 'summary_controls.docx')


#Graphics
dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(as.integer(president_approval))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "darkred") +
  theme_minimal() +
  labs(title = "Average approval of the President", x = "months", y = "Share of supporters") +
  ylim(0.30, 1)

dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(government_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average Government approval", x = "months", y = "Share of supporters")+
  ylim(0.30, 1)

dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(governor_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Governor", x = "months", y = "Share of supporters")+
  ylim(0.30, 1)

dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(mayor_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "darkred") +
  theme_minimal() +
  labs(title = "Average approval of the Mayor", x = "months", y = "Share of supporters")+
  ylim(0.30, 1)

#Analysis


fe_1 <- clogit(president_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
                 dohod + protest + as.factor(vote_recoded) + strata(region, wave), data = dataset)
summary(fe_1)


fe_2 <- clogit(governor_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
                 income + protest + as.factor(vote_recoded) + strata(region, wave), data = dataset)
summary(fe_2)

fe_3 <- clogit(government_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
                 income + protest + as.factor(vote_recoded) + strata(region, wave), data = dataset)
summary(fe_3)

fe_4 <- clogit(mayor_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
                 income + protest + as.factor(vote_recoded) + strata(region, wave), data = dataset)
summary(fe_4)

stargazer(fe_1, fe_2, fe_3, fe_4, type = 'text',
          omit = c("region", "wave", "education"),
          dep.var.labels = c("President approval", "Governor approval", "Government approval", "Mayor approval"),
          covariate.labels = c("Watch TV very rarely", "Watch TV several times per month",
                               "Watch TV several times per week", "Watch TV every day less than 4hrs",
                               "Watch TV every day more than 4hrs", "Age", "Use the Internet very rarely",
                               "Use the Internet several times per month", "Use the Internet several times per week",
                               "Use the Internet every day less than 4hrs", "Use the Internet every day more than 4hrs",
                               "Income", "Protest"),
          omit = c("vote_recoded", "protest", "education")
          header = FALSE, out = "results_file.txt")


