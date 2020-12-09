library(dplyr)
library(ggplot2)

dataset <- read.csv("final_project_data.csv")
## Descriptive statistics

```{r, warning = FALSE, figures-side, fig.show="hold", out.width="50%"}
par(mar = c(4, 4, .1, .1))

approval %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(government_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Government", x = "months", y = "Share of supporters")

approval %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(na.omit(as.integer(mayor_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Mayor", x = "months", y = "Share of supporters")
```


#GRAPHICS
#detach(package:plyr) - is not compatible with dplyr

#President
dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_pres = mean(as.integer(president_approval))) %>%
  ggplot(aes(x = wave, y = mean_pres))+
  geom_line() + #add abb line - mean
  theme_minimal() +
  ggtitle("")

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
  mutate(mean_pres = mean(na.omit(as.integer(governor_approval)))) %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y =  mean_pres), color = "steelblue") +
  theme_minimal() +
  labs(title = "Average approval of the Governor (monthly)", x = "months", y = "Share of supporters")


  
ggplot(economics, aes(x=date)) + 
  geom_line(aes(y = psavert), color = "darkred") + 
  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash") 
  
  
  
cdplot(as.factor(president_approval) ~ wave, data = dataset)

ggplot(dataset, 
       aes(x = region, 
           fill = as.factor(president_approval))) + 
  geom_bar(position = "dodge")


#Governor
dataset %>% 
  group_by(as.factor(wave)) %>%
  mutate(mean_governor = mean(as.integer(governor_approval))) %>%
  ggplot(aes(x = wave, y = mean_pres))+
  geom_line() #add abb line - mean

cdplot(as.factor(governor_approval) ~ wave, data = dataset)


ggplot(dataset, 
       aes(x = region, 
           fill = as.factor(governor_approval))) + 
  geom_bar(position = "dodge")

