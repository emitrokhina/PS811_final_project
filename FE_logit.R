library(survival)

fe_1 <- clogit(president_approval ~ as.factor(tv_rec) + age + as.factor(education) + as.factor(internet_rec) +
               income + protest + as.factor(vote_recoded) + strata(region, wave), data = dataset)
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
