library(survival)

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
              ("Income" = income) +
              ("Protest participation" = protest) +
              ("Voting last elections" = vote_recoded)  ~
              Mean + SD + Min + Max,
            data = dataset,
            output = 'summary_controls.docx')


dataset <- read.csv("presentation_data.csv")

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
