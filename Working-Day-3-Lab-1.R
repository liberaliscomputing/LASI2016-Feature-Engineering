#LASI 2016 Feature Engineering Workshop
#DAY 3 Lab 1 - June 29, 2016
#Modeling exmaple
#Meen Chul Kim

#Turn on the libraries
library('lme4')

#Set working directory
setwd('/Users/openhtml/Workspace/LASI2016-Feature-Engineering/data')

#Read in data
gradedata3<- read_csv('gradedata3.csv')
loggen <- read_csv('loggen.csv')
head(gradedata3)

#Merge data
merged1 <- merge(gradedata3, loggen, by.x='ID', by.y='ID')
head(merged1)

#Run our model
model3 <- glmer(GRADE ~ gm1_gpa + gm1_female + EXAM + 
                  LAB + SYLL + (1|SECTION), merged1, binomial(link = 'logit'))
summary(model3)

#Convert to odds ratio e^log(P/(1-P)) == P/(1-P) == odds
model3or <- exp(fixef(model3))
model3or

#Convert to predicted probability == odds/(1+odds)
model3pr <- model3or/(1+model3or)
model3pr

#Feature #2
logdata2 <- read_csv('logdata2.csv')
gradedata3 <- read_csv('gradedata3.csv')
head(logdata2)

logdata3 <- merge(logdata2, gradedata3, by.x='ID', by.y='ID')

logdata3 <- logdata3 %>% 
  group_by(ID, SECTION, EVENT) %>%
  summarise(FSESS=min(SESSION))
head(logdata3)

#Course level log data
cllogdata3 <- logdata3 %>%
  group_by(SECTION, EVENT) %>%
  summarise(P25=quantile(FSESS, probs = .25),
            P50=quantile(FSESS, probs = .50),
            P75=quantile(FSESS, probs = .75))
head(cllogdata3)

logdata3 <- merge(logdata3, cllogdata3, by=c('SECTION', 'EVENT'))

logdata3 <- logdata3 %>%
  mutate(EARLY=ifelse(FSESS<=P25, 1, 0),
         LATE=ifelse(FSESS>=P75, 1, 0))

head(logdata3)

logdata3 <- logdata3 %>%
  group_by(ID) %>%
  summarise(SEARLY=sum(EARLY)/11,
            SLATE=sum(LATE)/11)

#Merge data
merged2 <- merge(merged1, logdata3, by.x='ID', by.y='ID')
head(merged2)

#Run model 2 with grand mean centered predictors
model4 <- glmer(GRADE ~ gm1_gpa + gm1_female + SEARLY + SLATE +
                  (1|SECTION), merged2, binomial(link = 'logit'))
summary(model4)

#Convert to odds ratio e^log(P/(1-P)) == P/(1-P) == odds
model4or <- exp(fixef(model4))
model4or

#Convert to predicted probability == odds/(1+odds)
model4pr <- model4or/(1+model4or)
model4pr

write_csv(merged2, 'merged.csv')
