#LASI 2016 Feature Engineering Workshop
#DAY 2 Lab 3 - June 28, 2016
#Modeling exmaple
#Meen Chul Kim

#install modeling package
install.apckages('lme4')

#Turn on the libraries
library('dplyr')
library('lubridate')
library('tidyr')
library('reshape2')
library('ggplot2')
library('readr')
library('car')
library('lme4')

#Set working directory
setwd('/Users/openhtml/Workspace/LASI2016-Feature-Engineering/data')

#Read in gradedata2.csv
gradedata2 <- read_csv('gradedata2.csv')
head(gradedata2)

#Recode GRADE variables into Succeed==1; Not Succeed==0
gradedata2$GRADE <- recode(gradedata2$COURSE_GRADE,
                           "c('A','A-','A+','B','B-','B+')=1;
                           c('C','C-','C+','D','D-','D+','F','W')=0;
                           c('NA')=NA") ##Need to re-check based on old sheet

#Descriptive analyses: Is a student's smester wide performance correlated with STEM course grade?
gradedata2 %>% 
  group_by(GRADE) %>%
  summarise(mean=mean(SEM_GPA, na.rm=TRUE),
            sd=sd(SEM_GPA, na.rm=TRUE),
            median=median(SEM_GPA, na.rm=TRUE),
            iqr=IQR(SEM_GPA, na.rm=TRUE),
            min=min(SEM_GPA, na.rm=TRUE),
            max=max(SEM_GPA, na.rm=TRUE))

#Drop the students who had strange GPAs
gradedata2 <- gradedata2 %>%
  filter(SEM_GPA<=4)

#Visually explore variation in course success rates
plot <- gradedata2 %>% 
  group_by(SECTION) %>%
  summarise(mean=mean(GRADE, na.rm=TRUE))
plot <- plot %>% select(mean)
barplot(plot$mean, main='Plot of Success Rates', xlab='Section')

#What is the personality of passing, i.e., our baseline?
gradedata2 %>% summarise(mean=mean(GRADE, na.rm=TRUE))

#Run our null model random intercepts
modelnull <- glmer(GRADE ~ (1|SECTION), gradedata2, binomial(link = 'logit'))
summary(modelnull)

#Convert to odds ratio e^log(P/(1-P)) == P/(1-P) == odds
modelnullor <- exp(fixef(modelnull))
modelnullor

#Convert to predicted probability == odds/(1+odds)
modelnullpr <- modelnullor/(1+modelnullor)
modelnullpr

#Run model 1: GPA predictor with random intercepts
model1 <- glmer(GRADE ~ SEM_GPA + (1|SECTION), gradedata2, binomial(link = 'logit'))
summary(model1)

#Convert to odds ratio e^log(P/(1-P)) == P/(1-P) == odds
model1or <- exp(fixef(model1))
model1or

#Convert to predicted probability == odds/(1+odds)
model1pr <- model1or/(1+model1or)
model1pr

#Grand mean centering for GPA and FEMALE
#Identify grand means
gm2 <- gradedata2 %>% summarise(gm2_gpa=mean(SEM_GPA, na.rm=TRUE),
                                gm2_female=mean(FEMALE, na.rm=TRUE))

#Subtract grand means
gradedata2 <- gradedata2 %>% mutate(gm1_gpa = SEM_GPA - gm2$gm2_gpa,
                                    gm1_female = FEMALE - gm2$gm2_female)

#Run model 2 with grand mean centered predictors
model2 <- glmer(GRADE ~ gm1_gpa + gm1_female + (1|SECTION), gradedata2, binomial(link = 'logit'))
summary(model2)

#Convert to odds ratio e^log(P/(1-P)) == P/(1-P) == odds
model2or <- exp(fixef(model2))
model2or

#Convert to predicted probability == odds/(1+odds)
model2pr <- model2or/(1+model2or)
model2pr

#Save loggen
write_csv(gradedata2, 'gradedata3.csv')
