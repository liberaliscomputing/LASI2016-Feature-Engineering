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
barplot(plot$mean, main='Plot of Success Rates',
        xlab='Section')
