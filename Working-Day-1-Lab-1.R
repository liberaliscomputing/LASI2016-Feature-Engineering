#LASI 2016 Feature Engineering Workshop
#DAY 1 Lab 1 - June 27, 2016
#Introduction to R and dplyr
#Meen Chul Kim

#Install add-on packages
install.packages('dplyr')
install.packages('lubridate')
install.packages('tidyr')
install.packages('reshape2')
install.packages('ggplot2')
install.packages('readr')
install.packages('car')

#Turn on the libraries
library('dplyr')
library('lubridate')
library('tidyr')
library('reshape2')
library('ggplot2')
library('readr')
library('car')

#Set working directory
setwd('/Users/openhtml/Workspace/LASI2016-Feature-Engineering/data')

#Read in Grade Data
gradedata <- read_csv('GradeData.csv')

#Examine column (varaible) names
colnames(gradedata)

#Examine column (varaible) names
head(gradedata)

#Delete extraneous 6th column 
gradedata[, 6] <- NULL
head(gradedata)

#Count the total number of students
count <- gradedata %>% 
  summarise(count=n())
#write_csv(count, 'count.csv')

#Count the number of males and females
gradedata %>%
  group_by(SEX) %>%
  summarise(count=n())

#Count the number of students who score a particular grade
gradedata %>%
  group_by(COURSE_GRADE) %>%
  summarise(count=n())

#Count the number of students who score a particular grade
sexbysection <- gradedata %>%
  group_by(SECTION, SEX) %>%
  summarise(count=n())
#write_csv(sexbysection, 'sexbysection.csv')

#Calculate proportion of males and females
gradedata %>%
  group_by(SEX) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count))

#Recode sex into female indicator
gradedata$FEMALE <- recode(gradedata$SEX,
                           "c('F')=1; c('M')=0")
head(gradedata)

#Create bar chart for course grade
coursegrade <- table(gradedata$COURSE_GRADE)
barplot(coursegrade, main='STEM Course Grade', xlab="Grade")

#Describe semester GPA
gradedata %>% 
  summarise(mean=mean(SEM_GPA, na.rm=TRUE),
            sd=sd(SEM_GPA, na.rm=TRUE),
            median=median(SEM_GPA, na.rm=TRUE))

#Describe semester GPA by sex
gradedata %>% 
  group_by(SEX) %>%
  summarise(mean=mean(SEM_GPA, na.rm=TRUE),
            sd=sd(SEM_GPA, na.rm=TRUE),
            median=median(SEM_GPA, na.rm=TRUE))

#Create histogram for semster GPA
hist(gradedata$SEM_GPA, main='Histogram of Semester GPA',
     xlim=c(0,4))

