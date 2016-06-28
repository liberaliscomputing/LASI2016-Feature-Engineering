#LASI 2016 Feature Engineering Workshop
#DAY 2 Lab 2 - June 28, 2016
#dplyr with log data
#Meen Chul Kim

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

#Read in and check Log Data
logdata2 <- read_csv('logdata2.csv')

#Identify the total number of times a resource has been accessed
totals <- logdata2 %>% 
  group_by(ID, EVENT) %>% 
  summarise(count=n())

totals <- logdata2 %>% 
  group_by(EVENT) %>% 
  summarise(count=n())

#Create category variable for resource types
logdata2 <- logdata2 %>% 
  mutate(CEVENT=ifelse(grepl('Lab', EVENT, ignore.case = FALSE), 'LAB', EVENT),
         CEVENT=ifelse(grepl('Syllabus', EVENT, ignore.case = FALSE), 'SYLL', CEVENT),
         CEVENT=ifelse(grepl('Exam', EVENT, ignore.case = FALSE), 'EXAM', CEVENT))

#Create a new data frame with resource name and category
loggen <- select(logdata2, ID, EVENT, CEVENT)

#Identify unique resource accesses
loggen <- unique(loggen)

#Sum the total of unique resources accessed within a category
loggen <- loggen %>% 
  group_by(ID, CEVENT) %>%
  summarise(SUM=n())

#Reshape data frame to wide form
loggen <- dcast(loggen, ID ~ CEVENT, value.var = 'SUM')

#Create features that divide resources accessed by available per category
loggen <- loggen %>% 
  mutate(LAB=ifelse(is.na(LAB), 0, LAB),
         SYLL=ifelse(is.na(SYLL), 0, SYLL),
         EXAM=ifelse(is.na(EXAM), 0, EXAM),
         LAB=100*(LAB/6),
         SYLL=100*(SYLL/1),
         EXAM=100*(EXAM/4))
#loggen$LAB <- as.integer(loggen$LAB)
  
#Save loggen data frame
write_csv(loggen, 'loggen.csv')


