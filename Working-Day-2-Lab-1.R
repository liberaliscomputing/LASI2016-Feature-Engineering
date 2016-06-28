#LASI 2016 Feature Engineering Workshop
#DAY 2 Lab 1 - June 28, 2016
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
logdata <- read_csv('LogData.csv')
head(logdata)

#Create new data and time variable using lubridate
logdata$NEW_DATE_TIME <- mdy_hm(logdata$DATE_TIME)
head(logdata)

#Parse out day of year to create measure of "session"
logdata$SESSION <- yday(logdata$NEW_DATE_TIME)

#Count the number of events and export to .csv file
events <- logdata %>%
  group_by(EVENT) %>%
  summarise(count=n())
write_csv(events, 'event.csv')

#Separate out event name from string
logdata <- separate(logdata, EVENT, 
                    into=paste('V', 1:4, sep=''), sep='/')
head(logdata)

#Remove extraneous variables from parsing
logdata <- logdata %>%
    select(-V1, -V2, -V3)
head(logdata)

#Remove other extraneous variables and rename
logdata <- logdata %>%
  select(ID, NEW_DATE_TIME, SESSION, EVENT=V4)
head(logdata)

#Identify and delete duplicate events from same date & time
logdata <- logdata %>%
  count(ID, NEW_DATE_TIME, SESSION, EVENT) %>%
  select(-n)
head(logdata)

#Create overal order of event variable
logdata <- logdata %>%
  group_by(ID) %>%
  mutate(ORDER = rank(NEW_DATE_TIME, ties.method = 'random'))
head(logdata)

#Rearrange data in order of ID and order of event
logdata <- logdata %>%
  arrange(ID, ORDER)

#Save logdata
write_csv(logdata, 'logdata2.csv')
